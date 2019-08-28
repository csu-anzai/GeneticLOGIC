/* reprod.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Reproduction choices. Each takes a CHROMOSOME * pointing to the array
 * of populations, and two ints giving base index and size of target pop
 * in that array. Returns nothing.
 */

#define TRUE 1
#define FALSE 0

#include <stdio.h>
#include "pga.h"

extern CHROMOSOME (*select)();
extern CHROMOPAIR (*crossover)();
extern double (*mrate)();
extern double (*evaluate)();
extern double drandom();
extern int compchromo();
extern CHROMOSOME mutate();
extern void insert();

extern double mute_rate;
extern double cross_rate;
extern double *total_abs_fitness;
extern int numchromo;
extern int twins;
extern int ss_xmax, ss_ymax, ss_n;


extern char *malloc();
extern double fabs();


/************************************************************************/
/* This function performs one-at-a-time reproduction on the specified   */
/* subpopulation. Adaptive mutation is used if activated.               */
/************************************************************************/

void one_reproduction(pops,base,range)
CHROMOSOME *pops;
int base,range;
{
  CHROMOSOME parent1,parent2,child;
  CHROMOPAIR children;

  parent1=select(pops,base,range);         /* Select two parents. */
  parent2=select(pops,base,range);

  children=crossover(parent1,parent2);        /* Cross them */
  child=mutate(children.child1,mrate(parent1,parent2)); /*  mutate (per bit) */
  child.fitness=evaluate(child.gdp->genotype);  /* Evaluate the child. */
  insert(pops,child,base,range);           /* Insert the child into */
                                           /* the population.       */
  if(twins) {
    child=mutate(children.child2,mrate(parent1,parent2)); /*  mutate */
    child.fitness=evaluate(child.gdp->genotype);  /* Evaluate the child. */
    insert(pops,child,base,range);         /* Insert the child into */
                                           /* the population.       */
  }
}

/************************************************************************/
/* This function performs generation based reproduction. A new          */
/* population is built by selecting members of the old one, possibly    */
/* crossing them with another, and possibly mutating them. This new     */
/* population is sorted and then used to replace the old population.    */
/************************************************************************/

void gen_reproduction(pops,base,range)
CHROMOSOME *pops;
int base,range;
{
  CHROMOSOME *newpop,parent1,parent2;
  CHROMOPAIR children;
  int loop,reeval,pop;

  pop = base/numchromo;  /* which population we're reconstructing */

  newpop=(CHROMOSOME *)malloc(sizeof(CHROMOSOME)*range);

  for(loop=0; loop<range;) {
    reeval=FALSE;
    parent1=select(pops,base,range);
    parent2=select(pops,base,range);
    if (drandom()<=cross_rate) {
      reeval=TRUE;
      children=crossover(parent1,parent2);
      children.child1=mutate(children.child1, mrate(parent1,parent2));
      if(twins)
        children.child2=mutate(children.child2, mrate(parent1,parent2));
    }
    else {
      children.child1=parent1;
      if(twins)
        children.child2=parent2;
    }

    /* By this point, child may have refcount 0 if product of crossover */
    /* and/or of mutation; or may have refcount > 0 otherwise.          */
    /* Increment child's refcount, so it is 1 if completely new, >1     */
    /* if in old population too.                                        */
    children.child1.gdp->refcount++;
    if(twins)
      children.child1.gdp->refcount++;

    if (reeval) {
      children.child1.fitness=evaluate(children.child1.gdp->genotype);
      if(twins)
        children.child2.fitness=evaluate(children.child2.gdp->genotype);
    }
    newpop[loop++]=children.child1;
    if(twins)
      newpop[loop++]=children.child2;
  }

  qsort((char *)newpop,range,sizeof(CHROMOSOME),compchromo);

  total_abs_fitness[pop] = 0.0;
  for(loop=0; loop<range; loop+=1) {
    /* If old member is not wanted in new population or elsewhere, its */
    /* refcount will be 1. Decrement ref counts in old pop, ditch the  */
    /* unwanted. */
    if(pops[base+loop].gdp->refcount == 1) {
       free(pops[base+loop].gdp->genotype);
       free(pops[base+loop].gdp);
    } else
       pops[base+loop].gdp->refcount--;

    pops[base+loop]=newpop[loop];
    total_abs_fitness[pop] += fabs(newpop[loop].fitness);
  }

  free(newpop);
}

/* This does `spatial selection' reproduction, distantly based on Wright's
 * shifting-balance model of evolution. The much more common panmictic model
 * is Fisher's in which the entire population competes in selection - eg
 * in roulette-wheel or rank-based selection, the fitness of all members
 * of the population is implicitly used in each selection. In Wright's
 * model, selection need only consider chromosomes in some (2-D) neighbourhood
 * each time. It seems to make sense to use a 2-D grid rather than 3-D or
 * higher; given sufficiently high dimension, every point would be an
 * immediate neighbour of every other so it would be a form of panmictic
 * evolution again.
 *
 * In this implementation, the chromosomes are regarded as being arranged
 * in a 2-D toroidal grid; so first the population size is first automatically
 * adjusted so that the grid can be rectangular and roughly square.
 *
 * Reproduction is generational and proceeds as follows. Each chromosome
 * is replaced by the result of crossover and mutation applied to two parents
 * chosen as the fittest met on a random walk of length ss_n starting from
 * the place where the child will be installed. If you use the -t (twins)
 * flag location (x,y) and location (x,y+1) (if it exists) get used to 
 * store the children.
 * 
 * This spatial selection reproduction utilises specific selection and
 * insertion algorithms, so you cannot use the -s option with it.
 * /


/*******************************************************************/
/* This chooses a location in the neighbourhood of given x,y using */
/* a random walk of length ss_n. Choose the fittest found, ties    */
/* are broken by choosing last.                                    */
/*******************************************************************/

#define LOCATION(x,y) (base+(y)*ss_xmax+(x))

/* The helps cut down on the number of drandom() calls. */
/* Returns random integer in range [0,7].               */
int rand_dir_bits = 0;
int rand_dir_store = 0;
int rand_dir()
{
  int direction;
  
  if(rand_dir_bits == 0) {
    rand_dir_store = (int) (drandom() * 16777216.0);
    rand_dir_bits = 24;
  }
  direction = rand_dir_store & 0x07;
  rand_dir_store = rand_dir_store >> 3;
  rand_dir_bits -= 3;
  return(direction);
}

CHROMOSOME ss_select(pops,base,x,y)
CHROMOSOME *pops;
int base,x,y;
{
  int i;
  CHROMOSOME chosen;
  double f;

  chosen = pops[LOCATION(x,y)];
  f = chosen.fitness;
  for(i=0;i<ss_n;i++) {
    switch(rand_dir()) {
    	case 0:
    	  y -= 1;
    	  break;
    	case 1:
    	  x += 1;
    	  y -= 1;
    	  break;
    	case 2:
    	  x +=1;
    	  break;
    	case 3:
    	  x +=1;
    	  y +=1;
    	  break;
    	case 4:
    	  y += 1;
    	  break;
    	case 5:
    	  x -=1;
    	  y +=1;
    	  break;
    	case 6:
    	  x -=1;
    	  break;
    	case 7:
    	  x -=1;
    	  y -=1;
    	  break;
    }

    /* Note that it is probably cheaper to do the next two lines */
    /* than to use conditional tests of whether x or y is out of */
    /* bounds.                                                   */
    x = (x+ss_xmax) % ss_xmax;
    y = (y+ss_ymax) % ss_ymax;
    
    if(pops[LOCATION(x,y)].fitness >= f) {
    	chosen = pops[LOCATION(x,y)];
    	f = chosen.fitness;
    }
  }
  return(chosen);
}

/*******************************************************************/
/* Spatial selection. Uses specific selection and insertion, so    */
/* you cannot set them independently. Generational version.        */
/*******************************************************************/

void ss_gen_reproduction(pops,base,range)
CHROMOSOME *pops;
int base, range;
{
  int x,y;
  CHROMOSOME parent1, parent2;
  CHROMOPAIR children;
  CHROMOSOME *newpop;

  /* Somewhere to keep the new population, so that all selection is */
  /* from the old population.                                       */
  newpop = (CHROMOSOME *)malloc(range*sizeof(CHROMOSOME));

  /* Scan the grid, doing selection and insertion */
  for(x=0;x<ss_xmax;x++) {
    for(y=0;y<ss_ymax; ) {
      parent1 = ss_select(pops,base,x,y);
      parent2 = ss_select(pops,base,x,y);
      children = crossover(parent1, parent2);
      children.child1 = mutate(children.child1, mrate(parent1,parent2));
      children.child1.fitness=evaluate(children.child1.gdp->genotype);
      children.child1.gdp->refcount++;
      newpop[y*ss_xmax+x] = children.child1;
      y++;
      /* If twins, put the second chromosome just below the first if */
      /* there is a place to put it. Don't wrap; top is already new. */
      if(twins && y < ss_ymax) {
        children.child2 = mutate(children.child2, mrate(parent1,parent2));
        children.child2.fitness=evaluate(children.child2.gdp->genotype);
        children.child2.gdp->refcount++;
        newpop[y*ss_xmax+x] = children.child2;
        y++;
      }
    }
  }

  /* Copy new that is better into old, freeing any unwanted old stuff. */
  for(x=0;x<numchromo;x++) {
    if(pops[base+x].fitness < newpop[x].fitness) {
      if(pops[base+x].gdp->refcount == 1) {
      	  free(pops[base+x].gdp->genotype);
    	  free(pops[base+x].gdp);
      } else
         pops[base+x].gdp->refcount--;
      pops[base+x] = newpop[x];
    } else {
      free(newpop[x].gdp->genotype);
      free(newpop[x].gdp);
    } 
  }
  
  free(newpop);
}

/*******************************************************************/
/* Spatial selection. Uses specific selection and insertion, so    */
/* you cannot set them independently. One (Genitor-ish) version.   */
/*******************************************************************/

void ss_one_reproduction(pops,base,range)
CHROMOSOME *pops;
int base, range;
{
  int x,y;
  CHROMOSOME parent1, parent2;
  CHROMOPAIR children;

  x = drandom() * (double)ss_xmax;
  y = drandom() * (double)ss_ymax;

  parent1 = ss_select(pops,base,x,y);
  parent2 = ss_select(pops,base,x,y);
  children = crossover(parent1, parent2);
  children.child1 = mutate(children.child1, mrate(parent1,parent2));
  children.child1.fitness=evaluate(children.child1.gdp->genotype);
  if(pops[LOCATION(x,y)].fitness < children.child1.fitness) {
    children.child1.gdp->refcount++;
    if(pops[LOCATION(x,y)].gdp->refcount == 1) {
      free(pops[LOCATION(x,y)].gdp->genotype);
      free(pops[LOCATION(x,y)].gdp);
    } else
      pops[LOCATION(x,y)].gdp->refcount--;
    pops[LOCATION(x,y)] = children.child1;
  } else {
    free(children.child1.gdp->genotype);
    free(children.child1.gdp);
  }

  /* If twins, put the second chromosome just below the first if */
  /* there is a place to put it. Don't wrap; top is already new. */
  y++;
  if(twins && y < ss_ymax) {
    if(pops[LOCATION(x,y)].fitness < children.child2.fitness) {
      children.child2.gdp->refcount++;
      if(pops[LOCATION(x,y)].gdp->refcount == 1) {
        free(pops[LOCATION(x,y)].gdp->genotype);
        free(pops[LOCATION(x,y)].gdp);
      } else
        pops[LOCATION(x,y)].gdp->refcount--;
      pops[LOCATION(x,y)] = children.child2;
    } else {
      free(children.child2.gdp->genotype);
      free(children.child2.gdp);
    }
  }
}
