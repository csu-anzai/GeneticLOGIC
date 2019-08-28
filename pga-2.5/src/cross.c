/* cross.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Crossover choices. All take two CHROMOSOMEs as arguments, return
 * one. Ok, this is a limitation of the current design, I know.
 */

#include "pga.h"

extern int geno_size;
extern int chromo_id;
extern int twins;

extern long random();
extern char *malloc();


/************************************************************************/
/* This performs one point crossover on 'parent1' and 'parent2'.        */
/* Child is new (even though there may be identical genotypes elsewhere */
/* in the system) so has refcount 0; set to 1 only upon insertion       */
/************************************************************************/

CHROMOPAIR one_pt_cross(parent1,parent2)
CHROMOSOME parent1,parent2;
{
  int chosen,i;
  CHROMOPAIR children;

  chosen=random()%geno_size;        /* Generate crossover point randomly. */


                                   /* Create the child. */
  children.child1.gdp = (GENODATA *)malloc(sizeof(GENODATA));
  children.child1.gdp->refcount = 0;
  children.child1.gdp->genotype = (char *)malloc(geno_size+1);
  children.child1.gdp->genotype[geno_size]='\0';
  children.child1.gdp->id = chromo_id++;
  children.child1.gdp->parent1 = parent1.gdp->id;
  children.child1.gdp->parent2 = parent2.gdp->id;

  if(twins) {
    children.child2.gdp = (GENODATA *)malloc(sizeof(GENODATA));
    children.child2.gdp->refcount = 0;
    children.child2.gdp->genotype = (char *)malloc(geno_size+1);
    children.child2.gdp->genotype[geno_size]='\0';
    children.child2.gdp->id = chromo_id++;
    children.child2.gdp->parent1 = parent1.gdp->id;
    children.child2.gdp->parent2 = parent2.gdp->id;
  }       
                                   /* Do the crossover */
  for(i=0; i<chosen; i++) {
    children.child1.gdp->genotype[i] = parent2.gdp->genotype[i];
    if(twins)
      children.child2.gdp->genotype[i] = parent1.gdp->genotype[i];
  }
  for(i=chosen; i<geno_size; i++) {
    children.child1.gdp->genotype[i] = parent1.gdp->genotype[i];
    if(twins)
      children.child2.gdp->genotype[i] = parent2.gdp->genotype[i];
  }
  return(children);
}

/************************************************************************/
/* This performs two point crossover on 'parent1' and 'parent2'.        */
/* Child is new (even though there may be identical genotypes elsewhere */
/* in the system) so has refcount 0; set to 1 only upon insertion       */
/************************************************************************/

CHROMOPAIR two_pt_cross(parent1,parent2)
CHROMOSOME parent1,parent2;
{
  int first,last,i,swap;
  CHROMOPAIR children;

  first=random()%geno_size;        /* Generate the two points randomly. */
  last=random()%geno_size;

  if (first>last) {                /* Make sure that they are in the */
    swap=first;                    /* right order.                   */
    first=last;
    last=swap;
  }

                                   /* Create the child. */
  children.child1.gdp = (GENODATA *)malloc(sizeof(GENODATA));
  children.child1.gdp->refcount = 0;
  children.child1.gdp->genotype = (char *)malloc(geno_size+1);
  children.child1.gdp->genotype[geno_size]='\0';
  children.child1.gdp->id = chromo_id++;
  children.child1.gdp->parent1 = parent1.gdp->id;
  children.child1.gdp->parent2 = parent2.gdp->id;

  if(twins) {
    children.child2.gdp = (GENODATA *)malloc(sizeof(GENODATA));
    children.child2.gdp->refcount = 0;
    children.child2.gdp->genotype = (char *)malloc(geno_size+1);
    children.child2.gdp->genotype[geno_size]='\0';
    children.child2.gdp->id = chromo_id++;
    children.child2.gdp->parent1 = parent1.gdp->id;
    children.child2.gdp->parent2 = parent2.gdp->id;
  }        
                                   /* Do the crossover */
  for(i=0; i<first; i++) {
    children.child1.gdp->genotype[i] = parent2.gdp->genotype[i];
    if(twins)
      children.child2.gdp->genotype[i] = parent1.gdp->genotype[i];
  }
  for(i=first; i<last; i++) {
    children.child1.gdp->genotype[i] = parent1.gdp->genotype[i];
    if(twins)
      children.child2.gdp->genotype[i] = parent2.gdp->genotype[i];
  }
  for(i=last; i<geno_size; i++) {
    children.child1.gdp->genotype[i] = parent2.gdp->genotype[i];
    if(twins)
      children.child2.gdp->genotype[i] = parent1.gdp->genotype[i];
  }

  return(children);
}

/************************************************************************/
/* This performs uniform crossover on 'parent1' and 'parent2'.          */
/* Uses rand_bit() to cut down on expensive random number generation.   */
/* Child is new (even though there may be identical genotypes elsewhere */
/* in the system) so has refcount 0; set to 1 only upon insertion       */
/************************************************************************/

int rand_bit_src;
int rand_bit_bits = 0;
int rand_bit()
{
  if(rand_bit_bits == 0) {
    rand_bit_bits = 16;
    rand_bit_src = random() % 131072; /* Get 16+1 more random bits */
  }
  rand_bit_src = rand_bit_src >> 1;   /* Lose 1 bit */
  rand_bit_bits--;                    /* Number left after this call */
  return(rand_bit_src & 1);
}


CHROMOPAIR uniform_cross(parent1,parent2)
CHROMOSOME parent1,parent2;
{
  int i;
  CHROMOPAIR children;

  children.child1.gdp = (GENODATA *)malloc(sizeof(GENODATA));
  children.child1.gdp->refcount = 0;
  children.child1.gdp->genotype = (char *)malloc(geno_size+1);
  children.child1.gdp->genotype[geno_size]='\0';
  children.child1.gdp->id = chromo_id++;
  children.child1.gdp->parent1 = parent1.gdp->id;
  children.child1.gdp->parent2 = parent2.gdp->id;

  if(twins) {
    children.child2.gdp = (GENODATA *)malloc(sizeof(GENODATA));
    children.child2.gdp->refcount = 0;
    children.child2.gdp->genotype = (char *)malloc(geno_size+1);
    children.child2.gdp->genotype[geno_size]='\0';
    children.child2.gdp->id = chromo_id++;
    children.child2.gdp->parent1 = parent1.gdp->id;
    children.child2.gdp->parent2 = parent2.gdp->id;
  }        
                                   /* Do the crossover */
  for(i=0; i<geno_size; i++)
    if(rand_bit()) {
      children.child1.gdp->genotype[i] = parent2.gdp->genotype[i];
      if(twins)
        children.child2.gdp->genotype[i] = parent1.gdp->genotype[i];
    } else {
      children.child1.gdp->genotype[i] = parent1.gdp->genotype[i];
      if(twins)
        children.child2.gdp->genotype[i] = parent2.gdp->genotype[i];
    }
  return(children);
}


