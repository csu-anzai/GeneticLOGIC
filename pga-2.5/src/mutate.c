/* mutate.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Mutation. mutate() takes a CHROMOSOME as argument, returns one.
 */

#include "pga.h"

extern int geno_size;
extern int chromo_id;
extern double mute_rate;
extern double drandom();

extern char *malloc();
extern long random();
extern char *strncpy();

/************************************************************************/
/* This flips a random bit in 'original'.                               */
/* If given chromo has non-zero refcount, then work on copy instead.    */
/* Refcounts get adjusted upon insertion, later.                        */
/************************************************************************/

CHROMOSOME mutate(original, rate)
CHROMOSOME original;
double rate;
{
  int place;
  CHROMOSOME newchromo;

  if(original.gdp->refcount > 0) {
     newchromo.gdp = (GENODATA *)malloc(sizeof(GENODATA));
     newchromo.gdp->refcount = 0;
     newchromo.gdp->genotype = (char *)malloc(geno_size+1);
     newchromo.gdp->genotype[geno_size]='\0';
     newchromo.gdp->id = chromo_id++;
     newchromo.gdp->parent1 = original.gdp->parent1;
     newchromo.gdp->parent2 = original.gdp->parent2;
     strncpy(newchromo.gdp->genotype,original.gdp->genotype,geno_size);
  } else
     newchromo = original;

  for(place=0;place < geno_size; place++) {
     if(drandom() <= rate)
        if(newchromo.gdp->genotype[place] == '0')
          newchromo.gdp->genotype[place] = '1';
        else
          newchromo.gdp->genotype[place] = '0';
  }
  
  return(newchromo);
}

/************************************************************************/
/* Function mrate() points at one of the next two functions, depending  */
/* on whether adaptive is chosen or note. Adaptive mutation scales the  */
/* probability of mutating each bit by the degree of similarity of the  */
/* parents.                                                             */
/************************************************************************/

/************************************************************************/
/* This function returns the bit mutation rate appropriate to the given */
/* parents - a number in the range 0..mute_rate, depending on degree of */
/* similarity of parents.                                               */
/************************************************************************/

double adaptive(parent1,parent2)
CHROMOSOME parent1,parent2;
{
  int loop,count;
  double prob;

  count=0;
  for(loop=0; loop<geno_size; loop+=1) {
    if (parent1.gdp->genotype[loop] == parent2.gdp->genotype[loop]) count++;
  }
                        /* Proportion = observed / total. */
  prob=((double)count) / ((double)geno_size);

  return(prob*mute_rate);
}


/************************************************************************/
/* This function simply returns the fixed mutation rate.                */
/************************************************************************/

double nonadaptive(parent1,parent2)
CHROMOSOME parent1,parent2;
{
  return(mute_rate);
}


