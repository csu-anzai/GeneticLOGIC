/* select.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Selection choices, Each takes a CHROMOSOME * pointing to array of all
 * pops, and two integers giving base index and size of target pop; returns
 * the selected CHROMOSOME.
 */


#include "pga.h"

extern double bias;
extern double drandom();
extern double *total_abs_fitness;
extern int numchromo;
extern int tournament_size;


extern double sqrt();
extern double fabs();


/************************************************************************/
/* This function performs rank based selection and is lifted from       */
/* Whitley's paper on GENITOR in icga 89. It selects from the specified */
/* subpopulation within 'pops'.                                         */
/************************************************************************/

CHROMOSOME rank_select(pops,base,range)
CHROMOSOME *pops;
int base,range;
{
  double value;

  if(bias<1.05)
     value=drandom()/bias;
  else {
     value=(bias-sqrt(bias*bias-4.0*(bias-1.0)*drandom()))/(2.0*(bias-1.0));
  }
  value=value*(double)range;

  return(pops[base+(int)value]);
}


/************************************************************************/
/* This function implements fitness proportionate selection. It treats  */
/* the fitness of the population as a "roulette wheel", rolls a ball on */
/* the wheel, and picks the chromosome within whose sector the ball     */
/* stops.                                                               */
/************************************************************************/

CHROMOSOME fitprop_select(pops,base,range)
CHROMOSOME *pops;
int base,range;
{
  int pop,loop;
  double where;

  pop = base/numchromo;  /* which pop we're working on */

  where=drandom()*total_abs_fitness[pop];  /* Random float in 0..total */

  for(loop=base; where>fabs(pops[loop].fitness); loop+=1)
    where-=fabs(pops[loop].fitness);

  return(pops[loop]);
}

/************************************************************************/
/* This fitness function performs a form of tournament selection as in  */
/* the classic marriage problem in dynamic programming. Select one at   */
/* uniform random, then continue for up to tournament_size tries making */
/* uniform random selections: choose the first that exceeds the fitness */
/* of that first-chosen. After tournament_size failures, choose first.  */
/************************************************************************/

#define ANY (base + (int)(drandom() * (double)range))

CHROMOSOME tm_select(pops,base,range)
CHROMOSOME *pops;
int base, range;
{
  int i, choice;
  double f;
  int reject = ANY;

  f = pops[reject].fitness;
  for(i=0;i<tournament_size;i++) {
    choice = ANY;
    if(pops[choice].fitness > f) return(pops[choice]);
  }
  return(pops[reject]);
}

/************************************************************************/
/* This fitness function performs straight tournament selection as in   */
/* Brindle, Alberta TR 81-2. Pick tournament_size at uniform random,    */
/* return fittest of those.                                             */
/************************************************************************/

CHROMOSOME tn_select(pops,base,range)
CHROMOSOME *pops;
int base, range;
{
  int i, choice;
  double f;
  int one = ANY;

  f = pops[one].fitness;
  for(i=0;i<tournament_size-1;i++) {
    choice = ANY;
    if(pops[choice].fitness > f) {
       one = choice;
       f = pops[one].fitness;
    }
  }
  return(pops[one]);
}

