
/* ga.c -- genetic algorithm routines

   Copyright (C) 1993 Joerg Heitkoetter

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef lint
static char *rcsid = "$Id: ga.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

grecord_t garec;		/* the GA goes here */
FILE *gfile;			/* GA parameters file */
char gfilename[MAXFILENAME]
= "scs.gen";			/* parameters file name */


/*
 *    initga -- initialize GA parameters
 */
void
initga (gfile, ga, pop)
     FILE *gfile;
     grecord_t *ga;
     class_p *pop;
{
  if (readln (gfile, "%lf", &(ga->proportionselect)))
    panic (E_FATAL, "initga", "readln: %s", "can't read `proportionselect'");
  if (readln (gfile, "%lf", &(ga->pmutation)))
    panic (E_FATAL, "initga", "readln: %s", "can't read `pmutation'");
  if (readln (gfile, "%lf", &(ga->pcrossover)))
    panic (E_FATAL, "initga", "readln: %s", "can't read `pcrossover'");
  if (readln (gfile, "%d", &(ga->crowdingfactor)))
    panic (E_FATAL, "initga", "readln: %s", "can't read `crowdingfactor'");
  if (readln (gfile, "%d", &(ga->crowdingsubpop)))
    panic (E_FATAL, "initga", "readln: %s", "can't read `crowdingsubpop'");

  /* number of mate pairs to select */
  ga->nselect = round (ga->proportionselect
		       * pop->nclassifier * D_HALF);
  ga->nmutation = 0;
  ga->ncrossover = 0;
}

/*
 *    initrepga -- write initial report
 */
void
initrepga (rep, ga)
     FILE *rep;
     grecord_t *ga;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Genetic Algorithm Parameters\n");
  fprintf (rep, "----------------------------\n");
  fprintf (rep, "Proportion to select/gen  = %8.4lf\n", ga->proportionselect);
  fprintf (rep, "Number to select          = %8d\n", ga->nselect);
  fprintf (rep, "Mutation probability      = %8.4lf\n", ga->pmutation);
  fprintf (rep, "Crossover probability     = %8.4lf\n", ga->pcrossover);
  fprintf (rep, "Crowding factor           = %8d\n", ga->crowdingfactor);
  fprintf (rep, "Crowding subpopulation    = %8d\n", ga->crowdingsubpop);
}

/*
 *    select -- select a single individual according to strength
 */
static int
select (pop)
     class_p *pop;
{
  double rand;
  double partsum = D_ZERO;
  int j = 0;

  rand = randomperc () * pop->sumstrength;
  do
    {
      partsum += pop->classifier[j++].strength;
    }
  while (partsum < rand && j < pop->nclassifier);

  return (j - 1);
}

/*
 *    mutation -- mutate a single `position' with specified probability
 */
static int
mutation (positionvalue, pmutation, nmutation)
     trit_t positionvalue;
     double pmutation;
     int *nmutation;
{
  int tempmutation;

  if (flip (pmutation))
    {
      tempmutation = (positionvalue + rnd (1, 2) + 1) % 3 - 1;
      (*nmutation)++;
    }
  else
    {
      tempmutation = positionvalue;
    }
  return (tempmutation);
}

/*
 *    bmutation -- mutate a single `bit' with specified probability
 */
static int
bmutation (positionvalue, pmutation, nmutation)
     bit_t positionvalue;
     double pmutation;
     int *nmutation;
{
  int tempmutation;

  if (flip (pmutation))
    {
      tempmutation = (positionvalue + 1) % 2;
      (*nmutation)++;
    }
  else
    {
      tempmutation = positionvalue;
    }
  return (tempmutation);
}

/*
 *    crossover -- cross a pair at a given site with mutation on a given trit
 */
static void
crossover (parent1, parent2, child1, child2, pcrossover, pmutation,
	   sitecross, nposition, ncrossover, nmutation)
     class_t *parent1;
     class_t *parent2;
     class_t *child1;
     class_t *child2;

     double pcrossover;
     double pmutation;

     int *sitecross;
     int nposition;
     int *ncrossover;
     int *nmutation;
{
  double inheritance;
  int j;

  if (flip (pcrossover))
    {
      *sitecross = rnd (0, nposition - 1);
      (*ncrossover)++;
    }
  else
    {
      *sitecross = nposition;	/* transfer but no cross */
    }

  /* transfer action part regardless of sitecross */
  child1->a = bmutation (parent1->a, pmutation, nmutation);
  child2->a = bmutation (parent2->a, pmutation, nmutation);

  /* transfer and cross above cross sites */
  for (j = 0; j < nposition; j++)
    {
      child1->c[j] = mutation (parent1->c[j], pmutation, nmutation);
      child2->c[j] = mutation (parent2->c[j], pmutation, nmutation);
    }

  /* transfer only below cross site */
  for (j = 0; j < *sitecross; j++)
    {
      child1->c[j] = mutation (parent1->c[j], pmutation, nmutation);
      child2->c[j] = mutation (parent2->c[j], pmutation, nmutation);
    }

  /* children inherit the average parental strength values */
  inheritance = avg (parent1->strength, parent2->strength);

  child1->strength = inheritance;
  child1->ebid = 0.0;
  child1->bid = 0.0;
  child1->matchflag = FALSE;
  child1->specificity = countspecificity (child1->c, nposition);

  child2->strength = inheritance;
  child2->ebid = 0.0;
  child2->bid = 0.0;
  child2->matchflag = FALSE;
  child2->specificity = countspecificity (child2->c, nposition);
}

/*
 *    worstofn -- select worst individual from subpopulation of size n
 */
static int
worstofn (pop, n)
     class_p *pop;
     int n;
{
  int j, worst, candidate;
  double worststrength;

  /* initialize with random selection */
  worst = rnd (0, pop->nclassifier - 1);
  worststrength = pop->classifier[worst].strength;

  /* select and compare to remaining subpopulation */
  if (n > 1)
    for (j = 2; j <= n; j++)
      {
	candidate = rnd (0, pop->nclassifier - 1);

	if (worststrength > pop->classifier[candidate].strength)
	  {
	    worst = candidate;
	    worststrength = pop->classifier[candidate].strength;
	  }
      }
  return (worst);
}

/*
 *    matchcount -- count number of positions of similarity
 */
static int
matchcount (classifier1, classifier2, nposition)
     class_t *classifier1;
     class_t *classifier2;
     int nposition;
{
  int j, tempcount;

  /* -= HOOK =- */
  if (classifier1->a == classifier2->a)
    tempcount = 1;
  else
    tempcount = 0;

  for (j = 0; j < nposition; j++)
    if (classifier1->c[j] == classifier2->c[j])
      tempcount++;

  return (tempcount);
}

/*
 *    crowding -- replacement using modified DeJong crowding
 */
static int
crowding (child, pop, crowdingfactor, crowdingsubpop)
     class_t *child;
     class_p *pop;
     int crowdingfactor;
     int crowdingsubpop;
{
  int j, popmember, match, matchmax, mostsimilar;

  matchmax = -1;
  mostsimilar = 0;

  if (crowdingfactor < 1)
    crowdingfactor = 1;

  for (j = 0; j < crowdingfactor; j++)
    {

      /* pick worst of n */
      popmember = worstofn (pop, crowdingsubpop);
      match = matchcount (child, &(pop->classifier[popmember]),
			  pop->nposition);
      if (match > matchmax)
	{
	  matchmax = match;
	  mostsimilar = popmember;
	}
    }
  return (mostsimilar);
}

/*
 *    statistics -- population stats: max, avg, min, sum of strength
 */
static void
statistics (pop)
     class_p *pop;
{
  int j;

  pop->maxstrength = pop->classifier[0].strength;
  pop->minstrength = pop->classifier[0].strength;
  pop->sumstrength = pop->classifier[0].strength;

  for (j = 1; j < pop->nclassifier; j++)
    {
      pop->maxstrength = my_max (pop->maxstrength, pop->classifier[j].strength);
      pop->minstrength = my_min (pop->minstrength, pop->classifier[j].strength);
      pop->sumstrength += pop->classifier[j].strength;
    }

  pop->avgstrength = pop->sumstrength / pop->nclassifier;
}

/*
 *    ga -- coordinate selection, mating, crossover, mutation and replacement
 */
void
ga (ga, pop)
     grecord_t *ga;
     class_p *pop;
{
  int j;
  class_t child1, child2;

  /* get average, max, min, sumstrength */
  statistics (pop);

  for (j = 0; j < ga->nselect; j++)
    {
      /* pick mates */
      ga->mating[j].mate1 = select (pop);
      ga->mating[j].mate2 = select (pop);

      /* cross and mutate */
      crossover (&(pop->classifier[ga->mating[j].mate1]),
		 &(pop->classifier[ga->mating[j].mate2]),
		 &child1, &child2,
		 ga->pcrossover, ga->pmutation,
		 &(ga->mating[j].sitecross),
		 pop->nposition,
		 &(ga->ncrossover),
		 &(ga->nmutation));

      ga->mating[j].mort1 = crowding (&child1, pop,
				      ga->crowdingfactor,
				      ga->crowdingsubpop);

      /* update sumstrength */
      pop->sumstrength -= pop->classifier[ga->mating[j].mort1].strength
	+ child1.strength;

      /* insert child in mort1's place */
      pop->classifier[ga->mating[j].mort1] = child1;

      /* same for second child */
      ga->mating[j].mort2 = crowding (&child2, pop,
				      ga->crowdingfactor,
				      ga->crowdingsubpop);

      pop->sumstrength -= pop->classifier[ga->mating[j].mort2].strength
	+ child2.strength;

      pop->classifier[ga->mating[j].mort2] = child2;
    }
}

/*
 *    reportga -- report on mating, crossover and replacement
 */
void
reportga (rep, ga, pop)
     FILE *rep;
     grecord_t *ga;
     class_p *pop;
{
  int j;

  page (rep);

  fprintf (rep, "\n\n");
  fprintf (rep, "Genetic Algorithm Report\n");
  fprintf (rep, "------------------------\n");
  fprintf (rep, "Pair  Mate1  Mate2  SiteCross  Mort1  Mort2\n");
  fprintf (rep, "-------------------------------------------\n");

  for (j = 0; j < ga->nselect; j++)
    fprintf (rep, "%3d   %3d    %3d      %3d      %3d    %3d\n",
	     j + 1,
	     ga->mating[j].mate1 + 1,
	     ga->mating[j].mate2 + 1,
	     ga->mating[j].sitecross + 1,
	     ga->mating[j].mort1 + 1,
	     ga->mating[j].mort2 + 1);

  fprintf (rep, "\n\n");
  fprintf (rep, "Statistics Report\n");
  fprintf (rep, "-----------------\n");
  fprintf (rep, "Average strength      = %8.2lf\n", pop->avgstrength);
  fprintf (rep, "Maximum strength      = %8.2lf\n", pop->maxstrength);
  fprintf (rep, "Minimum strength      = %8.2lf\n", pop->minstrength);
  fprintf (rep, "Sum of strength       = %8.2lf\n", pop->sumstrength);
  fprintf (rep, "Number of crossings   = %8d\n", ga->ncrossover);
  fprintf (rep, "Number of mutations   = %8d\n", ga->nmutation);
}
