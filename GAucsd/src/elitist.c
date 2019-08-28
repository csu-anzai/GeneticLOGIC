/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:	elitist.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1982
 *
 *  purpose:	The elitist policy stipulates that the best individual
 *		always survives into the new generation.  The elite
 *		individual is placed in the last position in New pop,
 *		and is not changed through crossover or mutation.
 *
 *  modified:	24 mar 86
 */

#define   EXTERN
#include "global.h"


Elitist()
{
	register int i;		/* loop control variables */
	register int k;
	register int found;	 /* set if elite is found */


	Trace("Elitist entered");

	/* is anybody in current population identical  */
	/* to the best guy in the previous generation? */

	for (i = 0, found = 0; i < Popsize && !found; i++)
	{
		for (k = 0, found = 1; found && (k < Full); k++)
			found = (New[i].Gene[k] == Old[Best_guy].Gene[k]);
		if (found && Slop)
			found = ((New[i].Gene[k] & Pre[Slop]) ==
				 (Old[Best_guy].Gene[k] & Pre[Slop]));
	}

	if (!found) 	/* elite was not present */
	{
		/* replace last guy with the elite one */
		for (k = 0; k < Bytes; k++)
			New[Popsize-1].Gene[k] = Old[Best_guy].Gene[k];
		New[Popsize-1].Perf = Old[Best_guy].Perf;
		New[Popsize-1].Needs_evaluation = 0;
		if (Traceflag)
			printf("perf: %e\n", New[Popsize-1].Perf);
	}

	Trace("Elitist completed");
}

/*** end of file ***/
