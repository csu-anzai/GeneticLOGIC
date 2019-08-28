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
 *  file:	mutate.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	Perform mutation on the current population.
 *
 *	The global variable Mu_next indicates the position of
 *	next mutation, treating the entire population as a linear
 *	array of positions.  
 *
 *  modified:	7 feb 98
 */

#define   EXTERN
#include "global.h"


Mutate()
{
	static long bits;	/* number of bits per pop */
	register int i;		/* index of the Mutated structure */
	register int j;		/* position within the structure */
	register int k;		/* position within Mutated byte */
	register double r;	/* current random number */
	static int firstflag = 1;

	Trace("Mutate entered");

	if (firstflag)
	  {
	    bits = Popsize*Length;
	    firstflag = 0;
	  }

	if (M_rate > 0.0)
	{
		while (Mu_next<bits)
		{
			i = Mu_next/ Length;	/* Mutated structure */
			j = Mu_next % Length;	/* Mutated position */
			k = j % CHAR_BIT;   	/* Mutated bit  */
			j /= CHAR_BIT;      	/* Mutated byte */

			MUTATION(New[i].Gene[j], Bit[k]);
			New[i].Needs_evaluation = 1;

			/* update next mutation location */
			if (M_rate < 1.0)
			{
				while ((r = Rand()) == 0.0);
				Mu_next += ceil (log(r) / log(1.0 - M_rate));
			}
			else
				Mu_next += 1L;
		}

		/* adjust Mu_next for next Generation */
		Mu_next -= bits;

	}

	Trace("Mutate completed");
}


/** end of file **/

