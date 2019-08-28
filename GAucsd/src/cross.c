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
 *  file:	cross.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	perform two-point crossover on entire population
 *
 *  modified:	8 apr 86
 *
 *		13 nov 86:  perform crossover on packed structures.
 *
 *              4 august 87: add second crossover point
 *
 *              11 july 89: fix bug when xbyte1 == xbyte2 
 *                   (bug found by Jon Richardson.)
 *
 *		7 august 89: allow C_rate > 1.0   - Nici.
 */

#define   EXTERN
#include "global.h"

Crossover()
{
	static int firstflag = 1;
	static int last;	/* last element to undergo Crossover */

	register int mom, dad;	/* participants in the crossover */
	register int xpoint1;	/* first crossover point w.r.t. structure */
	register int xpoint2;	/* second crossover point w.r.t. structure */
	register int xbyte1;	/* first crossed byte */
	register int xbit1; 	/* first crossed bit in xbyte1 */
	register int xbyte2;	/* last crossed byte */
	register int xbit2; 	/* last crossed bit in xbyte2 */
	register int i;     	/* loop control variable */
	register char temp; 	/* used for swapping alleles */

	int diff;    	/* set if parents differ from offspring */
	int count = 0;	/* loop counter for crossovers */
	char *kid1;  	/* pointers to the offspring */
	char *kid2;

	Trace("Crossover entered");

	if (firstflag)
	{
		last = C_rate*Popsize;
		firstflag = 0;
	}

	while (count < last)
	{
		mom = count++ % Popsize;
		dad = count++ % Popsize;

		/* kids start as identical copies of parents */
		kid1 = New[mom].Gene;
		kid2 = New[dad].Gene;

		/* choose two Crossover points */
		xpoint1 = Randint(0, Length);
		xpoint2 = Randint(0, Length - 1);

		/* guarantee that xpoint1 < xpoint2 */
		if (xpoint2 >= xpoint1) xpoint2++;
		else
		{
			i = xpoint1;
			xpoint1 = xpoint2;
			xpoint2 = i;
		}

		xbyte1 = xpoint1 / CHAR_BIT;
		xbit1  = xpoint1 % CHAR_BIT;
		xbyte2 = xpoint2 / CHAR_BIT;
		xbit2  = xpoint2 % CHAR_BIT;

		/* do parents differ outside cross segment? */
		for (diff = i = 0; !diff && i < xbyte1; i++)
			diff = (kid1[i] != kid2[i]);
		for (i = xbyte2 + 1; !diff && i < Full; i++)
			diff = (kid1[i] != kid2[i]);
		if (!diff)
		{
			diff = ((kid1[xbyte1] & Pre[xbit1]) !=
				(kid2[xbyte1] & Pre[xbit1]));
			if (xbyte2 < Full)
				diff += ((kid1[xbyte2] & Post[xbit2]) !=
					 (kid2[xbyte2] & Post[xbit2]));
			if (Slop)
				diff += ((kid1[Full] & Pre[Slop]) !=
					 (kid2[Full] & Pre[Slop]));
		}

		if (diff)	/* they do */
		{
			/* perform crossover */
			temp = kid1[xbyte1];
			kid1[xbyte1] = (kid1[xbyte1] & Pre[xbit1]) |
				(kid2[xbyte1] & Post[xbit1]);

			kid2[xbyte1] = (kid2[xbyte1] & Pre[xbit1]) |
				(temp & Post[xbit1]);

			diff = ((kid1[xbyte1] & Post[xbit1]) !=
				(kid2[xbyte1] & Post[xbit1]));

			for (i = xbyte1 + 1; i < xbyte2; i++)
			{
				temp = kid1[i];
				kid1[i] = kid2[i];
				kid2[i] = temp;
				diff += (kid1[i] != kid2[i]);
			}

			if (xbyte1 < xbyte2)
			{
				temp = kid1[xbyte2];
				kid1[xbyte2] = (kid1[xbyte2] & Post[xbit2]) |
					(kid2[xbyte2] & Pre[xbit2]);

				kid2[xbyte2] = (kid2[xbyte2] & Post[xbit2]) |
					(temp & Pre[xbit2]);

				diff += ((kid1[xbyte2] & Pre[xbit2]) !=
					 (kid2[xbyte2] & Pre[xbit2]));
			}
			else
			{
				temp = kid1[xbyte2];
				kid1[xbyte2] = (kid1[xbyte2] & Pre[xbit2]) |
					(kid2[xbyte2] & Post[xbit2]);

				kid2[xbyte2] = (kid2[xbyte2] & Pre[xbit2]) |
					(temp & Post[xbit2]);

				diff = ((kid1[xbyte2] & Post[xbit1] & Pre[xbit2]) !=
					(kid2[xbyte2] & Post[xbit1] & Pre[xbit2]));

			}

			if (diff)	/* kids differ from parents */
			{
				/* set evaluation flags */
				New[mom].Needs_evaluation = 1;
				New[dad].Needs_evaluation = 1;
			}
		}
	}
	Trace("Crossover completed");
}

/** end of file **/

