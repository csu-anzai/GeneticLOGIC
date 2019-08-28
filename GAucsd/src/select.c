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
 *  file:	select.c
 *
 *  author:	John J. Grefenstette (algorithm by James E. Baker)
 *
 *  created:	4 august 1987
 *
 *  purpose:	choose a new population via Baker's selection.
 */

#define   EXTERN
#include "global.h"


Select()
{
	extern Gap();

	static firstflag = 1;
	static int *sample;	/* pointers to Selected structures	*/
	double expected;	/* expected number of offspring		*/
	double factor;		/* normalizer for expected value        */
	double ptr;		/* determines fractional selection	*/
	double sum;             /* control fro selection loop           */
	int i, j, k, temp;	/* loop control				*/

	Trace("Select entered");

	if (firstflag)
	{
		sample = (int *) calloc((unsigned) Popsize, sizeof(int));
		firstflag = 0;
	}

	/* denominator for selection probabilities */
	for (sum = i = j = 0; i < Popsize; i++)
		if (Old[i].Perf < Worst)
		{
			sum += Old[i].Perf;
			j++;
		}

	factor = Popsize/(Worst*j - sum);

	k = 0;		/* index of next Selected structure */
	ptr = Rand();   /* spin the wheel one time */

	for (sum = i = 0; i < Popsize; i++)
	{
		if (Old[i].Perf < Worst)
			expected = (Worst - Old[i].Perf) * factor;
		else expected = 0.0;

		for (sum += expected; sum > ptr; ptr++)
			sample[k++] = i;
	}
	if (k != Popsize) Error("Select: internal scaling error");

	if (Gapsize < 1.0) Gap(sample); 	/* Generation Gap */ 
	

	/* randomly shuffle pointers to new structures */
	for (i = 0; i < Popsize; i++)
	{
		j = Randint(i, Popsize-1);
		temp = sample[j];
		sample[j] = sample[i];
		sample[i] = temp;
	}

	/* finally, form the new population */
	for (i = 0; i < Popsize; i++)
	{
		k = sample[i];
		for (j = 0; j < Bytes; j++)
			New[i].Gene[j] = Old[k].Gene[j];

		New[i].Perf = Old[k].Perf;
		New[i].Needs_evaluation = 0;
	}

	Trace("Select completed");
}


/*** end of file ***/

