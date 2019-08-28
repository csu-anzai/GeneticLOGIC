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
 *  file:	gap.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	If the population gap is less than 1.0,
 *		choose survivors from old population	
 *		uniformly, without replacement.	
 *
 *  modified:	7 feb 86
 */

#define   EXTERN
#include "global.h"


Gap(sample)
int sample[];
{
	static firstflag = 1;
	static int *samp2;
	int i,j;		
	int temp;

	if (firstflag)
	{
		samp2 = (int *) calloc((unsigned) Popsize, sizeof(int));
		firstflag = 0;
	}

	/* randomly shuffle the new structures */
	for (i=0; i<Popsize; i++)
	{
		j = Randint(i,Popsize-1);
		temp = sample[j];
		sample[j] = sample[i];
		sample[i] = temp;
	}

	/* construct a uniform shuffle */
	for (j=0; j<Popsize; j++) samp2[j]=j;
	for (j=0; j<Popsize; j++)
	{
		i = Randint(j, Popsize-1);
		temp = samp2[i];
		samp2[i] = samp2[j];
		samp2[j] = temp;
	}

	/* now choose survivors */
	for (i=Gapsize*Popsize; i<Popsize; i++)
		sample[i] = samp2[i];
}

/* end of file */
