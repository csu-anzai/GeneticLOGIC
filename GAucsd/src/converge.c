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
 *  file:	converge.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1982
 *
 *  purpose:	measure system convergence
 *
 *  modified:	7 feb 86
 *
 *		13 nov 86: leave structures packed.
 */

#define   EXTERN
#include "global.h"

Converge()
{
	
	register int i,j;
	register int ones;
	int focus;
	int bit;
	FILE *fp, *fopen();

	Trace("Converge entered");

	Bias = 0.0;
	Lost = Conv = 0;
	if (!Convflag) return;

	for (j = 0; j < Length; j++)
	{
		focus = j / CHAR_BIT;
		bit = j % CHAR_BIT;
		ones = 0;
		for (i = 0; i < Popsize; i++)
			ones += ((New[i].Gene[focus] & Bit[bit]) != 0);
		Lost += (ones == 0) || (ones == Popsize);
		Conv += (ones <= Few) || (ones >= Popsize - Few);
		Bias += (ones > Popsize/2) ? ones : (Popsize - ones);
	}
	Bias /= Popsize*Length;

	Trace("Converge completed");
}

/*** end of file ***/
