/*************************************************************/
/*                                                           */
/*  Copyright (c) 1989                                       */
/*  Nicol N. Schraudolph                                     */
/*  Computer Science & Engineering, C-014                    */
/*  University of California, San Diego                      */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:   	dpe.c
 *
 *  author: 	Nicol N. Schraudolph
 *
 *  created:	November 1989
 *
 *  purpose:	implement Dynamic Parameter Encoding
 *
 */

#define   EXTERN
#include "global.h"


DPE()
{
	
	register int i,j;
	register int one, two;
	register int bit1, bit2;
	register int focus1, focus2;
	int zoom, active, posn;
	double range;
	FILE *fp, *fopen();

	Trace("DPE entered");
	posn = 0;

	for (j = 0; j < GAgenes; j++)
	{
		if (GAposn[j] < 1)
		{
			posn = -GAposn[j];
			continue;
		}

		bit1 = posn % CHAR_BIT;
		focus1 = posn / CHAR_BIT;
		posn++;
		bit2 = posn % CHAR_BIT;
		focus2 = posn / CHAR_BIT;

		one = two = 0;
		for (i = 0; i < Popsize; i++)
		{
			if (New[i].Gene[focus1] & Bit[bit1]) one++;
			if (!(New[i].Gene[focus2] & Bit[bit2])) two++;
		}

		DPEhist[2*j] *= 1.0 - 1.0/DPEfreq;
		DPEhist[2*j] += one/(double)DPEfreq;
		one = DPEhist[2*j];
		DPEhist[2*j+1] *= 1.0 - 1.0/DPEfreq;
		DPEhist[2*j+1] += two/(double)DPEfreq;
		two = DPEhist[2*j+1];

		zoom = 0;
		if (one < Few) zoom = 1;
		else
		{
			one = Popsize - one + 1;
			if (one < Few) zoom = 3;
			else one = Few;
		}
		if (two < one)
		{
			zoom = 2;
			bit1 = bit2;
			focus1 = focus2;
		}

		range = 1.0;
		for (i = 0; i <= GAposn[j] - posn; i++) range *= 2.0;
		range *= GAfact[j];
		posn = GAposn[j];

		if (zoom)
		{
			Lastzoom = Gen;
			DPEhist[2*j] = DPEhist[2*j+1] = 0.5*Popsize;
			range /= 2.0;
			GAfact[j] /= 2.0;
			GAbase[j] += (zoom - 1)*(range/2.0);
			bit2 = (posn - 1) % CHAR_BIT;
			focus2 = (posn - 1) / CHAR_BIT;

			for (i = 0; i < Popsize; i++)
			{
				one = focus1;
				two = New[i].Gene[one] & Pre[bit1];
				two |= (New[i].Gene[one] << 1) & Post[bit1];
				if (zoom > 1) two ^= Bit[bit1];

				while (one < focus2)
				{
					if (New[i].Gene[one+1] & Pre[1]) two ^= 1;
					New[i].Gene[one++] = two;
					two = New[i].Gene[one] << 1;
				}
				New[i].Gene[one] &= Post[bit2];
				New[i].Gene[one] |= two & Pre[bit2];

				if (Rand() < 0.5) New[i].Gene[one] ^= Bit[bit2];
				New[i].Needs_evaluation = 1;
			}

			fp = fopen(DPEfile, "a");
			fprintf(fp, "%4d %7ld %3d   % le", Gen, Trials, j, GAbase[j]);
			fprintf(fp, " % le   %le\n", GAbase[j] + range, GAfact[j]);
			fclose(fp);
		}
	}
	Trace("DPE completed");
}

/*** end of file ***/
