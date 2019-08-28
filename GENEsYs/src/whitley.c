/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/
 
/*
 *
 *	$Id$
 *	$Log$
 *
 *
 *  file:    	whitley.c
 *
 *  author:    	Thomas Baeck (formula by Darrell Whitley)
 *
 *  created:    19 february 1991
 *
 *  purpose:    choose a new population via selection by Whitleys ranking.
 *
 */

#include "extern.h"
 
void
RnkWht()

{
	int		CmmSltAct();
 
    	static int	Flg = 1;
    	static int     *Sample;    	/* pointers to selected structures */

    	register int 	i;

	double		e1,
			e2,
			sqrt();

    	Trace("RnkWht() entered");
 
    	if (Flg) {
        	if ((Sample = (int *) calloc((unsigned) Popsize, sizeof(int)))
								== NULL) {
			Error("RnkWht: No space");
		}
        	Flg = 0;
    	}

	e1 = Eta_max - 1.0;
	e2 = Eta_max * Eta_max;

	for (i = 0; i < Popsize; i++)  { /* direct index calculation */

		Sample[i] = (int) (((double) Popsize) *
			    (Eta_max - sqrt(e2 - 4.0 * e1 * Rand())) /
			    (2.0 * e1));
		
		if (Traceflag) {
			printf("Sample[% d] = % d\n", i, Sample[i]);
		}
	}

	CmmSltAct(New, Old, Sample, Length, Popsize, F_nbr, Gapsize); 

    	Trace("RnkWht() completed");

} /* end RnkWht() */

/*** end of file ***/
