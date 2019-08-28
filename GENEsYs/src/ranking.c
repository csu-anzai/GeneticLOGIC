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
 *  file:    	ranking.c
 *
 *  author:    	Thomas Baeck (algorithm by James E. Baker)	
 *
 *  created:    15 june 1990
 *
 *  purpose:    choose a new population via selection by ranking
 *
 *  modified:	Thomas Baeck, 4 aug 90 
 *			set Best_guy according to the sorted population.
 *
 *		Thomas Baeck, 3 sep 90 
 *			static, extinctive ranking variant.
 */

#include "extern.h"
 
void
Select_by_Ranking()

{
	int		CmmSltAct();
 
    	static 	int	Flg = 1;
    	static 	int    *Sample;    	/* pointers to Sampled structures */
    	static 	double 	ExpFrc;		/* multiplier for expected values */

    	double 		ComVal;		/* common value in expected values */
    	double 		Xpt;		/* expected number of offspring */
    	double 		Ptr;        	/* determines fractional selection */
    	double 		Sum;            /* control for selection loop */

    	register int 	i,
			k;

    	Trace("Select_by_Ranking entered");
 
    	if (Flg) {
        	if ((Sample = (int *) calloc((unsigned) Popsize, sizeof(int)))
								== NULL) {
			Error("Select_by_Ranking: No space");
		}
		ExpFrc = Popsize / Mu;
        	Flg    = 0;
    	}
 
    	k   = 0;        	/* index of next Selected structure */
    	Ptr = Rand();   	/* spin the wheel one time */
 
    	for (Sum = i = 0; i < Mu; i++) {	/* respect value of Mu	*/

    		/* ranking calculation of Xpt values */

		if (Mu == 1) {			/* pathological case */
			Xpt = (double) Popsize;
		}
		else {				/* serious selection */
			ComVal = 2.0 * (Eta_max - 1.0) *
			 	((double) i) / ((double) Mu - 1.0);

			switch (SltScm) {

			  case BRK_SLT:
        			Xpt = (Eta_max - ComVal) * ExpFrc;
				break;

			  case IRK_SLT:
        			Xpt = (2.0 - Eta_max + ComVal) * ExpFrc;
				break;

			  default:
        			Xpt = (Eta_max - ComVal) * ExpFrc;
				break;
			}
		}
			
        	for (Sum += Xpt; Sum > Ptr; Ptr++) {
          		Sample[k++] = i;
		}
    	}

	CmmSltAct(New, Old, Sample, Length, Popsize, F_nbr, Gapsize); 

    	Trace("Select_by_Ranking completed");

} /* end Select_by_Ranking() */

/*** end of file ***/ 
