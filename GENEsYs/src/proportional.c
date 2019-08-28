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
 *      file:   proportional.c
 *
 *    author:   John J. Grefenstette (algorithm by James E. Baker)
 *
 *   created:   4 august 1987
 *
 *   purpose:   Proportional selection.
 *
 *  modified:	Thomas Baeck, 5 sep 90	
 *			Allow the extinctive selection schemes. This means,
 *			that not all individuals may get a chance to reproduce
 *			and provides a combination of (Mu,lambda)-selection
 *			with proportional selection as suggested in our
 *			work.
 */
 
#include "extern.h"

extern FUNCTION		f_tab[]; 

void 
Select()

{
    	static 	 int 	Flg = 1;
    	static 	 int   *Sample;    	/* pointers to Selected structures */
	static 	 double ExpFrc;		/* multiplier for expected values */

    	register int 	i,
			k;

	int		CmmSltAct(); 	/* common actions for selection */

    	double 		Xpd;    	/* Xpd number of offspring */
    	double 		Fac;        	/* normalizer for Xpd value */
    	double 		Ptr;       	/* determines fractional selection */
    	double 		Sum;            /* control for selection loop */
 
    	Trace("Select entered");

    	if (Flg) {
        	if ((Sample = (int *) calloc((unsigned) Popsize, 
					     sizeof(int))) == NULL){
			Error("Select: No space");
		}
		ExpFrc = Popsize / (Mu - Rho + 1);
        	Flg    = 0;
    	}

	if (Mu == Popsize) {		/* calc. denominator for selection */
    		Fac = 1.0 / (Worst - Ave_current_perf);	
	}
	else {
		Fac = 1.0 / (Worst - AvgBstPfr);
	}

    	k   = 0;        	/* index of next selected structure 	*/
    	Ptr = Rand();   	/* spin the wheel one time 		*/

    	for (Sum = 0.0, i = Rho - 1; i < Mu; i++) {  /* respect value of Mu */
	
            	Xpd 	      = (Worst - Old[i].Perf) * ExpFrc * Fac;
		Old[i].SltPrb = Xpd / ((double) (Mu - Rho + 1));

        	for (Sum += Xpd; Sum > Ptr; Ptr++) {
            		Sample[k++]    = i;
		}
    	}

	CmmSltAct(New, Old, Sample, Length, Popsize, F_nbr, Gapsize);

    	Trace("Select completed");

} /* end Select() */

/*** end of file ***/
