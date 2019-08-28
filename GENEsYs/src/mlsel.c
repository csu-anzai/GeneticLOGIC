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
 *      file:   mlsel.c
 *
 *    author:   Thomas Baeck 
 *
 *   created:   31 july 1990
 *
 *   purpose:   Choose a new population via the following selection mechanism:
 *		The 'mu' (1 <= Mu <= Popsize) best individuals of the 
 *		population are selected with equal probabilities (1/Mu) to
 *		generate copies, until the population is filled.
 *
 *		If MLC_SLT is , the actual selection scheme, Mu best 
 *		individuals are first copied once. 
 *		The remaining places are filled by taking random Samples
 *		from the Mu best ones.
 *
 */

#include "extern.h"

void
Select_mu_best()

{
	int		CmmSltAct();
    	static 		Flg = 1;
    	static 	 int    *Sample;    	/* pointers to Selected structures */

    	register int    i;

    	int    		start = 0;	/* starting point for selection	*/
 
    	Trace("Select_mu_best() entered");
 
    	if (Flg) {
        	if ((Sample = (int *) calloc((unsigned) Popsize, 
					     sizeof(int))) == NULL){
			Error("Select_mu_best: No space");
		}
        	Flg = 0;
    	}
 
    	if (SltScm == MLC_SLT) {	/* copy best individuals */

		for (i = Rho - 1; i < Mu; i++) {	
			Sample[i - Rho + 1] = i;
		}
		start = Mu - Rho + 1;
    	}

    	for (i = start; i < Popsize; i++) { /* select individuals */
		Sample[i] = Randint(Rho - 1, Mu - 1);
    	}
 
	CmmSltAct(New, Old, Sample, Length, Popsize, F_nbr, Gapsize); 

    	Trace("Select_mu_best completed");
}

/* end Select_mu_best() */
