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
 *
 *	$Id$
 *	$Log$
 *
 *
 *      file:   elitist.c
 *
 *    author:   John J. Grefenstette
 *
 *   created:   1982
 *
 *   purpose:   The elitist policy stipulates that the best individual
 *        	always survives into the new generation.  The elite
 *        	individual is placed in the last position in New pop,
 *        	and is not changed through crossover or mutation.
 *
 *  modified:   24 mar 86
 */
 
#include "extern.h"
 
void
Elitist()

{
    register int i;      /* loop control variables 			*/
    register int k;
    register int found;  /* set if elite one is present 		*/
 
    Trace("Elitist entered");
 
    /* is any element in the current population        			*/
    /* identical to the Best guy in the last Generation?    		*/

    for (i = 0, found = 0; i < Popsize && (!found); i++)
        for (k = 0, found = 1; (k < Length) && (found); k++)
            found = (New[i].Gene[k] == Old[Best_guy].Gene[k]);
 
    if (   (!found && (Popsize >  1)) 		/* normal case */
	|| (!found && (Popsize == 1) 		/* pathological case */
		   && (Old[Best_guy].Perf < New[Popsize-1].Perf))) {    	

    	/* 
	 *	elite one was not present, replace last 
	 *	guy with the elite one 	
	 */

        for (k = 0; k < Length; k++) {
            New[Popsize-1].Gene[k] = Old[Best_guy].Gene[k];
	}

	if (NbrMttRts > 0) {
		for (k = 0; k < MttLen; k++) {
			New[Popsize-1].MttGen[k] = Old[Best_guy].MttGen[k];
		}
		for (k = 0; k < NbrMttRts; k++) {
			New[Popsize-1].MttRts[k] = Old[Best_guy].MttRts[k];
		}
	}

	if (FctDim > 0) {
		for (k = 0; k < FctDim; k++) {
			New[Popsize-1].ObjVar[k] = Old[Best_guy].ObjVar[k];
		}
	}

        New[Popsize-1].Perf 		= Old[Best_guy].Perf;
        New[Popsize-1].SltPrb		= Old[Best_guy].SltPrb;
        New[Popsize-1].Needs_evaluation = 1;

        if (Traceflag) {
            printf("perf: %e\n", New[Popsize-1].Perf);
        }
    }
 
    Trace("Elitist completed");
}
 
 
/*** end of file ***/
