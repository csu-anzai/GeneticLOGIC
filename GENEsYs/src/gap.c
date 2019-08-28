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
 *      file:    gap.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    1981
 *
 *   purpose:    If the population gap is less than 1.0, choose survivors 
 *		 from old population uniformly, without replacement.
 *
 *  modified:    7 feb 86
 */
 
#include "extern.h"
 
 
void
Gap(sample)
int sample[];

{
    	static 		int 	firstflag = 1;
    	static 		int    *samp2;

    	register	int 	i,
				j,
				temp;
 
    	if (firstflag) {
        	if ((samp2 = (int *) calloc((unsigned) Popsize, 
			     		    sizeof(int))) == NULL){
			Error("Gap: No space");
		}
        	firstflag = 0;
    	}
 
    	for (i = 0; i < Popsize; i++) {	/* shuffle new structures */
        	j 	  = Randint(i,Popsize-1);
        	temp      = sample[j];
        	sample[j] = sample[i];
        	sample[i] = temp;
    	}
 
    	for (j = 0; j < Popsize; j++) 	/* construct uniform shuffle */
		samp2[j] = j;

    	for (j = 0; j < Popsize; j++) {
        	i 	 = Randint(j, Popsize-1);
        	temp 	 = samp2[i];
        	samp2[i] = samp2[j];
        	samp2[j] = temp;
    	}
 
    	for (i = Gapsize*Popsize; i < Popsize; i++) /* choose survivors */
        	sample[i] = samp2[i];

} /* end Gap() */
 
/*** end of file ***/
