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
 *	$Id: converge.c,v 1.1 1992/06/19 16:53:04 baeck Exp $
 *	$Log: converge.c,v $
 * Revision 1.1  1992/06/19  16:53:04  baeck
 * Initial revision
 *
 *
 *
 *      file:    converge.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    1982
 *
 *   purpose:    measure system convergence
 *
 *  modified:    7 feb 86
 *
 *        13 nov 86: leave structures packed.
 *
 *		Thomas Baeck, 19 jul 90
 *			respect fopen() errors.
 * 
 *		Thomas Baeck, 28 nov 90
 *			Removed the packing of bits to characters in order
 *			to simplify the implementation of genetic operators
 *			as well as to avoid the unnecessary amount of time
 *			for Packing and Unpacking. Our machine has enough 
 *			memory to work with chararacter-bitstrings.
 *
 */
 
#include "extern.h"

#define 	fopen(a,b)	fOpen((a),(b))

extern FILE *fOpen();
 
void 
Converge()

{
 
    	register int 		i,
				j,
				Ons;

	char			Msg[NSZ];

    	FILE 		       *fp;
 
    	Trace("Converge entered");
 
    	for (	Bias = 0.0, Lost = Conv = 0, j = INDEX(0); 
	     	j < Length; 
		j++) {

        	for (Ons = 0, i = 0; i < Popsize; i++)
            		Ons += (New[i].Gene[j] != 0);

        	Lost += (Ons == 0)   || (Ons == Popsize);
        	Conv += (Ons <= FEW) || (Ons >= Popsize - FEW);
        	Bias += (Ons > Popsize / 2) ? Ons : (Popsize - Ons);
    	}
 
	Bias /= (Popsize * Length);
 
    	if (Lost == Length) {

        	if ((fp = fopen(Logfile, "a")) != NULL) {
        		fprintf(fp, "CONVERGED at Gen %1d, ",Gen);
        		fprintf(fp, "after %1d Trials\n", Trials);
        		fclose(fp);
		}
		else {
			sprintf(Msg, "Converge: can't open %s", Logfile);
			Error(Msg);
		}
    	}
 
    	Trace("Converge completed");

} /* end Converge() */
 
/*** end of file ***/
