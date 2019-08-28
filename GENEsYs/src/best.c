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
 *      file:    best.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    1983
 *
 *   purpose:    input and output of best structures
 *
 *  modified:    15 apr 86
 *
 *        16 dec 86: don't print Bestsize to minfile in Printbest()
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
 */
 
#include "extern.h"

#define 	fopen(a,b)	fOpen((a),(b))

extern FILE 		*fOpen();
extern FUNCTION 	 f_tab[];
extern int		 xcoord[],
			 ycoord[];
 
static double max;    	/* maximum (worst) performance in the Bestset 	*/
static int    maxptr;  	/* pointer to structure with perf = max 	*/
 
 
void
Savebest(i)
register int 		i;	/* number of structure to save */

{
    	/*  Save the ith structure in current population */
    	/*  if it is one of the Savesize best seen so far */
 
    	register int 	j,      /* loop control var */			
			k,
			ptr,	/* pointer to exchange element */
    			found;

	void		CpyBst();

        /* Bestsize is the number saved so far, so          	*/
        /* there may be empty slots in the Bestset.      	*/
        /* Check if an identical structure is already there. 	*/

        for (j = 0, found = 0; j < Bestsize && (!found); j++)
           	for (k = 0, found = 1; (k < Length) && (found); k++)
                	found = (New[i].Gene[k] == Bestset[j].Gene[k]);
        if (found) 
		return;

	if (Bestsize < Savesize) {
		ptr = Bestsize;
		CpyBst(&Bestset[ptr], &New[i]);
        	Bestsize++;
	}
	else { 		/* find worst element in Bestset */

        	for (max    = Bestset[0].Perf,
            	     maxptr = 0,
		     j      = 1; 
		     j      < Savesize; 
		     j++) {
                	if (max < Bestset[j].Perf) {
                    		max    = Bestset[j].Perf;
                    		maxptr = j;
                	}
            	}
		ptr = maxptr;
		if (New[i].Perf < max) {
			CpyBst(&Bestset[ptr], &New[i]);
		}
	}

} /* end Savebest */
		


void 
CpyBst(Bst, Ind)
register BESTSTRUCT	*Bst;		/* pointer to target */
register STRUCTURE	*Ind;		/* pointer to source */

{

	register int	 k;

        for (k = 0; k < Length; k++) {	
            	Bst->Gene[k] = Ind->Gene[k];
        }
	for (k = 0; k < FctDim; k++) {	
		Bst->ObjVar[k] = Ind->ObjVar[k];
	}
	if (f_tab[F_nbr].MrkFct == PERM) {
		for (k = 0; k < FctDim; k++)
			Bst->PerMut[k] = Ind->PerMut[k];
	}
	for (k = 0; k < MttLen; k++) {	
		Bst->MttGen[k] = Ind->MttGen[k];
	}
	for (k = 0; k < NbrMttRts; k++) {	
		Bst->MttRts[k] = Ind->MttRts[k];
	}
	Bst->Perf   = Ind->Perf;
	Bst->Gen    = Gen;
	Bst->Trials = Trials;

} /* end CpyBst */

 
void
Printbest()

{
    	/* Write the Best structures out to the Bestfile. */
 
    	FILE 	       *fp;

	void 		PrtPmt(),
			PrtBin();
 
    	Trace("Printbest entered");

    	if ((fp = fopen(Bestfile, "w")) != NULL) {

		switch (f_tab[F_nbr].MrkFct) {

			case PERM:
				PrtPmt(fp);
				break;

			case BINY:
			case REAL:
				PrtBin(fp);
				break;

			default:
				PrtBin(fp);
				break;
		}

    		fclose(fp);
    	}
    	else {
    		perror("Printbest(): open Bestfile");
	}
 
    	Trace("Printbest completed");
 
} /* end Printbest() */
 



void 
PrtPmt(fp)
FILE		*fp;

{

	register int	i,
			j;

	for (i = 0; i < FctDim; i++) {	/* print the best permutation */
		j = Bestset[0].PerMut[i];
		fprintf(fp, "%d\t%d\t%d\n", xcoord[j], ycoord[j], j);
	}
	j = Bestset[0].PerMut[0];	/* print last edge and length */
	fprintf(fp, "%d\t%d\t%d\t%12.10e\n", 
		xcoord[j], ycoord[j], j, Bestset[0].Perf);

} /* end PrtPmt */




void
PrtBin(fp)
FILE		*fp;

{

	register int	i,
			j;

    	for (i = 0; i < Bestsize; i++) {
        	for (j = 0; j < Length; j++) {
            		if ((j > 0) && ((j % 8) == 0)) 
				fprintf(fp, " ");
            		fprintf(fp, "%1d", Bestset[i].Gene[j]);
        	}
        	fprintf(fp, "  %11.4e ", Bestset[i].Perf);
        	fprintf(fp, " %4d  %4d\n", Bestset[i].Gen, Bestset[i].Trials);
      	}

} /* end PrtBin */


/**** end of file ****/
