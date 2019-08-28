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
 *      file:    checkpoint.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    1982
 *
 *   purpose:    save global variables in a file for later restart
 *
 *  modified:    18 apr 86
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

#define fopen(a,b)	fOpen((a),(b))

extern FILE *fOpen();
 
void
Checkpoint(CkpFil)
char 			*CkpFil;

{
	extern void 		Printbest();

    	FILE 	               *fp;

	char			Msg[NSZ];

    	register int 		i,
				j;
 
    	Trace("Checkpoint entered");
 
    	if ((fp = fopen(CkpFil, "w")) != NULL) {

      		fprintf(fp, "Experiment %d\n", Experiment);
    		fprintf(fp, "Totonline %e\n", Totonline);
    		fprintf(fp, "Totoffline %e\n", Totoffline);
    		fprintf(fp, "Gen %d\n", Gen);
    		fprintf(fp, "Onsum  %e\n", Onsum);
    		fprintf(fp, "Offsum %e\n", Offsum);
    		fprintf(fp, "Trials %d\n", Trials);
    		fprintf(fp, "Plateau %d\n", Plateau);
    		fprintf(fp, "Best %e\n", Best);
    		fprintf(fp, "Worst  %e\n", Worst);
    		fprintf(fp, "Spin %d\n", Spin);
    		fprintf(fp, "Curr_dump %d\n", Curr_dump);
    		fprintf(fp, "Mu_next %d\n", Mu_next);
    		fprintf(fp, "Random Seed %u\n", Seed);
    		fprintf(fp, "Initialization Seed %u\n", Initseed);
 
    		fprintf(fp,"\n");
    		fprintf(fp, "Window\n");

    		for (i = 0; i < Windowsize; i++) {
			fprintf(fp, "%e\n", Window[i]);
		}
    		fprintf(fp,"\n");
 
    		for (i = 0; i < Popsize; i++) {
        		for (j = 0; j < Length; j++) {
            			fprintf(fp, "%1d", New[i].Gene[j]);
			}
        		fprintf(fp, "\n");
			if (FctDim > 0) {
				for (j = 0; j < FctDim; j++) {
					fprintf(fp, "% 11.9e ", 
						New[i].ObjVar[j]);
				}
			}
        		fprintf(fp, "\n");
			if (NbrMttRts > 0) {
				for (j = 0; j < MttLen; j++) {
					fprintf(fp, "%1d", 
						New[i].MttGen[j]);
				}
        			fprintf(fp, "\n");
				for (j = 0; j < NbrMttRts; j++) {
					fprintf(fp, "%11.9e ", 
						New[i].MttRts[j]);
				}
        			fprintf(fp, "\n");
			}
			fprintf(fp, "%11.9e ", New[i].SltPrb);
        		fprintf(fp, "% 11.9e ", New[i].Perf);
        		fprintf(fp, "%1d", New[i].Needs_evaluation);
        		fprintf(fp, "\n");
    		}
    		fclose(fp);
    	}
    	else {
		sprintf(Msg, "Checkpoint: can't open %s", CkpFil);
		Error(Msg);
	}
 
    	if (Savesize) {		/* save the best structures */
        	Printbest();
	}
 
    	Trace("Checkpoint completed");
}
 
/** end of file **/
 
