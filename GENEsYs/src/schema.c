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
 *      file:   schema.c
 *
 *    author:   John J. Grefenstette
 *
 *   created:   3 feb 86
 *
 *   purpose:   measure the current allocation of trials to a schema
 *        	and record the results in Schemafile.
 *
 *  modified:   13 feb 86
 *
 *		Thomas Baeck, 19 jul 90 
 *			respect fopen() errors.
 */
 
#include "extern.h"

#define fopen(a,b) 	fOpen((a),(b))

extern FILE *fOpen();
 
void 
Schema()

{
    	static int 	lastcount  = 1;
    	static int 	firstflag  = 1;
    	static int 	firstcount = 1;
    	static FILE    *fp;
    	static char    *S;

    	register int 	i,
			j,
			count,
			ok;

    	double 		expected;
    	double 		perf;

    	char 		msg[NSZ];
    	char 		tmp;
	char	       *malloc();
 
    	Trace("Schema entered");
 
    	if (firstflag) { /*  initialize schema S from schemafile */

        	if ((S = malloc((unsigned) Length)) == NULL) {
			Error("Schema: No space");
		}
        	if ((fp = fopen(Schemafile, "r")) == NULL) {
            		sprintf(msg, "Schema: can't open %s", Schemafile);
            		Error(msg);
        	}

        	for (i = 0; i < Length; i++) {	
            		fscanf(fp, "%c", &tmp);
            		if (tmp == '0')  
				S[i] = 0;
            		else 
				if (tmp == '1')  
		    			S[i] = 1;
            			else  
		    			S[i] = '#';
        	}

        	fclose(fp);

        	if ((fp = fopen(Schemafile, "w")) == NULL) {
            		sprintf(msg, "Schema: can't open %s", Schemafile);
            		Error(msg);
        	}

        	for (i = 0; i < Length; i++) {
            		if (S[i] == '#')  
				fprintf(fp, "#");
            		else 
				if (S[i])    
		    			fprintf(fp, "1");
            			else              
		    			fprintf(fp, "0");
        	}

        	fprintf(fp, "\n");
        	fprintf(fp, " Gen  Count  Incr  Expct  ");
        	fprintf(fp, "Schema Ave    Pop. Ave\n");
        	firstflag = 0;
    	}
 
    	/* record count and expected offspring of S in current pop */

    	expected = 0.0;
    	perf     = 0.0;
    	count    = 0;

    	for (i = 0; i < Popsize; i++) {
        	for (ok = 1, j = 0; ok && (j < Length); j++) {
            		ok = (S[j] == '#') || (S[j] == New[i].Gene[j]);
        	}
        	if (ok) {
            		count++;
            		expected += (Worst-New[i].Perf) / 
				    (Worst-Ave_current_perf);
            		perf     += New[i].Perf;
        	}
    	}
 
    	if (firstcount && count) {
        	lastcount  = count;
        	firstcount = 0;
    	}

    	if (count) {
        	fprintf(fp, "%4d  %4d ", Gen, count);
        	fprintf(fp, " %5.3f ", (count*1.0)/lastcount);
        	lastcount = count;
        	fprintf(fp, " %5.3f ", expected/count);
        	fprintf(fp, " %10.3e ", perf/count);
        	fprintf(fp, " %10.3e ", Ave_current_perf);
        	fprintf(fp, "\n");
    	}
 
    	Trace("Schema completed");
 
} /* end Schema() */
 
/***  end of file ***/
