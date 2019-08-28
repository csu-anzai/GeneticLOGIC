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
 *  file:    	report.c
 *
 *  author:    	John J. Grefenstette
 *
 *  created:    sep 1981
 *
 *  purpose:    generate a report summarizing the mean
 *        	and variance of a number of performance
 *        	measures of a GA run.
 *
 *  modified:   5 aug 86
 *
 *		Thomas Baeck, 19 jul 90 
 *			respect fopen() errors.
 *
 *		Thomas Baeck, 25 jul 90 
 *			Best_current_perf and worst_current_perf added.
 *
 *		Thomas Baeck, 24 nov 90 
 *			stringsize given by NSZ from define.h
 */

#include <stdio.h> 
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "global.h"

#define fopen(a,b) 	fOpen((a),(b))
#define REPROWS 	5000
#define COLUMNS 	20
#define BUFSZ		1024

#define USAGE		"GENEsYs 1.0\t\t\t(c) 1992 Thomas Baeck\n\n\
`report' is usually not explicitly called,\n\
but invoked automatically by `ga'\n\
Options:\n\
\t-i suff \t\t# File suffix of the infile for later\n\
\t        \t\t# report generation\n\
\t-h       \t\t# This help information\n\
\n"

#define	OPTSTR		"i:h"

extern 	FILE 	       *fOpen();
extern 	void 		Error();
extern  int		WrtDta(),
			GetDta();

double 			average[REPROWS][COLUMNS];
double 			variance[REPROWS][COLUMNS];
double 			line[COLUMNS];
 
 
main(argc, argv)
int 		argc;
char 	       *argv[];

{
	extern char    *optarg;

	extern int      optind,
			opterr;

    	register int 	row,
			col,
			i,
			j;

    	double 		oldgens = -1.0, 	/* previous Generation count */
			tmp;

    	char 		Outfile[NSZ],    	/* output file produced */
			Infile[NSZ], 		/* Input file */
			AvgFil[NSZ],		/* average report file */
			VarFil[NSZ],		/* variance report file */
			Msg[NSZ], 		/* Message strings */
			Str[BUFSZ];
    
	char 	       *s,
	 	       *fgets(),
		       *strcpy(),
	 	       *strtok();

    	int 		Opt,
			expn,       		/* number of Experiments */
			columns,		/* number of columns */
    			lines   = 0,        	/* number of lines in Report */
    			eof     = 0,        	/* true iff Outfile exhausted */
    			cutoff  = 0,     	/* was data truncated ? */
			colflag = 0;		/* flag for column errors */
 
    	FILE 	       *fp;
 
    	/* For the purpose of computing the means and the variances,	*/
    	/* the number of lines is taken to be the minimum number of 	*/
    	/* lines produced by any Experiment.  If any data is        	*/
    	/* discarded, the flag cutoff is set, so that a warning     	*/
    	/* may be printed.                                          	*/
 
    	for (row = 0; row < REPROWS; row++) {
        	for (col = 0; col < COLUMNS; col++) {
            		average[row][col] = 0.0;
            		variance[row][col] = 0.0;
        	}
	}

	while ((Opt = getopt(argc, argv, OPTSTR)) != -1) {

		switch (Opt) {

			case 'i': /* file suffix */

				sscanf(optarg, "in.%s", Msg);
				if ((SrcFil(Infile,  "in",  Msg) < 0))
					exit(1);
				sprintf(Logfile, "%s%s%s", Msg, _PTHSEP, "log");
				sprintf(Outfile, "%s%s%s", Msg, _PTHSEP, "out");
				sprintf(AvgFil,  "%s%s%s", Msg, _PTHSEP, "rpt");
				sprintf(VarFil,  "%s%s%s", Msg, _PTHSEP, "var");
				break;

			case 'h':
				printf(USAGE);
				exit(0);
				break;

			default:
				printf(USAGE);
				exit(1);
				break;
		}
	}

    	if ((fp = fopen(Infile, "r")) != NULL) {	/* read parameters */
    		GetDta(fp);
    		fclose(fp);
    	}
        else {
		sprintf(Msg, "Report: can't open %s", Infile);
        	Error(Msg);
	}

    	if ((fp = fopen(Outfile, "r")) == NULL) { 	/* get the Outfile */
        	sprintf(Msg, "Report: can't open %s", Outfile);
        	Error(Msg);
    	}
 
    	if (fgets(Str, BUFSZ, fp) == NULL) { 	/* read first line of Outfile */
		sprintf(Msg, "Report: unexpected EOF on %s", Outfile);
		Error(Msg);
    	}
    	else {
		i = 0;
		s = strtok(Str," ");
		do {
	    		sscanf(s, "%lf", &line[i]);
	    		s = strtok(NULL," ");
	    		i++;
		} while ((s != NULL) && (i < COLUMNS));
		columns = i - 1;		/* correction */
    	}

    	for (expn = 0; (!eof); expn++) {
        	row = 0;
 
        /* oldgens < line[0] indicates that this data is from a new 	*/
	/* Experiment.        						*/

        	while ( !(eof) && (oldgens <= line[0])) {

        /* if oldgens = line[0], then this line repeats the 		*/
        /* previous line (this sometimes happens after Restarts) 	*/
        /* The current line is ignored in this case.        		*/

            		if (oldgens < line[0]) { 	/* record the values */
                		for (col = 0; col < COLUMNS; col++) {
                    			average[row][col]  += line[col];
                    			variance[row][col] += line[col] * 
							      line[col];
                		}
                		row++;
            		}
            		oldgens = line[0];

	    		if (fgets(Str,BUFSZ,fp) == NULL) {
				eof = 1;
			}
	    		else {				/* read next line */
				i = 0;
				s = strtok(Str," ");
				do {
		    			sscanf(s,"%lf",&line[i]);
		    			s = strtok(NULL," ");
		    			i++;
				} while ((s != NULL) && (i < COLUMNS));

				if (i - 1 != columns) {
		   			colflag = 1;
				}
	    		}
        	} /* end while */
 
        	oldgens = -1.0;
        	if (expn == 0) {
	    		lines = row;
		}
        	else {
            		if (row < lines) {
                		lines  = row;
                		cutoff = 1;
            		}
        	}

    	} /* end for */

    	fclose(fp);
 
    	for (row = 0; row < REPROWS; row++) { /* compute mean and variance */
        	for (col = 0; col < COLUMNS; col++) {
            		tmp  = average[row][col] * average[row][col];
            		tmp /= expn;
            		variance[row][col] -= tmp;
            		if (expn > 1) {
                		variance[row][col] /= (expn-1);
			}
            		average[row][col] /= expn;
        	}
    	}

	if ((fp = fopen(AvgFil, "w")) != NULL) {
		/**** WrtDta(fp);  ****/
    		fprintf(fp, "# MEAN\n"); 	/* print the mean values */
    		fprintf(fp, "# Gens Trials Lost  Conv   Bias    Online    ");
    		fprintf(fp, "Offline      Best     Average    Worst");
		if (NbrMttRts > 0) {
			fprintf(fp, "       Min-MR     Max-MR     Avg-MR");
		}
		fprintf(fp, "\n");
    
    		for (i = 0; i < lines; i++) {
        		fprintf(fp, "%4.0f  %6.0f  %4.0f  ",
        			average[i][0],average[i][1],average[i][2]);
        		fprintf(fp, "%4.0f  %5.3f",
        			average[i][3],average[i][4]);
			for (j = 5; j < columns; j++) {
	    			fprintf(fp, "  %9.3e",average[i][j]);
			}
			fprintf(fp, "\n");
    		}
		fclose(fp);
	}
	else {
        	sprintf(Msg, "Report: can't open %s", AvgFil);
        	Error(Msg);
	}

   	if (expn > 1)  { 		/* print the variance */
		if ((fp = fopen(VarFil, "w")) != NULL) {
        		fprintf(fp, "# VARIANCE\n");
        		fprintf(fp, "# Gens Trials Lost  Conv   Bias");
			fprintf(fp, "    Online    ");
    			fprintf(fp, "Offline      Best     Average    Worst");
			if (NbrMttRts > 0) {
				fprintf(fp, "       Min-MR     Max-MR");
				fprintf(fp, "     Avg-MR");
			}
			fprintf(fp, "\n");
        		for (i=0; i<lines; i++) {
            			fprintf(fp, "%4d  %6d  %4d  ",
            				(int) average[i][0], 
					(int) variance[i][1], 
					(int) variance[i][2]);
            			fprintf(fp, "%4d  %5.3f", 
					(int) variance[i][3],
					      variance[i][4]);
	    			for (j = 5; j < columns; j++) {
	    				fprintf(fp, "  %9.3e",variance[i][j]);
				}
	    			fprintf(fp, "\n");
        		}
			fclose(fp);
    		}
		else {
        		sprintf(Msg, "Report: can't open %s", VarFil);
        		Error(Msg);
		}
	}

	if ((fp = fopen(Logfile, "a")) != NULL) {	/* print any warnings */
    		if (cutoff) { 			
        		fprintf(fp, "\nNOTE: Some Experiments produced more");
        		fprintf(fp, " data than others.\n");
        		fprintf(fp, "Extra data was not Reported above.\n");
    		}
    		if (colflag) {
			fprintf(fp, "\nNOTE: Some data rows in the outfile ");
			fprintf(fp, "included more columns than others.\n");
    		}
    		if (expn < Totalexperiments) {
        		fprintf(fp, "\nWARNING: Too little data for given ");
			fprintf(fp, "number of Experiments.\n");
    		}
    		if (expn > Totalexperiments) {
        		fprintf(fp, "\nWARNING: Too much data for given ");
			fprintf(fp, "number of Experiments.\n");
    		}
		fclose(fp);
	}
	else {
        	sprintf(Msg, "Report: can't open %s", Logfile);
        	Error(Msg);
	}

} /* end main */





	/*
	 *  Test for existence of file 'in.ext' directory . and ext/;
	 *  in the first case Fil is 'in.ext', in the second case 
	 *  'ext/in.ext'.
	 */

int
SrcFil(Fil, Ext, Sfx)
char			*Fil,
			*Ext,
			*Sfx;

{

	struct stat	 Sbuf;

	char		 Pth[NSZ],
			*strcpy(),
			*strcat();

	strcpy(Pth, Ext);		/* generate filename */
	strcat(Pth, _FILSEP);
	strcat(Pth, Sfx);

	if (stat(Pth, &Sbuf) == 0) {	/* in current directory */
		if (S_ISREG(Sbuf.st_mode)) {
			strcpy(Fil, Pth);
			return(0);
		}
		else {
			printf("SrcFil: File %s is not a regular one", Pth);
			return(-1);
		}
	}

	strcpy(Pth, Sfx);		/* generate new pathname */
	strcat(Pth, _PTHSEP);
	strcat(Pth, Ext);
	strcat(Pth, _FILSEP);
	strcat(Pth, Sfx);

	if (stat(Pth, &Sbuf) < 0) {	/* not in subdirectory */
		switch (errno) {
			case ENOENT:	/* does not exist */
				printf("SrcFil: File %s does not exist\n", Pth);
				return(-1);
				break;

			default:
				break;
		}
	}
	else {
		strcpy(Fil, Pth);
		return(0);
	}
	return(-1);

} /* end SrcFil */


/*** end of file ***/
