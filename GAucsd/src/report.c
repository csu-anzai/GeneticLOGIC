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
 *  file:	report.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	sep 1981
 *
 *  purpose:	generate a report summarizing the mean
 *		and variance of a number of performance
 *		measures of a GA run.
 *
 *  modified:	5 aug 86
 */

#define   UTILITY
#include "global.h"

extern void Error();

#define COLUMNS 9

double *avg, *a, *var, *v;
double line[COLUMNS];


main(argc, argv)
int argc;
char *argv[];
{
	register int row, col, i;
	double oldgens;		/* previous Generation count */
	double tmp;
	int cutoff = 0;		/* was data truncated? see below */
	int droplast = 0;	/* was last run incomplete? */
	char Outfile[40];	/* output file produced by Genetic alg */
	char Infile[40];	/* Input file for Genetic alg. */
	char msg[80];   	/* message string */
	char *arg1 = 0;  	/* points to file suffix, if any */
	int lines;		/* number of lines in Report */
	int eof;		/* true when Outfile is exhausted */
	int expn;		/* number of runs */

	FILE *fp, *fopen();

	/* set up the file names */
	if (argc >= 2) arg1 = argv[1];
	name_file(Infile,  "in",  arg1);   
	name_file(Outfile, "out", arg1);   

	/* read the parameters from the Infile */
	if ((fp = fopen(Infile, "r")) == NULL)
	{
		sprintf(msg, "Report: can't open %s", Infile);
		Error(msg);
	}
	fscanf(fp, FORMAT, IN_VARS);
	printf(FORMAT, OUT_VARS);
	fscanf(fp, FORM_2, IN_2);
	printf(FORM_2, OUT_2);
	while (fgets(msg, 80, fp)) fputs(msg, stdout);
	fclose(fp);

	/* For the purpose of computing the means and the variances, */
	/* the number of lines is taken to be the minimum number of */
	/* lines produced by any run.  If any data is        */
	/* discarded, the flag cutoff is set, so that a warning     */
	/* may be printed.                                          */

	row = Totaltrials/Interval + 2;
	a = avg = (double *) calloc(row, COLUMNS * sizeof(double));
	v = var = (double *) calloc(row, COLUMNS * sizeof(double));

	/* get the Outfile */
	if ((fp = fopen(Outfile, "r")) == NULL)
	{
		sprintf(msg, "Report: can't open %s", Outfile);
		Error(msg);
	}

	lines = 0;
	oldgens = -1.0;

	/* read in a line */
	if (fscanf(fp, LINE_FIN, LINE_VIN) == EOF)
	{
		sprintf(msg, "Report: unexpected EOF on %s", Outfile);
		Error(msg);
	}

	eof = 0;
	for (expn = 0; (!eof); expn++)
	{
		row = 0;

		/* oldgens > line[0] indicates the start of a new run. */
		while ( !(eof) && (oldgens <= line[0]))
		{
			/* if oldgens = line[0], then this line repeats the */
			/* previous line (this sometimes happens after Restarts) */
			/* The current line is ignored in this case.        */
			if (oldgens < line[0])
			{
				/* record the values */
				for (col = 0; col < COLUMNS; col++)
				{
					*a++ += line[col];
					*v++ += line[col] * line[col];
				}
				row++;
			}
			oldgens = line[0];

			/* read in a line */
			eof = (fscanf(fp, LINE_FIN, LINE_VIN) == EOF);
		}

		oldgens = -1.0;
		if (expn)
		{
			if (row > lines) cutoff = 1;
			else if (row < lines)
			{
				lines = row;
				if (eof) droplast = 1;
				else cutoff = 1;
			}
		}
		else lines = row;

		a = avg;
		v = var;
	}
	fclose(fp);

	/* compute the mean and variance */
	for (row = 0; row < lines; row++)
	{
		for (col = 0; col < COLUMNS; col++)
		{
			if (expn > 1)
			{
				tmp = *a * *a;
				tmp /= expn;
				*v -= tmp;
				*v++ /= expn - 1;
			}
			*a++ /= expn;
		}
	}

	/* print the mean values */
	printf("\nMEAN\n");
	printf("Gens Trials Lost ");
	printf("Conv  Bias     Online      ");
	printf("Offline       Best        Average\n");
	a = avg;
	for (i = 0; i < lines; i++)
	{
		printf("%4.0lf ",    *a++);
		printf("%6.0lf ",    *a++);
		printf("%3.0lf ",    *a++);
		printf("%4.0lf  ",   *a++);
		printf("%5.3lf ",    *a++);
		printf("% 11.5le ",  *a++);
		printf("% 11.5le ",  *a++);
		printf("% 11.5le ",  *a++);
		printf("% 11.5le\n", *a++);
	}

	/* print the variance */
	if (expn > 1)
	{
		printf("\nVARIANCE\n");
		printf("Gens Trials Lost ");
		printf("Conv  Bias     Online      ");
		printf("Offline       Best        Average\n");
		v = var;
		for (i = 0; i < lines; i++)
		{
			printf("%4d ",  (int) *v++);
			printf("%6d ",  (int) *v++);
			printf("%3d ",  (int) *v++);
			printf("%4d  ", (int) *v++);
			printf("%5.3lf ",     *v++);
			printf("% 11.5le ",   *v++);
			printf("% 11.5le ",   *v++);
			printf("% 11.5le ",   *v++);
			printf("% 11.5le\n",  *v++);
		}
	}

	/* print any warnings */
	if (expn != Totalexperiments)
	{
		printf("\nNOTE: found data for %d runs, ", expn);
		printf("not %d as specified above.", Totalexperiments);
	}
	if (droplast)
	{
		printf("\nNOTE: incomplete last run caused ");
		printf("truncation of data; consider removing\n");
		printf("      the offending lines from file ");
		printf("'%s' and re-running report.", Outfile);
	}
	if (cutoff)
	{
		printf("\nWARNING: ignored extraneous data reported ");
		printf("by some runs;\n         please check ");
		printf("validity of data file '%s'.", Outfile);
	}
	printf("\n");
}

/*** end of file ***/

