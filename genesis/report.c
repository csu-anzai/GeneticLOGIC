
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	report.c
 *
 *  purpose:	generate a report summarizing the mean
 *		and variance of a number of performance
 *		measures of a GA run.
 *
 *  modified:	5 aug 86
 */

#include "global.h"

extern void Error();

#define ROWS 200
#define COLUMNS 9

double average[ROWS][COLUMNS];
double variance[ROWS][COLUMNS];
double line[COLUMNS];


main(argc, argv)
int argc;
char *argv[];
{
	register int row, col, i;
	double oldgens;		/* previous Generation count */
	double tmp;
	int cutoff;		/* was data truncated? see below */
	char Outfile[15];	/* output file produced by Genetic alg */
	char Infile[15];	/* Input file for Genetic alg. */
	char errmsg[40];	/* Error message string */
	int lines;		/* number of lines in Report */
	int eof;		/* true when Outfile is exhausted */
	int expn;		/* number of Experiments */

	FILE *fp, *fopen();

	/* For the purpose of computing the means and the variances, */
	/* the number of lines is taken to be the minimum number of */
	/* lines produced by any Experiment.  If any data is        */
	/* discarded, the flag cutoff is set, so that a warning     */
	/* may be printed.                                          */

	cutoff = 0;

	for (row = 0; row < ROWS; row++)
		for (col = 0; col < COLUMNS; col++)
		{
			average[row][col] = 0.0;
			variance[row][col] = 0.0;
		}

	/* set up the file names */
	if (argc < 2)
	{
		strcpy(Infile, "in");
		strcpy(Outfile, "out");
	}
	else
	{
		sprintf(Infile, "in.%s", argv[1]);
		sprintf(Outfile, "out.%s", argv[1]);
	}

	/* read the parameters from the Infile */
	if ((fp = fopen(Infile, "r")) == NULL)
	{
		sprintf(errmsg, "Report: can't open %s", Infile);
		Error(errmsg);
	}
	fscanf(fp, IN_FORMAT, IN_VARS);
	fclose(fp);

	/* get the Outfile */
	if ((fp = fopen(Outfile, "r")) == NULL)
	{
		sprintf(errmsg, "Report: can't open %s", Outfile);
		Error(errmsg);
	}

	lines = 0;
	oldgens = -1.0;

	/* read in a line */
	if (fscanf(fp, LINE_FIN, LINE_VIN) == EOF)
	{
		sprintf(errmsg, "Report: unexpected EOF on %s", Outfile);
		Error(errmsg);
	}

	eof = 0;
	for (expn = 0; (!eof); expn++)
	{
		row = 0;

		/* oldgens > line[0] indicates that this */
		/* data is from a new Experiment.        */
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
					average[row][col] += line[col];
					variance[row][col] += line[col] * line[col];
				}
				row++;
			}
			oldgens = line[0];

			/* read in a line */
			eof = (fscanf(fp, LINE_FIN, LINE_VIN) == EOF);
		}

		oldgens = -1.0;
		if (expn == 0) lines = row;
		else
		{
			if (row < lines)
			{
				lines = row;
				cutoff = 1;
			}
		}
	}
	fclose(fp);

	/* compute the mean and variance */
	for (row = 0; row < ROWS; row++)
	{
		for (col = 0; col < COLUMNS; col++)
		{
			tmp = average[row][col] * average[row][col];
			tmp /= expn;
			variance[row][col] -= tmp;
			if (expn > 1)
				variance[row][col] /= (expn-1);
			average[row][col] /= expn;
		}
	}

	/* print the table */
	printf(OUT_FORMAT, OUT_VARS);

	/* print the mean values */
	printf("\nMEAN\n");
	printf("Gens  Trials  Lost  ");
	printf("Conv   Bias    Online    ");
	printf("Offline      Best     Average\n");
	for (i = 0; i < lines; i++)
	{
		printf("%4.0f  %6.0f  %4.0f  ",
		average[i][0],average[i][1],average[i][2]);
		printf("%4.0f  %5.3f  %9.3e  ",
		average[i][3],average[i][4],average[i][5]);
		printf("%9.3e  %9.3e  %9.3e\n",
		average[i][6],average[i][7],average[i][8]);
	}

	/* print the variance */
	if (expn > 1)
	{
		printf("\nVARIANCE\n");
		printf("Gens  Trials  Lost  ");
		printf("Conv   Bias    Online    ");
		printf("Offline      Best     Average\n");
		for (i=0; i<lines; i++)
		{
			printf("%4d  %6d  %4d  ",
			(int) variance[i][0], (int) variance[i][1],
			(int) variance[i][2]);
			printf("%4d  %5.3f  %9.3e  ",
			(int) variance[i][3],variance[i][4],variance[i][5]);
			printf("%9.3e  %9.3e  %9.3e\n",
			variance[i][6],variance[i][7],variance[i][8]);
		}
	}

	/* print any warnings */

	if (cutoff)
	{
		printf("\nNOTE: Some Experiments produced more");
		printf(" data than others.\n");
		printf("       Extra data was not Reported above.\n");
	}

	if (expn < Totalexperiments)
	{
		printf("\nWARNING: Too little data for given number of ");
		printf("Experiments.\n");
	}
	if (expn > Totalexperiments)
	{
		printf("\nWARNING: Too much data for given number of Experiments.\n");
	}

}

/*** end of file ***/
