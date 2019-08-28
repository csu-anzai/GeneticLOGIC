
/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    (c) 1993
 *
 * --------------------------------------------------
 */

/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:       report.c
 *
 *  purpose:    generate a report summarizing the mean and variance
 *              of a number of performance measures of a GA run.
 *
 *  modified:   5 aug 86
 *              13 oct 93:
 *                  - Record data from files produced by
 *                    different processors.
 *                  - Report best-value statistics.
 *                  - Dynamic allocation of average & variance arrays.
 *                  - Dump average and variance tables to files.
 *              8 feb 94:
 *                  - Add -b option to report only best-value statistics
 *
 */


#include "global.h"


/**************************************************************************/
/*                     data definitions & constants                       */
/**************************************************************************/

/* data columns in report */
#define COLUMNS 9
/* columns in report media */
#define WIDTH 80

/* best value information structure, there's one of this for each process */
typedef struct {
	double BestValue;	/* best value   */
	int BestExp, BestGen;	/* when was the best found (exp, gen) */
	int BestTrial;
	int BestProc;
}      BESTSTATS;




/**************************************************************************/
/*                        function prototypes                             */
/**************************************************************************/
#ifdef __STDC__
void Input(int argc, char *argv[]);
void PrintTable(FILE * fp, double *ptr, int lines, char *sep);
void Usage(void);
void CenterStr(char *str);
void PrintBest(double *average, int lines, int expn, BESTSTATS *Overall);
int RecordValues(int proc, double *avg, double *var, BESTSTATS *Overall);
void ComputeAvg(double *average, int lines, int processes);
void ComputeVar(double *avg, double *var, int lines, int expn, int processes);
void ComputeGblAvg(double *gbl, double *avg, int lines, int expn);
extern void Error(char *str);
#endif

#ifndef __STDC__
void Input();
void PrintTable();
void Usage();
void CenterStr();
void PrintBest();
int RecordValues();
void ComputeAvg();
void ComputeVar();
void ComputeGblAvg();
extern void Error();
#endif


/**************************************************************************/
/*                            Global variables                            */
/**************************************************************************/
char Avgfile[40];		/* file for average table */
char Varfile[40];		/* file for variance table */
char expname[40];		/* experiment name (from command line) */
int Tableflag;			/* print the tables to files */
int Completeflag;		/* print the complete report to stdout */
int OnlyBestflag;		/* print only best value statistics */
int Moreflag;			/* too much data for given experiments */
int Lessflag;			/* too little data for given experiments */
int Experiments;
int nRows;



/*
 * Print program usage
 */
void Usage()
{
	printf("\n\tDGENESIS Report Utility\n\n");
	printf("Usage:\n");
	printf("\treport [options] experiment-name\n");
	printf("\nOptions:\n");
	printf("\t-b   print only best value statistics\n");
	printf("\t-c   print complete report (default)\n");
	printf("\t-h   display this help message\n");
	printf("\t-t   dump average and variance tables to files\n\n");
}



/*
 * Print a centered string to stdout
 */
void CenterStr(str)
char *str;
{
	register int i;
	int pos;

	pos = (WIDTH - strlen(str)) / 2;
	for (i = 0; i < pos; i++)
		putchar(' ');
	printf(str);
}



/*
 * Prints the table specified by 'ptr' and 'lines' to stream 'fp' using
 * 'separator' between columns.
 */
void PrintTable(fp, ptr, lines, separator)
FILE *fp;
double *ptr;
int lines;
char *separator;
{
	int i;

	for (i = 0; i < lines; i++) {
		fprintf(fp, "%4.0f%s", *ptr++, separator);
		fprintf(fp, "%6.0f%s", *ptr++, separator);
		fprintf(fp, "%4.0f%s", *ptr++, separator);
		fprintf(fp, "%4.0f%s", *ptr++, separator);
		fprintf(fp, "%5.3f%s", *ptr++, separator);
		fprintf(fp, "%9.3e%s", *ptr++, separator);
		fprintf(fp, "%9.3e%s", *ptr++, separator);
		fprintf(fp, "%9.3e%s", *ptr++, separator);
		fprintf(fp, "%9.3e\n", *ptr++);

	}
}



/*
 * Process the command line input
 */
void Input(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	int i, j;
	char Msg[80];

	if (argc < 2) {
		Usage();
		exit(1);
	}
	/* default values for the flags */
	Completeflag = 1;
	Tableflag = 0;
	OnlyBestflag = 0;

	/* set flag values */
	for (i = 1; i < argc; i++)
		if (argv[i][0] == '-')
			for (j = 0; j < strlen(argv[i]); j++)
				switch (argv[i][j]) {
				case 't':
					Tableflag = 1;
					break;
				case 'c':
					Completeflag = 1;
					break;
				case 'b':
					OnlyBestflag = 1;
					break;
				case 'h':
					Usage();
					exit(0);
				}
		else
			strcpy(expname, argv[i]);

	if (OnlyBestflag)
		Completeflag = 0;
	if (Completeflag)
		OnlyBestflag = 0;

	/* set up the file names */
	sprintf(Infile, "in.%s", expname);
	sprintf(Outfile, "out.%s", expname);
	sprintf(Avgfile, "avg.%s", expname);
	sprintf(Varfile, "var.%s", expname);

	/* read the parameters from the Infile */
	if ((fp = fopen(Infile, "r")) == NULL) {
		sprintf(Msg, "Report: can't open %s", Infile);
		Error(Msg);
	}
	fscanf(fp, IN_FORMAT, IN_VARS);
	fclose(fp);

        Maxflag = (strchr(Options, 'M') == NULL ? 0 : 1);
	Moreflag = Lessflag = 0;

	nRows = Totaltrials / Interval + 2;
}


/*
 * Print Best Value statistics
 */
void PrintBest(average, lines, expn, Overall)
double *average;
int lines, expn;
BESTSTATS *Overall;
{

	register int table, row;
	double avg_gen, avg_trial, var_gen, var_trial;
        double *ptr;	/* pointer in average */
	BESTSTATS *BestValues;


	BestValues = (BESTSTATS *) calloc((unsigned) expn, sizeof(BESTSTATS));
	if (BestValues == NULL)
		Error("Report: can't allocate memory for BestValues");



	/* find the best in each experiment */

	for (table = 0; table < expn; table++) {
        	ptr = average + table * nRows * COLUMNS;
                BestValues[table].BestGen = *(ptr);
                BestValues[table].BestTrial = *(ptr + 1);
        	BestValues[table].BestValue = *(ptr + 7);

        	for (row=1; row<lines; row++){
                	ptr += COLUMNS;
                        if ( *(ptr + 7) < BestValues[table].BestValue){
				BestValues[table].BestGen = *(ptr);
	                	BestValues[table].BestTrial = *(ptr + 1);
        			BestValues[table].BestValue = *(ptr + 7);
                        }
                }
        }


        printf("\n\nBEST VALUE\n\n");
	printf("EXP\tGEN\tTRIAL\tVALUE\n");

	/* compute the average trials and gens needed to reach the */
	/* optimimum in each experiment                            */
        avg_gen = avg_trial = 0;
        for (table = 0; table < expn; table++) {
		avg_gen += BestValues[table].BestGen;
		avg_trial += BestValues[table].BestTrial;
		printf("%d\t", table+1);
		printf("%d\t", BestValues[table].BestGen);
		printf("%d\t", BestValues[table].BestTrial);
		printf("%lf\n", BestValues[table].BestValue);
	}

	avg_gen /= expn;
	avg_trial /= expn;

	printf("\nAVG:\t%0.2lf\t%0.2lf\n", avg_gen, avg_trial);

	/* compute the variance of trials and gens needed to reach the */
	/* optimimum in each experiment                                */
	if (expn > 1){
	var_gen = var_trial = 0;
	for (table = 0; table<expn; table++){
		var_gen += (BestValues[table].BestGen - avg_gen) * (BestValues[table].BestGen - avg_gen);
		var_trial += (BestValues[table].BestTrial - avg_trial) * (BestValues[table].BestTrial - avg_trial);
	}
	var_gen /= (expn - 1);
	var_trial /= (expn - 1);
	printf("VAR:\t%0.2lf\t%0.2lf\n\n", var_gen, var_trial);
	}


	printf("\tThe best overall value is: %9.3e\n", Overall->BestValue);
	printf("\tFirst found by");
	printf("\tProcess: %d", Overall->BestProc);
	printf("\tExperiment: %d", Overall->BestExp);
	printf("\tGeneration: %d\n", Overall->BestGen);

        free(BestValues);
}



/*
 * Records values from the given Outfile of a particular process in the
 * average and variance tables
 */
int RecordValues(proc, avg, var, Overall)
int proc;
double *avg, *var;
BESTSTATS *Overall;
{
	double line[COLUMNS];
	double oldgens;		/* previous Generation count */
	double *average, *variance;
	register int row, col;
	int lines;		/* number of lines in Report */
	int expn;		/* number of Experiments */
	int eof;		/* true when Outfile is exhausted */
	int cutoff;		/* was data truncated? see below */
	char Msg[WIDTH];	/* Message string */
	FILE *fp;


	/* For the purpose of computing the means and the variances, */
	/* the number of lines is taken to be the minimum number of */
	/* lines produced by any Experiment.  If any data is        */
	/* discarded, the flag cutoff is set, so that a warning     */
	/* may be printed.                                          */

	cutoff = 0;
	lines = 0;

	/* get the Outfile */
	sprintf(Outfile, "%dout.%s", proc, expname);

	if ((fp = fopen(Outfile, "r")) == NULL) {
		sprintf(Msg, "Report: can't open %s", Outfile);
		Error(Msg);
		return -1;
	}
	/* read in a line */
	if (fscanf(fp, LINE_FIN, LINE_VIN) == EOF) {
		sprintf(Msg, "Report: unexpected EOF on %s", Outfile);
		Error(Msg);
	}
	if (proc == 0) {
		Overall->BestValue = line[7];
		Overall->BestExp = 1;
		Overall->BestGen = (int) line[0];
		Overall->BestTrial = (int) line[1];
	}
			

	oldgens = -1.0;
	row = 0;
	eof = 0;
	for (expn = 0; (!eof); expn++) {

                average = avg + (expn*nRows*COLUMNS);
                variance = var;

		/* oldgens > line[0] indicates that this */
		/* data is from a new Experiment.        */
		while (!(eof) && (oldgens <= line[0])) {
			/* if oldgens = line[0], then this line repeats the      */
			/* previous line (this sometimes happens after Restarts) */
			/* The current line is ignored in this case.             */
			if (oldgens < line[0]) {
				/* record the values */
				for (col = 0; col < COLUMNS; col++) {
					*average += line[col];
					average++;
					*variance += (line[col] * line[col]);
					variance++;
				}

				if (BETTER(line[7], Overall->BestValue) ||
				    (line[7] == Overall->BestValue) && Overall->BestTrial > line[1]) {
					Overall->BestValue = line[7];
					Overall->BestExp = expn + 1;
					Overall->BestGen = (int) line[0];
					Overall->BestTrial = (int) line[1];
					Overall->BestProc = proc;
				}
				row++;
			}
			oldgens = line[0];

			/* read in a line */
			fscanf(fp, LINE_FIN, LINE_VIN);
			eof = feof(fp);
		}		/* one experiment */

		oldgens = -1.0;
		if (expn == 0)
			lines = row;
		else {
			if (row < lines) {
				lines = row;
				cutoff = 1;
			}
		}
		average = avg;
		variance = var;
	}

	fclose(fp);

	if (proc == 0)
		Experiments = expn;

	if (expn > Totalexperiments)
		Moreflag = 1;

	if (expn < Totalexperiments) {
		Experiments = expn;
		Lessflag = 1;
	}
	if (cutoff)
		return -lines;
	else
		return lines;
}




/*
 * compute the average for the given experiment table
 */
void ComputeAvg(average, lines, processes)
double *average;
int lines, processes;
{
	register int col, row;

	for (row = 0; row < lines; row++)
		for (col = 0; col < COLUMNS; col++) {
			*average /= processes;
			average++;
		}
}



/*
 * computes the global average from the experiment average tables
 */
void ComputeGblAvg(global, average, lines, expn)
double *global, *average;
int lines, expn;
{
	register int col, row, table;
        double *ptr;	/* pointer in global average table */
        double *avg;	/* pointer in experiment tables	*/

        for (table=0; table<expn; table++){
        	ptr = global;
		avg = average + table * nRows * COLUMNS;
		for (row = 0; row < lines; row++){
			for (col = 0; col < COLUMNS; col++){
				*ptr += *avg;
                                ptr++;
                                avg++;
                        }
                }
        }

        ptr = global;
        for (row = 0; row < lines; row++)
		for (col = 0; col < COLUMNS; col++){
                	*ptr /= expn;
                        ptr++;
                }


}


/*
 * computes the variance table
 */
void ComputeVar(average, variance, lines, expn, processes)
double *average, *variance;
int lines, expn, processes;
{
	register int col, row;
	double tmp;

        if (expn > 1 || processes > 1) {
		for (row = 0; row < lines; row++){
			for (col = 0; col < COLUMNS; col++){
				tmp = (*average) * (*average);
				tmp *= (expn * processes);
				*variance -= tmp;
				*variance /= ((expn * processes) - 1);
				variance++;
                                average++;
			}
                }
        }
}



/**************************************************************************/
/*                           The main program.                            */
/**************************************************************************/
main(argc, argv)
int argc;
char *argv[];
{
	double *average, *avg;
	double *variance, *var;
        double *globalavg;
	int i;
	int lines;
	int proc;
	int cutoff;
	char Msg[WIDTH];	/* Message string */
	BESTSTATS BestOverall;
	FILE *fp;


	/* process command line input, read parameters from Infile */
	Input(argc, argv);


	/* allocate dynamic memory for average and variance tables */

	avg = average = (double *) calloc((unsigned) Totalexperiments, nRows * COLUMNS * sizeof(double));
	if (average == NULL)
		Error("Report: can't allocate memory for average");

        globalavg = (double *) calloc((unsigned) 1, nRows * COLUMNS * sizeof(double));
	if (globalavg == NULL)
		Error("Report: can't allocate memory for globalavg");

	var = variance = (double *) calloc((unsigned) 1, nRows * COLUMNS * sizeof(double));
	if (variance == NULL)
		Error("Report: can't allocate memory for variance");

	/* record data from files generated by all processes */
	average = avg;
	variance = var;
	lines = RecordValues(0, average, variance, &BestOverall);
	for (proc = 1; proc < Processes; proc++) {
		int ln;
		ln = RecordValues(proc, average, variance, &BestOverall);
		if (ln < 0) {
			cutoff = 1;
			ln = -ln;
		}
		if (ln < lines)
			lines = ln;
	}

	/* compute the mean and variance */
        for (i=0; i<Experiments; i++){
        	average = avg + i*nRows*COLUMNS;
		ComputeAvg(average, lines, Processes);
        }
        average = avg;
        variance = var;
        ComputeGblAvg(globalavg, average, lines, Experiments);
        ComputeVar(globalavg, variance, lines, Experiments, Processes);

	/* print the report */
	average = avg;
	variance = var;
	if (Completeflag) {
		printf("\n\n");
		sprintf(Msg, "Report for Experiment %s", expname);
		CenterStr(Msg);
		printf("\n\n");
		printf(OUT_FORMAT, OUT_VARS);

		/* print the mean values */
		printf("\nMEAN\n");
		printf("Gens  Trials  Lost  ");
		printf("Conv   Bias    Online    ");
		printf("Offline      Best     Average\n");
		PrintTable(stdout, globalavg, lines, "  ");

		/* print the variance */
		if (Totalexperiments > 1 || Processes > 1) {
			printf("\nVARIANCE\n");
			printf("Gens  Trials  Lost  ");
			printf("Conv   Bias    Online    ");
			printf("Offline      Best     Average\n");
			PrintTable(stdout, variance, lines, "  ");
		}
		PrintBest(average, lines, Experiments, &BestOverall);

		printf("\n\n");
		CenterStr("*** End of Report ***");
		printf("\n\n");
	}
	if (OnlyBestflag)
		PrintBest(average, lines, Experiments, &BestOverall);

	/* dump tables to files */
	if (Tableflag) {
		fp = fopen(Avgfile, "w");
		PrintTable(fp, globalavg, lines, "\t");
		fclose(fp);
		fp = fopen(Varfile, "w");
		PrintTable(fp, variance, lines, "\t");
		fclose(fp);
	}
	/* print any warnings */
	if (cutoff) {
		printf("\nNOTE: Some Experiments produced more");
		printf(" data than others.\n");
		printf("       Extra data was not Reported above.\n");
	}
	if (Lessflag)
		printf("\nWARNING: Too little data for given number of Experiments.\n");
	if (Moreflag)
		printf("\nWARNING: Too much data for given number of Experiments.\n");


	free((char *) average);
	free((char *) globalavg);
	free((char *) variance);

	return 0;
}

/*** end of file ***/
