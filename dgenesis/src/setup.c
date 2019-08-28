
/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 */
/*
 *   GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *   This program may be freely copied for educational
 *   and research purposes.  All other rights reserved.
 *
 *   file:       setup.c
 *
 *   purpose:    creates input files for DGENESIS: in, template & proc
 *               Default values for input parameters are provided when
 *		 the user gives a null response to a prompt.
 *
 *   modified:   feb. 1986
 *               10 sep 90: handle floating point option
 *               13 sep 93: get default values from previous files (ECP)
 */



#include "global.h"
#include "format.h"


/**************************************************************************/
/*                           Function Prototypes                          */
/**************************************************************************/
#ifdef __STDC__
char *ReadLine(FILE *, char *, int);
void Error(char *);
int ilog2(unsigned long);
void Input(int argc, char *argv[]);
void PromptPops(void);
int PromptGenes(void);
#endif
#ifndef __STDC__
char *ReadLine();
int ilog2();
void Input();
void PromptPops();
int PromptGenes();
#endif

char infile[40];
char templatefile[40];
char procfile[40];


/*
 * Process command line arguments
 */
void Input(argc, argv)
int argc;
char *argv[];
{

	char exp[40];

	if (argc < 2) {
		exp[0] = 0;
		INPUTSTR("\nExperiment name", exp);
	} else
		sprintf(exp, argv[1]);

	/* set file names */
	if (strlen(exp) == 0) {
		sprintf(infile, "in");
		sprintf(templatefile, "template");
		sprintf(procfile, "proc");
	} else {
		sprintf(infile, "in.%s", exp);
		sprintf(templatefile, "template.%s", exp);
		sprintf(procfile, "proc.%s", exp);
	}
}


/*
 * Prompts population sizes for each process. Creates procfile.
 */
void PromptPops()
{
	char prompt[40];
	register int i, j;
	int pop = 100;
	int repetition;
	int *Populations;	/* population size of each process  */
	FILE *fp;
	char *status;
	char line[40];

	if (Processes > 1) {
		Populations = (int *) calloc((unsigned) Processes, sizeof(int));
		if (Populations == NULL)
			Error("setup: can't allocate memory for Populations");

		/* default population size = 100 */
		for (i = 0; i < Processes; i++)
			Populations[i] = 100;

		/* open procfile and get default popsize */
		if ((fp = fopen(procfile, "r")) != NULL) {
			status = ReadLine(fp, line, 40);

			while (status != NULL) {
				sscanf(line, "%*d %d", &Populations[i]);
				Totalpop += Populations[i];
				status = ReadLine(fp, line, 40);
				i++;
			}
			fclose(fp);
		}
		/* ask populations for each process */

		Totalpop = 0;

		for (i = 0; i < Processes;) {
			sprintf(prompt, "Population for process %d", i);
			INPUT(prompt, "%d", pop);
			repetition = Processes - i;
			INPUT("repetition: ", "%d", repetition);
			printf("\n");

			for (j = 0; j < repetition && i < Processes; j++, i++) {
				Populations[i] = pop;
				Totalpop += pop;
			}
		}

		/* write the procfile */
		if ((fp = fopen(procfile, "w")) != NULL) {
			fprintf(fp, "#\n# file: %s\n#\n", procfile);

			for (i = 0; i < Processes; i++)
				fprintf(fp, "%d\t%d\n", i, Populations[i]);

			fclose(fp);
		} else
			printf("Can't create %s\n", procfile);

		free((void *) Populations);

	} else			/* only one process */
		INPUT("Total Population", "%d", Totalpop);

}				/* PromptPops() */




/*
 * Promps for floating point representation. Creates templatefile
 */
int PromptGenes()
{
	register int i, j;	/* loop control	 */
	double min, max;
	unsigned long values;
	int genes = 3;
	int temp;
	int repetition;
	int bitlength;
	unsigned long verify;
	int ok;
	char format[16];
	FILE *fp;
	GENESTRUCT *Genes = NULL;


	i = 0;
	/* open template file to get defaults */
	if ((fp = fopen(templatefile, "r")) != NULL) {
		fscanf(fp, "genes : %d", &genes);

		if ((Genes = (GENESTRUCT *) calloc((unsigned) genes, sizeof(GENESTRUCT))) == NULL)
			Error("setup can't allocate memory for Genes");

		for (i = 0; i < genes; i++) {
			fscanf(fp, " gene %*d min: %lf", &Genes[i].min);
			fscanf(fp, " max: %lf", &Genes[i].max);
			fscanf(fp, " values: %ld", &Genes[i].values);
			fscanf(fp, " format: %s", Genes[i].format);
		}
		fclose(fp);

	}
	temp = genes;
	INPUT("Number of genes: ", "%d", temp);
	if (temp != genes) {
		if ((Genes = (GENESTRUCT *) realloc(Genes, sizeof(GENESTRUCT) * temp)) == NULL)
			Error("setup can't allocate memory for Genes");
	} else if (i == 0)
		if ((Genes = (GENESTRUCT *) calloc((unsigned) genes, sizeof(GENESTRUCT) * temp)) == NULL)
			Error("setup can't allocate memory for Genes");


	genes = temp;

	/* set default values */
	for (; i < genes; i++) {
		Genes[i].min = -5.11;
		Genes[i].max = 5.12;
		Genes[i].values = 1024;
		strcpy(Genes[i].format, "%7.3f");
	}


	for (i = 0; i < genes;) {
		min = Genes[i].min;
		max = Genes[i].max;
		values = Genes[i].values;
		strcpy(format, Genes[i].format);

		/* prompt for values */
		printf("gene %d\n", i);
		INPUT("min: ", "%lf", min);
		INPUT("max: ", "%lf", max);
		ok = 0;
		while (!ok) {
			INPUT("values (must be a power of 2): ", "%lu", values);
			verify = 1L << ilog2(values);
			ok = verify == values;
			if (!ok)
				printf("bad choice for values\n");
		}
		strcpy(format, "%7.2f");
		INPUTSTR("format string: ", format);
		repetition = genes - i;
		INPUT("repetition: ", "%d", repetition);
		printf("\n");

		for (j = 0; j < repetition && i < genes; j++, i++) {
			Genes[i].min = min;
			Genes[i].max = max;
			Genes[i].values = values;
			strcpy(Genes[i].format, format);
		}

	}

	/* write template file */
	bitlength = 0;
	fp = fopen(templatefile, "w");
	fprintf(fp, "genes: %d\n\n", genes);
	printf("\n");
	for (i = 0; i < genes; i++) {
		fprintf(fp, "gene %d\n", i);
		fprintf(fp, "min: %g\n", Genes[i].min);
		fprintf(fp, "max: %g\n", Genes[i].max);
		fprintf(fp, "values: %lu\n", Genes[i].values);
		fprintf(fp, "format: %s\n", Genes[i].format);
		fprintf(fp, "\n");
		bitlength += ilog2(Genes[i].values);
	}

	fclose(fp);
	free((void *) Genes);

	return bitlength;
}				/* PromptGenes() */




main(argc, argv)
int argc;
char *argv[];
{

	char Msg[40];
	char cmd[80];
	int interpret;		/* using floating point? */
	FILE *fp;


	printf("\n\n\tDGENESIS Setup Utility");
	printf("\n\n\nDefault values are displayed in brackets\n\n");

	/* handle input parameters */
	Input(argc, argv);

	/* set default defaults */
	Processes = 1;
	Totalexperiments = 1;
	Totaltrials = 5000;
	Totalpop = 500;
	Length = 30;
	C_rate = 0.6;
	M_rate = 0.001;
	Gapsize = 1.0;
	Windowsize = 5;
	Interval = 100;
	Savesize = 10;
	Maxspin = 2;
	Dump_freq = 0;
	Num_dumps = 0;
	strcpy(Options, "cefglS");
	OrigSeed = 123456789L;
	Rank_min = 0.75;

	/* open infile and get defaults */
	if ((fp = fopen(infile, "r")) != NULL) {
		if (fscanf(fp, IN_FORMAT, IN_VARS) < 0)
			printf("Can't read defaults\n");
		fclose(fp);
	}
	/* ask number of processes and population sizes for each */

	INPUT("Processes", "%d", Processes);
	PromptPops();


	INPUT("Experiments", "%d", Totalexperiments);
	INPUT("Total Trials", "%d", Totaltrials);

	/* handle floating point representation */
	if (strchr(Options, 'f') == NULL)
		strcpy(Msg, "n");
	else
		strcpy(Msg, "y");
	INPUTSTR("Floating point representation", Msg);
	if (Msg[0] != 'n')
		interpret = 1;
	else
		interpret = 0;

	if (interpret)
		Length = PromptGenes();
	else
		INPUT("Structure Length", "%d", Length);

	INPUT("Crossover Rate", "%f", C_rate);
	INPUT("Mutation Rate", "%f", M_rate);
	INPUT("Generation Gap", "%f", Gapsize);
	INPUT("Scaling Window", "%d", Windowsize);
	INPUT("Report Interval", "%d", Interval);
	INPUT("Structures Saved", "%d", Savesize);
	INPUT("Max Gens w/o Eval", "%d", Maxspin);
	INPUT("Dump Interval", "%d", Dump_freq);
	INPUT("Dumps Saved", "%d", Num_dumps);

	if (interpret)
		strcpy(Options, "cefglS");
	else
		strcpy(Options, "ceglS");

	INPUTSTR("Options", Options);
	INPUT("Random Seed", "%lu", OrigSeed);
	INPUT("Rank Min", "%lf", Rank_min);


	/* write user input to infile */
	if ((fp = fopen(infile, "w")) == NULL) {
		printf("can't open %s\n", infile);
		printf("Setup aborted.\n");
		exit(1);
	}
	fprintf(fp, OUT_FORMAT, OUT_VARS);
	fclose(fp);


	/* echo infile */
	printf("\n\nContents of file %s:\n\n", infile);
#if TURBOC
	sprintf(cmd, "type %s", infile);
#else
	sprintf(cmd, "cat %s", infile);
#endif
	system(cmd);

	printf("\n\tSetup Done !!\n\n");
	return 0;
}

/*** end of file ***/
