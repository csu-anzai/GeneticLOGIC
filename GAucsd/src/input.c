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
 *  file:	input.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	Set up filenames and read the input file, and
 *		initialize variables for this run.
 *
 *		See init.c for the initialization of variables for each run.
 *
 *  modified:	26 jun 86
 *              23 may 90   added application-specific arguments  - Nici.
 */

#define   EXTERN
#include "global.h"

extern void Setflag();

Input(argc,argv)
int argc;
char *argv[];
{
	FILE *fopen(), *fp;

	int i;
	char msg[80];
	long clock;
	long time();
	char *ctime();
	char *arg1 = 0;


	/* set up file names */

	if (argc >= 2) arg1 = argv[1];

	name_file(Infile,    "in",  arg1);   
	name_file(Outfile,   "out", arg1); 
	name_file(Ckptfile,  "cpt", arg1); 
	name_file(Minfile,   "min", arg1); 
	name_file(Logfile,   "log", arg1);
	name_file(Initfile,  "ini", arg1);
	name_file(Schemafile,"sma", arg1);
	name_file(DPEfile,   "dpe", arg1);

	strcpy(Bestfile, Minfile);


	/* read in the Input parameters from the in file */

	if ((fp = fopen(Infile, "r")) == NULL)
	{
		sprintf(msg, "Input: can't open %s", Infile);
		Error(msg);
	}
	if (fscanf(fp, " This is %s for %s ", msg, msg)) fgets(msg, 80, fp);
	if (fscanf(fp, FORMAT, IN_VARS) < N_FORM)
	{
		sprintf(msg, "Input: garbled input file %s", Infile);
		Error(msg);
	}
	fscanf(fp, FORM_2, IN_2);

	if (!Interval) Interval = Totaltrials;
	if (!Maxspin) Maxspin = INT_MAX;
	if (!Maxconv) Maxconv = Length + 1;
	Few = (int)((1.0 - Convlev)*Popsize);
	Experiment = 1;

	/* read application-specific arguments, if any */
	GArgc = 0;
	if (fgets(msg, 80, fp)) if (!strcmp(msg, GARGSEP))
		while (fgets(msg, 80, fp))
		{
			i = strlen(msg);
			if (msg[i - 1] == '\n') msg[i - 1] = '\0';
			if (!*msg) break;
			if (GArgc >= MAXGARG)
			{
				sprintf(msg,
					"more than %d application-specific arguments", MAXGARG);
				Error(msg);
			}
			GArgv[GArgc] = (char *) malloc(strlen(msg) + 1);
			strcpy(GArgv[GArgc++], msg);
		}
	fclose(fp);

	/* Bytes is the size of each packed chromosome */
	Full = Length / CHAR_BIT;
	Slop = Length % CHAR_BIT;
	Bytes = Full;
	if (Slop) Bytes++;

	/* set up all the bit masks */
	for (i = 0; i < CHAR_BIT; i++)
	{
		 Pre[i] = PRE(i);
		Post[i] = POST(i);
		 Bit[i] = BIT(i);
	}

	/* allocate storage for variable sized structures */

	/* population arrays */
	Old = (STRUCTURE *) calloc((unsigned) Popsize, sizeof(STRUCTURE));
	New = (STRUCTURE *) calloc((unsigned) Popsize, sizeof(STRUCTURE));

	/* the following request avoids excessive swapping  */
	/* in the following loop                            */
	Buff = malloc((unsigned) 2*Popsize*(Bytes+4));
	free(Buff);

	for (i = 0; i < Popsize; i++)
	{
		Old[i].Gene = malloc((unsigned) Bytes);
		New[i].Gene = malloc((unsigned) Bytes);
	}

	/* used to compute moving value for Worst */
	if (Windowsize > 0)
		Window = (double *) calloc((unsigned) Windowsize, sizeof(double));

	/* used to save DPE history */
	if (GAgenes) DPEhist = (double *) calloc(2*GAgenes, sizeof(double));

	/* used to re-initialize DPEscale and DPEbasis for each run */
	if (Totalexperiments > 1)
	{
		Scale = (double *) calloc(GAgenes, sizeof(double));
		Basis = (double *) calloc(GAgenes, sizeof(double));
		for (i = 0; i < GAgenes; i++)
		{
			Scale[i] = GAfact[i];
			Basis[i] = GAbase[i];
		}
	}

	/* used to save best structures */
	if (Savesize) Bestset = (BESTSTRUCT *)
		calloc((unsigned) Savesize, sizeof(BESTSTRUCT));
	Phenesize = 8*GAgenes + Length + 85;

	/* the following request avoids excessive swapping  */
	/* in the following loop                            */
	Buff = malloc((unsigned) Savesize*(Bytes+Phenesize+8));
	free(Buff);

	for (i = 0; i < Savesize; i++)
	{
		Bestset[i].Gene = malloc((unsigned) Bytes);
		Bestset[i].Phene = malloc((unsigned) Phenesize);
		Bestset[i].Phene++;
	}
	Phenesize -= 2; 	/* room for first & last char */

	/* buffer for accessing alleles */
	Buff = malloc((unsigned) Length);

	/* activate the Options */
	for (i = 0; Options[i] != '\0'; i++) Setflag(Options[i]);

	/* echo Input params */
	if (Traceflag) printf(FORMAT, OUT_VARS);

	/* enable setting of Logflag from command line (used in "ga") */
	if (argc >= 3) if (*argv[2] == 'l') Logflag = 1;

	/* scratch the output file (unless this is a restart) */
	if (!Restartflag)
	{
		if ((fp = fopen(Outfile, "w")) == NULL)
		{
			sprintf(msg, "Input: can't open %s", Outfile);
			Error(msg);
		}
		fclose(fp);
		if (DPEfreq) if ((fp = fopen(DPEfile, "w")) == NULL)
		{
			sprintf(msg, "Input: can't open %s", DPEfile);
			Error(msg);
		}
		fclose(fp);
	}

	/* log this activation */
	if (Logflag)
	{
		if ((fp = fopen(Logfile, "a")) == NULL)
		{
			sprintf(msg,"Input: can't open %s", Logfile);
			Error(msg);
		}
		fprintf(fp, "%s ", argv[0]);
		if (Restartflag) fprintf(fp, "re");
		time(&clock);
		fprintf(fp, "started  %s", ctime(&clock));
		fclose(fp);
	}

#ifndef NOSIGNAL
	{
		extern void (*Catcher)();

		/* set custom signal catcher */
		signal(SIGINT,  Catcher);
		signal(SIGTERM, Catcher);
	}
#endif  /* !NOSIGNAL */

}

/** end of file **/

