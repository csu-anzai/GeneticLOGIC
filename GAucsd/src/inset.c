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
 *  file:	inset.c (formerly setup.c)
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1983
 *
 *  purpose:	create an input file for GAucsd.
 *		Default values for input parameters are provided
 *		when the user gives a null response to a prompt.
 *
 *  modified:	feb. 1986
 *              08/24/89: get input parameters from global.h - Nici.
 */

#define UTILITY

#include "global.h"
#include "format.h"

#define  LIMIT 30	/* max. length of user input field */
#define  DIGITS 2	/* significant digits for rounding */

#define SETPAR(Prompt, Format, Var) \
	{	printf(Prompt); printf(" ["); printf(Format, Var); \
		printf("]: "); fflush(stdout); \
		sscanf(gets(s), Format, &Var); }

#define ROUND(X) \
	if ((X) > 1e-30) \
	{	double _dIgS; _dIgS = floor(DIGITS-log((double)X)/log(10.0)); \
		_dIgS = exp(_dIgS*log(10.0)); \
		X = ((long)(_dIgS*X + 0.5))/_dIgS; }

main()
{
	extern double Rand();
	char s[LIMIT];
	char base[LIMIT];
	char ga[LIMIT+3];
	char infile[LIMIT+3];
	char cmd[128];
	char eos = '\0';
	char *garg[MAXGARG];
	FILE *fp, *fopen();
	int preset = 0;
	int status;
	double tmp;

	printf("\nEvaluation File Name [f1]: "); 
	fgetstr(ga, LIMIT);
	if (strlen(ga) == 0) strcpy(ga, "f1");
#ifndef NOSTAT
	if (!stat("Makefile", NULL) || !stat("makefile", NULL))
		sprintf(cmd, "make GAeval=%s", ga);
	else
#endif
	sprintf(cmd, "make -f %s GAeval=%s", MF, ga);
	status = system(cmd);
	if (status)
	{
		printf("Setup aborted.\n\n");
		exit(1);
	}

	sprintf(base, "ga%06d.tmp", ((int) time(0))%1000000);
	sprintf(cmd, "awk \"/\\/\\*[ \t][ \t]*GAlength[ \t][ \t]*[0-9][0-9]*[ \t]/ {print \\$3}\" %s_ga.c > %s", ga, base);
	printf("Checking for GAlength entry in %s_ga.c\n", ga);

	if (system(cmd))
	{
		printf("Warning: unable to locate GAlength entry;");
		printf(" error in command\n    %s\n", cmd);
	}
	else
	{
		fp = (FILE *) fopen(base, "r");

		if (fgets(cmd, 80, fp))
		{
			if (!sscanf(cmd, "%d", &Length))
				printf("Warning: ignoring bad GAlength entry\n    %s\n", cmd);
			else if (fscanf(fp, "%*s") != EOF)
				printf("Warning: ignoring extraneous GAlength entries.\n");
		}
		else printf("Warning: no GAlength entry found.\n");

		fclose(fp);
		unlink(base);
	}

	printf("\nName of Experiment [%s]: ", ga);
	fgetstr(base, LIMIT);
	if (strlen(base) == 0) strcpy(base, ga);
	name_file(infile, "in", base);   

	for (status = 0; status < MAXGARG; status++)
		garg[status] = &eos;
	if ((fp = fopen(infile, "r")) != NULL)
	{
		preset = fscanf(fp, FORMAT, IN_VARS);
		if (preset == N_FORM)
		{
			preset += fscanf(fp, FORM_2, IN_2);
			if (preset < N_FORM) preset = N_FORM;
		}
		else if (preset < 0) preset = 0;

		status = 0;
		if (fgets(cmd, 100, fp))
			if (!strcmp(cmd, GARGSEP))
				while (fgets(cmd, 100, fp))
				{
					garg[status] = (char *) malloc(strlen(cmd));
					cmd[strlen(cmd) - 1] = '\0';
					strcpy(garg[status++], cmd);
				}
		fclose(fp);
		preset += status;
		printf("Using %d defaults from %s\n\n", preset, infile);
	}
	else printf("Creating file %s\n\n", infile);

	while ((fp = fopen(infile, "a")) == NULL)
	{
		printf("Can't open %s for writing\n", infile);
		printf("Please provide alternative name: ");
		*base = '\0';
		fgetstr(base, LIMIT);
		if (strlen(base) == 0)
		{
			printf("Setup aborted.\n\n");
			exit(1);
		}
		name_file(infile, "in", base);   
		printf("\n");
	}
	fclose(fp);	/* this was just a test */

	SETPAR("Genome Length", "%d", Length);

	if (preset < 3)
	{
	/* Default shatters schemata in sqrt(Length)
	 * generations at the given crossover rate.
	 */
		tmp = pow((double)2.0, sqrt((double)Length)/(2.0*C_rate));
		if (tmp > 32499.0) Popsize = 32500;
		else
		{
			Popsize = (int) tmp;
			if (Popsize < 3) Popsize = 3;
			else ROUND(Popsize);
		}
	}
	SETPAR("Population Size", "%d", Popsize);

	if (preset < 2)
	{
		/* Default is square of revised shatter time.
		 */
        tmp = Length / (2.8854*C_rate*log((double)Popsize));
		Totaltrials = (int) Popsize*tmp*tmp;
		ROUND(Totaltrials);
	}
	SETPAR("Trials per Run", "%ld", Totaltrials);

	SETPAR("Number of Runs", "%d", Totalexperiments);

	if (preset < 5)
	{
    /* Default shatters schemata in square root of
     * running time at the given population size.
     */
        C_rate = (Length / sqrt(Totaltrials/(double)Popsize))
               / (2.8854*log((double)Popsize));
		ROUND(C_rate);
	}
	SETPAR("Crossover Rate (per individual)", "%lf", C_rate);

	if (preset < 6)
	{
		/* Default uses empirical relationship found by
		 * Schaffer et al. in ICGA-3; no theoretic support
		 * but seems to work well in practice.
		 */
		M_rate = 0.5*sqrt(2.72/Length)/Popsize;
		ROUND(M_rate);
	}
	SETPAR("Mutation Rate", "%lf", M_rate);

	SETPAR("Generation Gap", "%lf", Gapsize);

	SETPAR("Windowsize (hit return for sigma scaling)", "%d", Windowsize);
	if (Windowsize < 0)
		SETPAR("Sigma Scaling Factor", "%lf", Sigfact);

	SETPAR("DPE Time Constant", "%d", DPEfreq);

	if (preset < 18)
	{
		/* Default based on DPE analysis by Schraudolph
		 */
		tmp = (1.0 - FIT_RATIO) + M_rate*(FIT_RATIO + 1.0);
		Convlev = tmp - sqrt(tmp*tmp - M_rate*(4.0 - 4.0*FIT_RATIO));
		Convlev /= 2.0 - 2.0*FIT_RATIO;
		Convlev = 1.0 - Convlev;
		ROUND(Convlev);
	}
	SETPAR("Convergence Threshold", "%lf", Convlev);

	if (preset < 17) Maxconv = Length;
	SETPAR("Max Alleles to Converge", "%d", Maxconv);

	SETPAR("Maximum Bias", "%lf", Maxbias);
	SETPAR("Max Gens w/o Evaluation", "%d", Maxspin);

	if (preset < 9)
	{
		Interval = exp(log((double) Totaltrials)/3.0) + 0.5;
		Interval *= Interval;
		ROUND(Interval);
	}
	SETPAR("Report Interval", "%d", Interval);

	if (preset < 10)
		Savesize = (int)((1.0 - Convlev)*Popsize);
	SETPAR("Structures Saved", "%d", Savesize);

	if (preset < 12)
	{
		Dump_freq = (Interval*Totalexperiments)/Popsize;
		ROUND(Dump_freq);
	}
	SETPAR("Dump Interval", "%d", Dump_freq);
	if (Dump_freq)
		SETPAR("Dumps Saved", "%d", Num_dumps);

	printf("Options [%s]: ", Options);
	fflush(stdout);
	sscanf(gets(s), "%s", Options);

	if (preset < 15)
	{
		Seed = (unsigned int) time(0);
		Srand(&Seed);
	}
	SETPAR("Random Seed", "%u", Seed);

	printf("\nWriting new settings to %s\n", infile);
	fp = fopen(infile, "w");
	fprintf(fp, FORMAT, OUT_VARS);
	fprintf(fp, FORM_2, OUT_2);

	printf("\nApplication-specific Arguments:\n");
	for (status = 0; status < MAXGARG; status++)
	{
		printf("%2d [%s]: ", status, garg[status]);
		if (!status) fprintf(fp, GARGSEP);
		if (!*gets(s))
		{
			if (!*garg[status]) break;
			else fprintf(fp, "%s\n", garg[status]);
		}
		else fprintf(fp, "%s\n", s);
	}
	fclose(fp);

#ifdef NOGAGX
	printf("\nRun experiment with: %s %s\n", ga, base);
#else
	printf("\nqueue []: ");
	fgetstr(s, LIMIT);
	if (strlen(s) == 0)
	{
		sprintf(cmd, "(ga %s %s; echo ) &", ga, base);
		system(cmd);
		printf("ga command executed\n");
	}
	else
	{
		if ((fp = fopen(s, "a")) == NULL)
		{
			printf("Error: can't open %s\n", s);
			printf("Setup aborted.\n\n");
			exit(1);
		}
		else
		{
			fprintf(fp, "        ga %s %s &\n", ga, base);
			fclose(fp);
		}
		printf("ga command queued on %s\n", s);
	}
#endif   /* NOGAGX */
	printf("Setup done.\n\n");
}



fgetstr(s, n) 
char *s;
int n;
{
	register int c;
	while (--n > 0 && (c = getchar()) != EOF)
		if ( c != '\n' )
			*s++ = c;
		else
			break;
	*s = '\0';
	return;
}

/*** end of file ***/
