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
 *  file:	init.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	create an initial population of structures,
 *		and initalize some performance variables.
 *		This is called at the start of each run.
 *
 *  modified:	7 feb 86
 *
 */

#define   EXTERN
#include "global.h"

extern void Generate();
extern void Pack();


Initialize()
{
	FILE *fp, *fopen();
	register int i, j;
	register int k;
	register int num;
	int shift, byte;
	int nc, *counters;
	int x;
	double r;

	Trace("Initialize entered");

	if (Totalexperiments > 1)
		sprintf(Bestfile, "%s.%03d", Minfile, Experiment);

	/* prepare for new run */

	/* NOTE: Srand() automatically advances Seed for next run */
	Srand(&Seed);

	Sigcount = 0;
	Doneflag = 0;
	Curr_dump = 0;
	Bestsize = 0;
	Trials = Gen = 0;
	Lost = Conv = 0;
	Plateau = 0;
	Spin = 0;
	Lastzoom = -1;
	Onsum = Offsum = 0.0;
	for (i = 0; i < Windowsize; i++) Window[i] = 0.0;
	for (i = 0; i < 2*GAgenes; i++)  DPEhist[i] = 0.5*Popsize;

	/* set up initial population */

	i = 0;		/* current structure */

	if (Initflag)		/* get some structures from Initfile */
	{
		if ((fp = fopen(Initfile, "r")) == NULL)
		{
			char msg[40];
			sprintf(msg, "Init: can't open %s", Initfile);
			Error(msg);
		}

		/* get the number of structures in Initfile */
		k = 0;
		while (fscanf(fp, "%1d", &x) != EOF) k++;
		fclose(fp);
		if (k % Length != 0) 
		{
			char msg[40];
			sprintf( msg, "Init: bad format in %s", Initfile);
			Error(msg);
		}

		/* num = number of structures to be read from file */
		num = k/Length < Popsize ? k/Length : Popsize;

		if ((fp = fopen(Initfile, "r")) == NULL)
		{
			char msg[40];
			sprintf(msg, "Init: can't open %s", Initfile);
			Error(msg);
		}
		for (i = 0; i < num; i++)
		{
			for (j = 0; j < Length; j++)
			{
				fscanf(fp,"%1d", &x);
				Buff[j] = x;
			}
			Pack(Buff, New[i].Gene);
			New[i].Needs_evaluation = 1;
		}
		fclose(fp);
	}

	/* restore DPE to original search space */
	if (Experiment > 1)
	{
		for (j = 0; j < GAgenes; j++)
		{
			GAfact[j] = Scale[j];
			GAbase[j] = Basis[j];
		}
	}

	if (Uniflag)	/* super-uniform initialization */
	{
		/* find out how many individuals to work on */
		x = 0;
		for (num = 1; num <= Popsize - i; num *= 2) x++;
		num /= 2;
		num--;
		x--;

		/* allocate and initialize counters */
		nc = Length/x;
		if (Length%x) nc++;
		counters = (int *) calloc((unsigned) nc, sizeof(int));
		for (k = 0; k < nc; k++) counters[k] = Randint(0, num);

		for (j = 0; j <= num; i++, j++) 	/* for each individual */
		{
			for (k = 0; k < Bytes; k++) New[i].Gene[k] = 0;

			k = byte = 0;
			shift = CHAR_BIT - x;

			while (k < nc && byte < Bytes)	/* scan through genome */
			{
				/* splice counter into byte */
				if (shift > 0)
				{
					New[i].Gene[byte] |=	/* next counter */
						(char)((counters[k++]++ & num) << shift);
					shift -= x;
				}
				else
				{
					New[i].Gene[byte++] |=	   /* next byte */
						(char)((counters[k] & num) >> -shift);
					shift += CHAR_BIT;
				}
			}
			if (k < nc) counters[k]++;
			New[i].Needs_evaluation = 1;
		}
		free(counters);
	}

	for (; i < Popsize; i++)   /* initialize remainder randomly */ 
	{
		for (j = 0; j < Bytes; j++)
			New[i].Gene[j] = Randint(0, Post[0]);
		New[i].Needs_evaluation = 1;

	}

	/* find first location to mutate */
	if (M_rate > 0.0 && M_rate < 1.0)
	{
		while ((r = Rand()) == 0.0);
		Mu_next = (long) ceil(log(r) / log(1.0 - M_rate))/2;
	}
	else Mu_next = 0;

	Trace("Initialize completed");
}

/** end of file **/

