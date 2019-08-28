
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	schema.c
 *
 *  purpose:	measure the current allocation of trials to a schema
 *		and record the results in Schemafile.
 *
 *  modified:	13 feb 86
 *
 *		12 nov 86: pass Length to Unpack()
 *
 *		10 sep 90: print statistics in display mode
 */

#include "extern.h"

#define DISPSCH 9


Schema()
{
	int i;
	double expected;
	double perf;
	int count;
	static int lastcount = 1;
	char msg[40];
	int ok;
	static int firstflag = 1;
	static int firstcount = 1;

	FILE *fopen();
	static FILE *fp;
	int j;
	char tmp;
	static char *S;

	Trace("Schema entered");
	Dtrace("schema");

	if (firstflag)
	{
	/*  initialize schema S from schemafile */
		S = malloc((unsigned) (Length + 1));
		if ((fp = fopen(Schemafile, "r")) == NULL)
		{
			sprintf(msg, "Schema: can't open %s", Schemafile);
			Error(msg);
		}
		for (i=0; i<Length; i++)
		{
			fscanf(fp, "%c", &S[i]);
		}
		fclose(fp);
		S[Length] = '\0';
		if ((fp = fopen(Schemafile, "w")) == NULL)
		{
			sprintf(msg, "Schema: can't open %s", Schemafile);
			Error(msg);
		}
		fprintf(fp, "%s\n", S);
		fprintf(fp, " Gen  Count  Incr  Expct  ");
		fprintf(fp, "Schema Ave    Pop. Ave\n");

		if (Displayflag)
		{
			move(DISPSCH,0);
			clrtoeol();
			printw("Schema Trace:");
			move(DISPSCH+1,0);
			clrtoeol();
			printw("%s", S);
			move(DISPSCH+3,0);
			clrtoeol();
			printw(" Gen  Count  Incr  Expct  ");
			printw("Schema Ave    Pop. Ave\n");
			refresh();
		}
	
		firstflag = 0;
	}

	/* record count and expected offspring of S in current pop */
	expected = 0.0;
	perf = 0.0;
	count = 0;
	for (i=0; i<Popsize; i++)
	{
		Unpack(New[i].Gene, Bitstring, Length);
		for (ok = 1, j = 0; ok && (j<Length); j++)
		{
			ok = (S[j] == '#') || (S[j] == Bitstring[j]);
		}
		if (ok)
		{
			count++;
			expected += (New[i].Perf - Worst) / 
				   (Ave_current_perf - Worst);
			perf += New[i].Perf;
		}
	}

	if (firstcount && count)
	{
		lastcount = count;
		firstcount = 0;
	}

	if (Displayflag)
	{
		move(DISPSCH+4,0);
		clrtoeol();
		printw("%4d  %4d ", Gen, count);
		printw(" %5.3f ", (count*1.0)/lastcount);
		if (count)
		{
			printw(" %5.3f ", expected/count);
			printw(" %10.3e ", perf/count);
		}
		else
		{
			printw(" %5.3f ", 0.0);
			printw(" %10.3e ", 0.0);
		}
		printw(" %10.3e ", Ave_current_perf);
		refresh();
	}
	
	fprintf(fp, "%4d  %4d ", Gen, count);
	fprintf(fp, " %5.3f ", (count*1.0)/lastcount);
  	if (count)
	{
		fprintf(fp, " %5.3f ", expected/count);
		fprintf(fp, " %10.3e ", perf/count);
		lastcount = count;
	}
	else
	{
		fprintf(fp, " %5.3f ", 0.0);
		fprintf(fp, " %10.3e ", 0.0);
	}
	fprintf(fp, " %10.3e ", Ave_current_perf);
	fprintf(fp, "\n");

	Trace("Schema completed");

}

/***  end of file ***/
