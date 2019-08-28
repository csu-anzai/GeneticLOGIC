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
 *  file:	schema.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	3 feb 86
 *
 *  purpose:	measure the current allocation of trials to a schema
 *		and record the results in Schemafile.
 *
 *  modified:	13 feb 86
 *
 */

#define   EXTERN
#include "global.h"

extern void Unpack();

Schema()
{
	FILE *fopen();
	static FILE *fp;
	static char *S;
	static int firstflag = 1;
	static int firstcount = 1;
	static int lastcount;

	int i, j, ok;
	int count = 0;
	double expected = 0.0;
	double perf = 0.0;
	double sum = 0.0;
	char msg[40], tmp;


	Trace("Schema entered");

	if (firstflag)
	{
	/*  initialize schema S from schemafile */
		S = malloc((unsigned) Length);
		if ((fp = fopen(Schemafile, "r")) == NULL)
		{
			sprintf(msg, "Schema: can't open %s", Schemafile);
			Error(msg);
		}
		for (i = 0; i < Length; i++)
		{
			fscanf(fp, "%c", &tmp);
			if (tmp == '0')      S[i] = 0;
			else if (tmp == '1') S[i] = 1;
			else                 S[i] = '#';
		}
		fclose(fp);
		if ((fp = fopen(Schemafile, "w")) == NULL)
		{
			sprintf(msg, "Schema: can't open %s", Schemafile);
			Error(msg);
		}
		for (i = 0; i < Length; i++)
		{
			if (S[i] == '#') fprintf(fp, "#");
			else if (S[i])   fprintf(fp, "1");
			else             fprintf(fp, "0");
		}
		fprintf(fp, "\n");
		fprintf(fp, " Gen  Count  Incr  Expct  ");
		fprintf(fp, "Schema Ave    Pop. Ave\n");
		firstflag = 0;
	}

	/* record count and expected offspring of S in current pop */
	for (i = 0; i < Popsize; i++)
	{
		Unpack(New[i].Gene, Buff);
		for (ok = 1, j = 0; ok && (j < Length); j++)
		{
			ok = (S[j] == '#') || (S[j] == Buff[j]);
		}
		if (New[i].Perf < Worst)
		{
			sum += Worst - New[i].Perf;
			if (ok)
			{
				count++;
				perf += New[i].Perf;
			}
		}
		else if (ok)
		{
			count++;
			perf += New[i].Perf;
			expected += Worst - New[i].Perf;
		}
	}

	if (firstcount && count)
	{
		lastcount = count;
		firstcount = 0;
	}
	if (count)
	{
		expected = Worst*count - perf - expected;
		expected *= Popsize;
		expected /= sum;
		perf /= count;

		fprintf(fp, "%4d  %4d ", Gen, count);
		fprintf(fp, " %5.3f ", ((double)count)/lastcount);
		lastcount = count;
		fprintf(fp, " %5.3f ", expected);
		fprintf(fp, " %10.3e ", perf);
		fprintf(fp, " %10.3e ", Ave_current_perf);
		fprintf(fp, "\n");
	}

	Trace("Schema completed");
}

/***  end of file ***/
