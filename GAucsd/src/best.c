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
 *  file:	best.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1983
 *
 *  purpose:	input and output of best structures
 *
 *  modified:	15 apr 86
 *		16 dec 86: don't print Bestsize to minfile in Printbest()
 *      02 nov 90: store phenotypes, allow duplicates when -a
 */

#define   EXTERN
#include "global.h"

extern void Pack();
extern void Unpack();
extern double _eval();

static double max;	/* maximum (worst) performance in the Bestset */
static int maxptr;	/* pointer to structure with perf = max */


Savebest(i)
register int i;
{
	/*  Save the ith structure in current population  */
	/*  if it is one of the Savesize best seen so far */

	register int j;		/* loop control var */
	register int k;

	int found;
	if (Bestsize < Savesize)
	{
		if (!Allflag)
		{
			/* Bestsize is the number saved so far, so  		*/
			/* there are still empty slots in the Bestset.		*/
			/* Check if an identical structure is already there	*/

			for (j = 0; j < Bestsize; j++)
			{
				for (k = 0, found = 1; (k < Bytes) && found; k++)
					found = (New[i].Gene[k] == Bestset[j].Gene[k]);
				if (found && Bestset[j].Gen > Lastzoom) return;
			}
		}

		/* insert ith structure */
		for (k = 0; k < Bytes; k++)
			Bestset[Bestsize].Gene[k] = New[i].Gene[k];

		/* obtain the phenotype, if available */
		*(Bestset[Bestsize].Phene) = '\0';
		_eval(Bestset[Bestsize].Phene, -Phenesize);

		Bestset[Bestsize].Perf = New[i].Perf;
		Bestset[Bestsize].Gen = Gen;
		Bestset[Bestsize].Trials = Trials;
		Bestsize++;

		if (Bestsize == Savesize)
		{
			/* find worst element in Bestset */
			max = Bestset[0].Perf;
			maxptr = 0;
			for (j = 1; j < Savesize; j++)
			{
				if (max < Bestset[j].Perf)
				{
					max = Bestset[j].Perf;
					maxptr = j;
				}
			}
		}
	}
	else if (New[i].Perf < max) 	/* then save New[i] */
	{
		if (!Allflag)   	/* unless its already there */
		{
			for (j = 0; j < Bestsize; j++)
			{
				for (k = 0, found = 1; (k < Bytes) && found; k++)
					found = (New[i].Gene[k] == Bestset[j].Gene[k]);
				if (found && Bestset[j].Gen > Lastzoom) return;
			}
		}

		/* overwrite the worst one */
		for (k = 0; k < Bytes; k++)
			Bestset[maxptr].Gene[k] = New[i].Gene[k];

		/* obtain the phenotype, if available */
		*(Bestset[maxptr].Phene) = '\0';
		_eval(Bestset[maxptr].Phene, -Phenesize);

		Bestset[maxptr].Perf = New[i].Perf;
		Bestset[maxptr].Gen = Gen;
		Bestset[maxptr].Trials = Trials;

		/* find worst element in Bestset */
		max = Bestset[0].Perf;
		maxptr = 0;
		for (j = 1; j < Savesize; j++)
		{
			if (max < Bestset[j].Perf)
			{
				max = Bestset[j].Perf;
				maxptr = j;
			}
		}
	}
}


Printbest()
{
	/*	Write the Best structures out to the Bestfile.	*/

	register int i;
	register int j;
	register int k;
	register int posn;
	FILE *fp, *fopen();

	Trace("Printbest entered");

	fp = fopen(Bestfile, "w");
	for (i = 0; i < Bestsize; i++)
	{
		Unpack(Bestset[i].Gene, Buff);
		for (j = k = posn = 0; j < Length; j++)
		{
			if (GAgenes)
			{
				if (j == posn)
				{
					if (j) fprintf(fp, " ");
					posn = GAposn[k++];
					if (posn < 0) posn = -posn;
				}
			}
			fprintf(fp, "%1d", Buff[j]);
		}

		fprintf(fp, "  %11.4e ", Bestset[i].Perf);
		fprintf(fp, "%4d %4ld ", Bestset[i].Gen, Bestset[i].Trials);
		fprintf(fp, "%s\n\n",    Bestset[i].Phene);
	}

	fclose(fp);

	Trace("Printbest completed");
}


Readbest()
{
	/*   Read the Best structures in from the Bestfile    */
	/*   (during a Restart)                               */

	int i, j, k;
	char *s;
	FILE *fp, *fopen();
	static char *dummy = ".";

	if (Savesize)
	{
		Trace("Readbest entered");

		fp = fopen(Bestfile, "r");
		if (fp == NULL) 
		{
			Bestsize = 0;
			return;
		}

		for (Bestsize = 0; (fscanf(fp, "%1d", &k) != EOF); Bestsize++)
		{
			Buff[0] = k;
			for (j = 1; j < Length; j++)
			{
				fscanf(fp, "%1d", &k);
				Buff[j] = k;
			}
			Pack(Buff, Bestset[Bestsize].Gene);

			fscanf(fp, " %lf", &Bestset[Bestsize].Perf);
			fscanf(fp, " %d ", &Bestset[Bestsize].Gen);
			fscanf(fp, " %ld", &Bestset[Bestsize].Trials);

			/* read phenotype description, if any */
			s = dummy;
			for (k = -1; k < Phenesize && *s != '\n'; k += strlen(s))
				s = fgets(Bestset[Bestsize].Phene+k, Phenesize+1-k, fp);

			if (k < Phenesize) Bestset[Bestsize].Phene[k-2] = '\0';
			else Error("Phenotype description too long");
		}
		fclose(fp);

		/* find worst element in Bestset */
		max = Bestset[0].Perf;
		maxptr = 0;
		for (i = 1; i < Bestsize; i++)
		{
			if (max < Bestset[i].Perf)
			{
				max = Bestset[i].Perf;
				maxptr = i;
			}
		}

		Trace("Readbest completed");
	}
}

/**** end of file ****/

