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
 *  file:	checkpt.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1982
 *
 *  purpose:	save global variables in a file for later restart
 *
 *  modified:	18 apr 86
 *
 */

#define   EXTERN
#include "global.h"

extern void Printbest();
extern void Unpack();
extern int  Getptr();
extern long Rstate[];


Checkpoint(ckptfile)
char ckptfile[];
{
	FILE *fp, *fopen();
	int i,j;

	Trace("Checkpoint entered");

	fp = fopen(ckptfile, "w");
	fprintf(fp, FORM_CKPT, OUT_CKPT);

	if (Windowsize > 0)
	{
		fprintf(fp, "\nWindow");
		for (i = 0; i < Windowsize; i++)
		{
			if (!(i%4)) fprintf(fp, "\n");
			fprintf(fp, "%.8le\t", Window[i]);
		}
		fprintf(fp,"\n");
	}

	fprintf(fp, "\nRandom State");
	for (i = 0; i < RAND_DEG; i++)
	{
		if (!(i%4)) fprintf(fp, "\n");
		fprintf(fp, "0x%08lx\t", Rstate[i]);
	}
	fprintf(fp, "%d\n", Getptr());

	if (DPEfreq)
	{
		fprintf(fp, "\nDPE State");
		for (i = 0; i < GAgenes; i++)
			fprintf(fp, "\n%.8le\t%.8le\t%.8le\t%.8le",
				GAfact[i], GAbase[i], DPEhist[2*i], DPEhist[2*i+1]);
		fprintf(fp,"\n");
	}

	fprintf(fp, "\nPopulation\n");
	for (i = 0; i < Popsize; i++)
	{
		Unpack(New[i].Gene, Buff);
		for (j = 0; j < Length; j++)
			fprintf(fp, "%1d", Buff[j]);
		fprintf(fp, " %.12le ", New[i].Perf);
		fprintf(fp, "%1d\n", New[i].Needs_evaluation);
	}
	fclose(fp);

	/*  save the best structures */
	if (Savesize) Printbest();

	Trace("Checkpoint completed");
}

/** end of file **/

