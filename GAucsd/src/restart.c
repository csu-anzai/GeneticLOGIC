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
 *  file:	restart.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1982
 *
 *  purpose:	restart an interupted GA run.
 *
 *  modified:	13 feb 86
 *
 */

#define   EXTERN
#include "global.h"

extern void Readbest();
extern void Setptr();
extern long Rstate[];


Restart()
{
	FILE *fp, *fopen();
	int i,j;
	int k;
	char msg[40];

	Trace("Restart entered");

	fp = fopen(Ckptfile, "r");
	if (fp == NULL) 
	{
		sprintf(msg, "Restart: can't open %s", Ckptfile);
		Error(msg);
	}

	if (fscanf(fp, FORM_CKPT, IN_CKPT) < N_CKPT)
	{
		sprintf(msg,"Restart: garbled file %s (globals)", Ckptfile);
		Error(msg);
	}

	fscanf(fp, " Window ");
	for (i = 0; i < Windowsize; i++)
		if (fscanf(fp, " %lf ", &Window[i]) < 1)
		{
			sprintf(msg,"Restart: garbled file %s (Window)", Ckptfile);
			Error(msg);
		}

	fscanf(fp, " Random State ");
	for (i = 0; i < RAND_DEG; i++)
		if (fscanf(fp, " 0x%lx ", &Rstate[i]) < 1)
		{
			sprintf(msg,"Restart: garbled file %s (Random State)", Ckptfile);
			Error(msg);
		}
	fscanf(fp, " %d ", &i);
	Setptr(i);

	fscanf(fp, " DPE State ");
	for (i = 0; i < GAgenes; i++)
		if (fscanf(fp, " %lf %lf %lf %lf ",
			&GAfact[i], &GAbase[i], &DPEhist[2*i], &DPEhist[2*i+1]) < 4)
			if (DPEfreq)
			{
				sprintf(msg,"Restart: garbled file %s (DPE State)", Ckptfile);
				Error(msg);
			}

	fscanf(fp, " Population ");
	for (i = 0; i < Popsize; i++)
	{
		for (j = 0; j < Length; j++)
		{
			fscanf(fp,"%1d", &k);
			Buff[j] = k;
		}
		Pack(Buff, New[i].Gene);
		fscanf(fp, " %lf ", &New[i].Perf);
		if (fscanf(fp, " %d ", &New[i].Needs_evaluation) < 1)
		{
			sprintf(msg,"Restart: %s garbled (line %d of Population)",
				Ckptfile, i + 1);
			Error(msg);
		}
	}
	fclose(fp);

	if (Totalexperiments > 1)
		sprintf(Bestfile, "%s.%03d", Minfile, Experiment);

	Readbest();
	Doneflag = (Trials >= Totaltrials);

	Trace("Restart completed");
}

/** end of file **/

