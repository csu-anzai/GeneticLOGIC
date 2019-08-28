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
 *  file:	generate.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	One generation consists of
 *			(1) forming a new population of structures.
 *			(2) evaluating the population.
 *			(3) gathering performance statistics.
 *
 *  modified:	7 feb 86
 *
 *		2 dec 86: Measure() now calls Converge().
 */

#define   EXTERN
#include "global.h"

extern void Checkpoint();
extern void Crossover();
extern void Elitist();
extern void Evaluate();
extern void Initialize();
extern void Mutate();
extern void Printbest();
extern void Restart();
extern void Select();
extern void DPE();


Generate()
{
	STRUCTURE *temp;	/* for swapping population pointers	*/
	register int i;		/* for marking structures		*/

	if (Traceflag) printf("                    Gen %d\n",Gen);
	Trace("Generate entered");

	/* create a new population */

	if (Restartflag)	/* restart - read "ckpt" file */
		Restart();
	else if (Gen == 0)	/* this is a fresh run */
	{
		Initialize();	/* form an initial population */
		Spin++;	
	}
	else
		/* form a new population from the */
		/* old one via genetic operators  */
	{
		Select();
		Mutate();
		Crossover();
		if (Eliteflag) Elitist();

		/* perform DPE if appropriate */
		if (DPEfreq) DPE();

		if (Allflag)	/* mark structures for evaluation */
			for (i = 0; i < Popsize; i++) New[i].Needs_evaluation = 1;

		Spin++;
	}

	/* evaluate the newly formed population */
	Evaluate();

	/* gather performance statistics */
	Measure();

	/* exit with dump after two signals */
	if (Sigcount == 2) Doneflag = Lastflag = 1;

	/* checkpoint if appropriate */
	if (Num_dumps && Dump_freq && Gen % Dump_freq == 0)
	{
		if (Num_dumps > 1)
		{
			sprintf(Dumpfile, "%s.%03d", Ckptfile, Curr_dump);
			Curr_dump = (Curr_dump + 1) % Num_dumps;
			Checkpoint(Dumpfile);
		}
		Checkpoint(Ckptfile);
	}
	else if (Doneflag)
	{
		if (Lastflag) Checkpoint(Ckptfile);
		else if (Savesize) Printbest();
	}

	/* prepare for next generation */
	temp = Old;
	Old = New;
	New = temp;
	Gen++;
	Restartflag = 0;

	Trace("Generate completed");
}

/*  end of file */
