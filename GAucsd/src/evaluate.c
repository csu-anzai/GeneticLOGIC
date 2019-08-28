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
 *  file:	evaluate.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	evaluate the current population by
 *		calling the user-defined function "eval"
 *
 *  modified:	13 feb 86
 *
 *		28 Aug 89: call _eval() with packed gene  - Nici.
 */

#define   EXTERN
#include "global.h"

extern double _eval(); 
extern void Checkpoint();
extern void Savebest();

Evaluate()
{
	register double performance;
	register int i;

	Trace("Evaluate entered");

	for (i = 0; i < Popsize; i++)
	{
		if (New[i].Needs_evaluation)
		{
			New[i].Perf = _eval(New[i].Gene, Length);
			performance = New[i].Perf;
			New[i].Needs_evaluation = 0;
			Trials++;
			Spin = 0;   /* we're making progress */
			if (Savesize) Savebest(i);

			if (performance < Best || Trials == 1) Best = performance;
			Onsum += performance;
			Offsum += Best;

			if (Dumpflag) Checkpoint(Ckptfile);
		}
	}
	Trace("Evaluate completed");
}

/** end of file **/

