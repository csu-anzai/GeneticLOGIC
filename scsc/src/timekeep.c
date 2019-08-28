
/* timekeep.c -- timed events handling routines

   Copyright (C) 1993 Joerg Heitkoetter

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef lint
static char *rcsid = "$Id: timekeep.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

trecord_t timekeeprec;		/* timekeeper's data goes here */
FILE *tfile;			/* timekeeper's parameters file */
char tfilename[MAXFILENAME]
= "scs.tim";			/* parameters file name */


/*
 *    addtime -- increment interactions counter and set carry flag if due
 */
static int
addtime (t, dt, carryflag)
     int t, dt;
     boolean *carryflag;
{
  int tempadd;

  tempadd = t + dt;

  if (tempadd >= ITERATIONSPERBLOCK)
    *carryflag = TRUE;
  else
    *carryflag = FALSE;

  if (*carryflag)
    tempadd %= ITERATIONSPERBLOCK;

  return (tempadd);
}

/*
 *    inittimekeeper -- initialize the keeper
 */
void
inittimekeeper (tfile, trec)
     FILE *tfile;
     trecord_t *trec;
{
  boolean dummyflag;

  trec->iteration = 0;
  trec->block = 0;

  if (readln (tfile, "%d", &(trec->initialiteration)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `initialiteration'");
  if (readln (tfile, "%d", &(trec->initialblock)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `initialblock'");
  if (readln (tfile, "%d", &(trec->reportperiod)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `reportperiod'");
  if (readln (tfile, "%d", &(trec->consolereportperiod)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `consolereportperiod'");
  if (readln (tfile, "%d", &(trec->plotreportperiod)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `plotreportperiod'");
  if (readln (tfile, "%d", &(trec->gaperiod)))
    panic (E_FATAL, "inittimekeeper", "readln: %s", "failed to read `gaperiod'");

  trec->iteration = trec->initialiteration;
  trec->block = trec->initialblock;
  trec->nextga = addtime (trec->iteration, trec->gaperiod, &dummyflag);
  trec->nextreport = addtime (trec->iteration, trec->reportperiod, &dummyflag);
  trec->nextconsolereport = addtime (trec->iteration,
				     trec->consolereportperiod,
				     &dummyflag);
  trec->nextplotreport = addtime (trec->iteration,
				  trec->plotreportperiod,
				  &dummyflag);
}

/*
 *    initreptimekeeper -- initial timekeeper report
 */
void
initreptimekeeper (rep, trec)
     FILE *rep;
     trecord_t *trec;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Timekeeper Parameters\n");
  fprintf (rep, "---------------------\n");

  fprintf (rep, "Initial iteration         = %8d\n", trec->initialiteration);
  fprintf (rep, "Initial block             = %8d\n", trec->initialblock);
  fprintf (rep, "Report period             = %8d\n", trec->reportperiod);
  fprintf (rep, "Console report period     = %8d\n", trec->consolereportperiod);
  fprintf (rep, "Plot report period        = %8d\n", trec->plotreportperiod);
  fprintf (rep, "Genetic algorithm period  = %8d\n", trec->gaperiod);
}

/*
 *    timekeeper -- keep time and set flags for time-driven events
 */
void
timekeeper (trec)
     trecord_t *trec;
{
  boolean carryflag, dummyflag;

  trec->iteration = addtime (trec->iteration, 1, &carryflag);
  if (carryflag)
    trec->block++;

  trec->reportflag = (trec->nextreport == trec->iteration);
  if (trec->reportflag)
    trec->nextreport = addtime (trec->iteration,
				trec->reportperiod,
				&dummyflag);

  trec->consolereportflag = (trec->nextconsolereport == trec->iteration);
  if (trec->consolereportflag)
    trec->nextconsolereport = addtime (trec->iteration,
				       trec->consolereportperiod,
				       &dummyflag);

  trec->plotreportflag = (trec->nextplotreport == trec->iteration);
  if (trec->plotreportflag)
    trec->nextplotreport = addtime (trec->iteration,
				    trec->plotreportperiod,
				    &dummyflag);

  trec->gaflag = (trec->nextga == trec->iteration);
  if (trec->gaflag)
    trec->nextga = addtime (trec->iteration,
			    trec->gaperiod,
			    &dummyflag);
}

/*
 *    reporttime -- print out block and iteration number
 */
void
reporttime (rep, trec)
     FILE *rep;
     trecord_t *trec;
{
  fprintf (rep, "[ Block : Iteration ]   =  [ %d : %d ]\n", trec->block, trec->iteration);
}
