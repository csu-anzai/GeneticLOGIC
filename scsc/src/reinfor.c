
/* reinfor.c -- reinforcement and criterion procedures

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
static char *rcsid = "$Id: reinfor.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

rrecord_t reinforcementrec;	/* reinforcement data goes here */
FILE *rfile;			/* reinforcement parameters file */
char rfilename[MAXFILENAME]
= "scs.rfc";			/* parameters file name */


/*
 *    initreinforcement -- assigns initial values to all parameters
 */
void
initreinforcement (rfile, rrec)
     FILE *rfile;
     rrecord_t *rrec;
{
  if (readln (rfile, "%lf", &(rrec->reward)))
    panic (E_FATAL, "initreinforcement", "readln: %s", "can't read `reward'");

  rrec->rewardcount = D_ZERO;
  rrec->rewardcount50 = D_ZERO;
  rrec->totalcount = D_ZERO;
  rrec->count50 = D_ZERO;
  rrec->proportionreward = D_ZERO;
  rrec->proportionreward50 = D_ZERO;

  rrec->lastwinner = 0;
}

/*
 *    initrepreinforcement -- initial reinforcement report
 */
void
initrepreinforcement (rep, rrec)
     FILE *rep;
     rrecord_t *rrec;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Reinforcement Parameters\n");
  fprintf (rep, "------------------------\n");
  fprintf (rep, "Reinforcement reward      = %8.1lf\n", rrec->reward);
}

/*
 *    criterion -- return true if criterion is achieved
 */
boolean
criterion (rrec, envrec)
     rrecord_t *rrec;
     erecord_t *envrec;
{
  boolean tempflag;

  tempflag = (envrec->output == envrec->classifieroutput);
  rrec->totalcount++;
  rrec->count50++;

  /* increment and reward counters */
  if (tempflag)
    {
      rrec->rewardcount++;
      rrec->rewardcount50++;
    }
  /* calculate reward proportions: running and last 50 */
  rrec->proportionreward = rrec->rewardcount / rrec->totalcount;
  if (round (rrec->count50 - 50) == 0)
    {
      rrec->proportionreward50 = rrec->rewardcount50 / D_FIFTY;

      /* reset */
      rrec->rewardcount50 = D_ZERO;
      rrec->count50 = D_ZERO;
    }
  return (tempflag);
}

/*
 *    payreward -- pay reward to appropriate individual
 */
void
payreward (pop, rrec, clrec)
     class_p *pop;
     rrecord_t *rrec;
     crecord_t *clrec;
{
  pop->classifier[clrec->winner].strength += rrec->reward;
  rrec->lastwinner = clrec->winner;
}

/*
 *    reportreinforcement -- report award
 */
void
reportreinforcement (rep, rrec)
     FILE *rep;
     rrecord_t *rrec;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Reinforcement Report\n");
  fprintf (rep, "--------------------\n");

  fprintf (rep, "Proportion Correct (from start) = %8.4lf\n", rrec->proportionreward);
  fprintf (rep, "Proportion Correct (last fifty) = %8.4lf\n", rrec->proportionreward50);
  fprintf (rep, "Last winning classifier number  = %8d\n", rrec->lastwinner + 1);
}

/*
 *    reinforcement -- make payment if criterion satisfied
 */
void
reinforcement (rrec, pop, clrec, envrec)
     rrecord_t *rrec;
     class_p *pop;
     crecord_t *clrec;
     erecord_t *envrec;
{
  if (criterion (rrec, envrec))
    payreward (pop, rrec, clrec);
}
