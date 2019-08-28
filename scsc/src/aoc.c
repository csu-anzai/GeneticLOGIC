
/* aoc.c -- apportionment of credit routines

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
static char *rcsid = "$Id: aoc.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

crecord_t clearingrec;		/* the clearinghouse goes here */


/*
 *    initaoc -- initialize clearinghouse record
 */
void
initaoc (cfile, crec)
     FILE *cfile;
     crecord_t *crec;
{
  char c;

  if (readln (cfile, "%c", &c))
    panic (E_FATAL, "initaoc", "readln: %s", "can't scan `bucketbrigadeflag'");

  if ((c == 'y') || (c == 'Y'))
    crec->bucketbrigadeflag = TRUE;
  else
    crec->bucketbrigadeflag = FALSE;

  crec->winner = 0;
  crec->oldwinner = 0;
}

/*
 *    initrepaoc -- initial report of clearinghouse parameters
 */
void
initrepaoc (rep, crec)
     FILE *rep;
     crecord_t *crec;
{

  fprintf (rep, "\n\n");
  fprintf (rep, "Apportionment of Credit Parameters\n");
  fprintf (rep, "----------------------------------\n");
  fprintf (rep, "Bucket brigade flag       =    ");

  if (crec->bucketbrigadeflag)
    fprintf (rep, " true\n");
  else
    fprintf (rep, "false\n");
}

/*
 *    auction -- auction among currently matched classifiers, returns winner
 */
int
auction (pop, mlist, oldwinner)
     class_p *pop;
     class_l *mlist;
     int oldwinner;
{
  int j, k, winner;
  double bidmaximum;

  bidmaximum = D_ZERO;
  winner = oldwinner;

  if (mlist->nactive)
    for (j = 0; j < mlist->nactive; j++)
      {
	k = mlist->clist[j];

	/* use specificity criterion? */
	if (speciflag)
	  {
	    pop->classifier[k].bid =
	      pop->cbid * (pop->bid1 + pop->bid2
			   * pop->classifier[k].specificity)
	      * pop->classifier[k].strength;
	  }
	else
	  {
	    pop->classifier[k].bid =
	      pop->cbid * (pop->bid1 + pop->bid2)
	      * pop->classifier[k].strength;
	  }

	/* add some noise? */
	if (noiseflag)
	  {
	    pop->classifier[k].ebid =
	      pop->cbid * (pop->ebid1 + pop->ebid2
			   * pop->classifier[k].specificity)
	      * pop->classifier[k].strength
	      + noise (D_ZERO, pop->bidsigma);
	  }
	else
	  {
	    pop->classifier[k].ebid =
	      pop->cbid * (pop->ebid1 + pop->ebid2
			   * pop->classifier[k].specificity)
	      * pop->classifier[k].strength;
	  }
	if (pop->classifier[k].ebid > bidmaximum)
	  {
	    winner = k;
	    bidmaximum = pop->classifier[k].ebid;
	  }
      }
  return (winner);
}

/*
 *    clearinghouse -- distribute payment from recent winner to old winner
 */
void
clearinghouse (pop, crec)
     class_p *pop;
     crecord_t *crec;
{
  double payment;

  payment = pop->classifier[crec->winner].bid;

  pop->classifier[crec->winner].strength =
    pop->classifier[crec->winner].strength - payment;

  if (crec->bucketbrigadeflag == TRUE)
    pop->classifier[crec->oldwinner].strength =
      pop->classifier[crec->oldwinner].strength + payment;
}

/*
 *    taxcollector -- collect existing and bidding taxes from population members
 */
void
taxcollector (pop)
     class_p *pop;
{
  int j;
  double bidtaxswitch;

  /* life tax from everyone, bid tax from actives */
  if ((pop->lifetax != D_ZERO)
      || (pop->bidtax != D_ZERO))
    for (j = 0; j < pop->nclassifier; j++)
      {
	if (pop->classifier[j].matchflag == TRUE)
	  bidtaxswitch = D_ONE;
	else
	  bidtaxswitch = D_ZERO;

	pop->classifier[j].strength =
	  pop->classifier[j].strength
	  - (pop->lifetax * pop->classifier[j].strength)
	  - (pop->bidtax * pop->classifier[j].strength
	     * bidtaxswitch);
      }
}

/*
 *    reportaoc -- report who pays to whom
 */
void
reportaoc (rep, crec)
     FILE *rep;
     crecord_t *crec;
{
  fprintf (rep, "New winner = [%d] : old winner = [%d]\n\n",
	   crec->winner + 1, crec->oldwinner + 1);
}

/*
 *    aoc -- apportionment of credit coordinator
 */
void
aoc (pop, mlist, crec)
     class_p *pop;
     class_l *mlist;
     crecord_t *crec;
{
  crec->winner = auction (pop, mlist, crec->oldwinner);
  taxcollector (pop);
  clearinghouse (pop, crec);
}
