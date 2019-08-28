
/* report.c -- report coodination routines

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
static char *rcsid = "$Id: report.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

FILE *pfile;			/* plotting data goes here */
char pfilename[MAXFILENAME]
= "scs.plt";			/* name of plotting data file */


/*
 *    reportheader -- write a report's header
 */
void
reportheader (rep)
     FILE *rep;
{
  page (rep);

  fprintf (rep, "Snapshot Report\n");
  fprintf (rep, "---------------\n");
}

/*
 *    report -- report condition routine
 */
void
report (rep)
     FILE *rep;
{
  reportheader (rep);
  reporttime (rep, &timekeeprec);
  reportenvironment (rep, &environrec);
  reportdetectors (rep, envmessage, population.nposition);
  reportclassifiers (rep, &population);
  reportaoc (rep, &clearingrec);
  reportreinforcement (rep, &reinforcementrec);
}

/*
 *    consolereport -- print report to stdout
 */
void
consolereport (rrec)
     rrecord_t *rrec;
{
  printf ("****************************\n");
  printf ("     Iteration = %8.0lf\n", rrec->totalcount);
  printf ("     P correct = %8.6lf\n", rrec->proportionreward);
  printf ("   P50 correct = %8.6lf\n", rrec->proportionreward50);
  printf ("****************************\n");
}

/*
 *    plotreport -- print report to plot file
 */
void
plotreport (pfile, rrec)
     FILE *pfile;
     rrecord_t *rrec;
{
  fprintf (pfile, "%8.0lf\t%8.6lf\t%8.6lf\n", rrec->totalcount,
	   rrec->proportionreward, rrec->proportionreward50);
}
