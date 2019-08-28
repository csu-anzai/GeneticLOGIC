
/* initial.c -- initialization routines

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
static char *rcsid = "$Id: initial.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"


/*
 *    interactiveheader -- write a header to the specified file/dev
 */
void
interactiveheader (rep)
     FILE *rep;
{
  fprintf (rep, "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n");
  fprintf (rep, "           SCS-C A Simple Classifier System in C\n");
  fprintf (rep, "              (C) Joerg Heitkoetter, 1993\n");
  fprintf (rep, "                        based on:\n");
  fprintf (rep, "           A Simple Classifier System - SCS\n");
  fprintf (rep, "              (C) David E. Goldberg, 1987\n");
  fprintf (rep, "                 All rights reserved.\n");
  fprintf (rep, "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\n");
}

/*
 *    initialization -- coordinate input and initialization
 */
void
initialization (batchflag)
     int batchflag;
{
  /* some copyright stuff first */
  if (!batchflag)
    interactiveheader (stdout);

  /* random number and normal init */
  randomize (batchflag);
  initrandomnormaldeviate ();

  /* file/device init */
  cfile = open_input (batchflag, "classifier", (char *) cfilename);
  dfile = open_input (batchflag, "detectors", (char *) dfilename);
  efile = open_input (batchflag, "environment", (char *) efilename);
  rfile = open_input (batchflag, "reinforcement", (char *) rfilename);
  tfile = open_input (batchflag, "timekeeper", (char *) tfilename);
  gfile = open_input (batchflag, "GA", (char *) gfilename);

  rep = open_output (batchflag, "report", (char *) lfilename);
  pfile = open_output (batchflag, "plot file", (char *) pfilename);

  /* segment init */
  interactiveheader (rep);

  initclassifiers (cfile, &population);
  initrepclassifiers (rep, &population);

  initenvironment (efile, &environrec);
  initrepenvironment (rep, &environrec);

  initdetectors (dfile, &detecrec);
  initrepdetectors (rep, &detecrec);

  initaoc (cfile, &clearingrec);
  initrepaoc (rep, &clearingrec);

  initreinforcement (rfile, &reinforcementrec);
  initrepreinforcement (rep, &reinforcementrec);

  inittimekeeper (tfile, &timekeeprec);
  initreptimekeeper (rep, &timekeeprec);

  initga (gfile, &garec, &population);
  initrepga (rep, &garec);
}

/*
 *    deinitialization -- file/device cleanup
 */
void
deinitialization ()
{
  fclose (cfile);
  fclose (dfile);
  fclose (efile);
  fclose (rfile);
  fclose (tfile);
  fclose (gfile);
  fclose (pfile);
  fclose (rep);
}
