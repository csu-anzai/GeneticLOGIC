
/* detector.c -- detector routines

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
static char *rcsid = "$Id: detector.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

drecord_t detecrec;		/* detector inputs go here */
FILE *dfile;			/* detector parameters */
char dfilename[MAXFILENAME]
= "scs.det";			/* parameter file's name */


/*
 *    detectors -- convert environmental state to env. message
 */
void
detectors (envrec, drec, envmesg)
     erecord_t *envrec;
     drecord_t *drec;
     message_t envmesg;
{
  int i;

  for (i = 0; i < envrec->lsignal; i++)
    envmesg[i] = envrec->signal[i];
}

/*
 *    writemessage -- write a message in bit-reverse order
 */
void
writemessage (rep, envmesg, nposition)
     FILE *rep;
     message_t envmesg;
     int nposition;
{
  int j;

  for (j = 0; j < nposition; j++)
    fprintf (rep, "%d", envmesg[j]);
}

/*
 *    reportdetectors -- write out environmental message
 */
void
reportdetectors (rep, envmesg, nposition)
     FILE *rep;
     message_t envmesg;
     int nposition;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Environmental message: ");
  writemessage (rep, envmesg, nposition);
  fprintf (rep, "\n");
}

/*
 *    initdetectors -- dummy detector initialization
 */
void
initdetectors (dfile, detecrec)
     FILE *dfile;
     drecord_t *detecrec;
{
}

/*
 *    initrepdetectors -- dummy initial detectors report
 */
void
initrepdetectors (rep, detecrec)
     FILE *rep;
     drecord_t *detecrec;
{
}
