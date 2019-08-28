
/* environ.c -- environment routines: your application goes here

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
static char *rcsid = "$Id: environ.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

erecord_t environrec;		/* the environment goes here */
FILE *efile;			/* environment parameters file */
char efilename[MAXFILENAME]
= "scs.env";			/* name of parameters file */


/*
 *    generatesignal -- generate a random signal
 */
void
generatesignal (erec)
     erecord_t *erec;
{
  int j;

  for (j = 0; j < erec->lsignal; j++)
    if (flip (D_HALF))
      erec->signal[j] = 1;
    else
      erec->signal[j] = 0;
}

/*
 *    decode -- decode substring as unsigned binary integer
 */
int
decode (mess, start, length)
     message_t mess;
     int start;
     int length;
{
  int j, accum, powerof2;

  accum = 0;
  powerof2 = 1;
  for (j = start - 1; j > start - length - 1; j--)
    {
      accum += powerof2 * mess[j];
      powerof2 *= 2;
    }
  return (accum);
}

/*
 *    multiplexeroutput -- calculate correct multiplexer output
 */
void
multiplexeroutput (erec)
     erecord_t *erec;
{
  /* decode the address */
  erec->address = decode (erec->signal, erec->lsignal, erec->laddress);

  /* set the output */
  erec->output = erec->signal[erec->lsignal -
			      erec->laddress - erec->address - 1];
}

/*
 *    environment -- coordinate multiplexer calculations
 */
void
environment (erec)
     erecord_t *erec;
{
  generatesignal (erec);
  multiplexeroutput (erec);
}

/*
 *    initenvironment -- initialize the multiplexer environment
 */
void
initenvironment (efile, erec)
     FILE *efile;
     erecord_t *erec;
{
  int j;

  /* read number of address lines */
  if (readln (efile, "%d", &(erec->laddress)))
    panic (E_FATAL, "initenvironment", "readln: %s", "can't read `laddress'");

  /* calc number of data lines */
  erec->ldata = round (poweri (D_TWO, erec->laddress));

  /* calc length of signal */
  erec->lsignal = erec->laddress + erec->ldata;

  /* zero out multiplexer */
  erec->address = 0;
  erec->output = 0;
  erec->classifieroutput = 0;

  for (j = 0; j < erec->lsignal; j++)
    erec->signal[j] = 0;
}

/*
 *    initrepenvironment -- write initial environment report
 */
void
initrepenvironment (rep, erec)
     FILE *rep;
     erecord_t *erec;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Environmental Parameters (Multiplexer)\n");
  fprintf (rep, "--------------------------------------\n");
  fprintf (rep, "Number of address lines   = %8d\n", erec->laddress);
  fprintf (rep, "Number of data lines      = %8d\n", erec->ldata);
  fprintf (rep, "Total number of lines     = %8d\n", erec->lsignal);
}

/*
 *    writesignal -- write a signal in bit-reversed order
 */
void
writesignal (rep, signal, lsignal)
     FILE *rep;
     message_t signal;
     int lsignal;
{
  int j;

  for (j = 0; j < lsignal; j++)
    fprintf (rep, "%d", signal[j]);
}

/*
 *    reportenvironment -- write current multiplexer info
 */
void
reportenvironment (rep, erec)
     FILE *rep;
     erecord_t *erec;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Current Multiplexer Status\n");
  fprintf (rep, "--------------------------\n");
  fprintf (rep, "Signal                =   ");
  writesignal (rep, erec->signal, erec->lsignal);
  fprintf (rep, "\n");
  fprintf (rep, "Decoded address       = %8d\n", erec->address);
  fprintf (rep, "Multiplexer output    = %8d\n", erec->output);
  fprintf (rep, "Classifier output     = %8d\n", erec->classifieroutput);
}
