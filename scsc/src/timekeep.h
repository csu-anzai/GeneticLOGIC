
/* timekeep.h -- timed events handling routines interface

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

/* $Id: timekeep.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __TIMEKEEP_H__
#define __TIMEKEEP_H__

#define ITERATIONSPERBLOCK	(10000)

typedef struct
  {
    int initialiteration;
    int initialblock;
    int iteration;
    int block;
    int reportperiod;
    int gaperiod;
    int consolereportperiod;
    int plotreportperiod;
    int nextplotreport;
    int nextconsolereport;
    int nextreport;
    int nextga;

    boolean reportflag;
    boolean gaflag;
    boolean consolereportflag;
    boolean plotreportflag;
  }

trecord_t, TRECORD;

extern trecord_t timekeeprec;	/* timekeeper's data goes here */
extern FILE *tfile;		/* timekeeper's parameters file */
extern char tfilename[MAXFILENAME];	/* parameters file name */

/* ./timekeep.c */
#ifdef __STDC__
void inittimekeeper (FILE * tfile, trecord_t * trec);
void initreptimekeeper (FILE * rep, trecord_t * trec);
void timekeeper (trecord_t * trec);
void reporttime (FILE * rep, trecord_t * trec);
#else
void inittimekeeper ( /* FILE *tfile, trecord_t *trec */ );
void initreptimekeeper ( /* FILE *rep, trecord_t *trec */ );
void timekeeper ( /* trecord_t *trec */ );
void reporttime ( /* FILE *rep, trecord_t *trec */ );
#endif /* !__STDC__ */

#endif /* __TIMEKEEP_H__ */
