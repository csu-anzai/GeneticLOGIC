
/* aoc.h -- apportionment of credit routines interface

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

/* $Id: aoc.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __AOC_H__
#define __AOC_H__

typedef struct
  {
    int winner;
    int oldwinner;

    boolean bucketbrigadeflag;
  }

crecord_t, CRECORD;

extern crecord_t clearingrec;	/* the clearinghouse goes here */

extern int noiseflag;		/* noisy auction? */
extern int speciflag;		/* use specificity criterion? */

/* ./aoc.c */
#ifdef __STDC__
void initaoc (FILE * cfile, crecord_t * crec);
void initrepaoc (FILE * rep, crecord_t * crec);
int auction (class_p * pop, class_l * mlist, int oldwinner);
void clearinghouse (class_p * pop, crecord_t * crec);
void taxcollector (class_p * pop);
void reportaoc (FILE * rep, crecord_t * crec);
void aoc (class_p * pop, class_l * mlist, crecord_t * crec);
#else
void initaoc ( /* FILE *cfile, crecord_t *crec */ );
void initrepaoc ( /* FILE *rep, crecord_t *crec */ );
int auction ( /* class_p *pop, class_l *mlist, int oldwinner */ );
void clearinghouse ( /* class_p *pop, crecord_t *crec */ );
void taxcollector ( /* class_p *pop */ );
void reportaoc ( /* FILE *rep, crecord_t *crec */ );
void aoc ( /* class_p *pop, class_l *mlist, crecord_t *crec */ );
#endif /* !__STDC__ */

#endif /* __AOC_H__ */
