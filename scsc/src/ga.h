
/* ga.h -- genetic algorithm routines interface

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

/* $Id: ga.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __GA_H__
#define __GA_H__

#define MAXMATING	(10)

typedef struct
  {
    int mate1;
    int mate2;
    int mort1;
    int mort2;
    int sitecross;
  }

mrecord_t, MRECORD;		/* 2 candidates go here */


typedef mrecord_t
  marray_t[MAXMATING], MARRAY[MAXMATING];	/* all mating candidates go here */


typedef struct
  {
    double proportionselect;
    double pmutation;
    double pcrossover;

    int ncrossover;
    int nmutation;
    int crowdingfactor;
    int crowdingsubpop;
    int nselect;

    marray_t mating;		/* mating records for GA reports */
  }

grecord_t, GRECORD;		/* simple GA with crowding */


extern grecord_t garec;		/* the GA goes here */
extern FILE *gfile;		/* GA parameters file */
extern char gfilename[MAXFILENAME];	/* parameters file name */

/* ./ga.c */
#ifdef __STDC__
void initga (FILE * gfile, grecord_t * ga, class_p * pop);
void initrepga (FILE * rep, grecord_t * ga);
void ga (grecord_t * ga, class_p * pop);
void reportga (FILE * rep, grecord_t * ga, class_p * pop);
#else
void initga ( /* FILE *gfile, grecord_t *ga, class_p *pop */ );
void initrepga ( /* FILE *rep, grecord_t *ga */ );
void ga ( /* grecord_t *ga, class_p *pop */ );
void reportga ( /* FILE *rep, grecord_t *ga, class_p *pop */ );
#endif /* !__STDC__ */

#endif /* __GA_H__ */
