
/* random.h -- random number generator routines interface

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

/* $Id: random.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __RANDOM_H__
#define __RANDOM_H__

#define V_RANDOM	"Goldberg's default"

#define R_MAXRAND	(55)
#define	R_INITIALSEED	((double )1.0e-9)
#define R_X1		((double )-2)
#define R_T		((double )6.2831853072)

extern double randomseedvalue;	/* user supplied program parameter */
extern char *randomversion;	/* pointer to V_RANDOM */

/* ./random.c */
#ifdef __STDC__
void randomize (int batchflag);
double randomperc (void);
boolean flip (double probability);
int rnd (int lo, int hi);
void initrandomnormaldeviate (void);
double randomnormaldeviate (void);
double noise (double mu, double sigma);
double rndreal (double lo, double hi);
#else
void randomize ( /* int batchflag */ );
double randomperc ( /* void */ );
boolean flip ( /* double probability */ );
int rnd ( /* int lo, int hi */ );
void initrandomnormaldeviate ( /* void */ );
double randomnormaldeviate ( /* void */ );
double noise ( /* double mu, double sigma */ );
double rndreal ( /* double lo, double hi */ );
#endif /* !__STDC__ */

#endif /* __RANDOM_H__ */
