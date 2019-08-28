
/* random.c -- random number generator routines

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
static char *rcsid = "$Id: scs-rand.c,v 1.1 1993/02/15 09:43:25 joke Exp $";
#endif

#define D_ONE		(double)1.0
#define D_ZERO		(double)0.0
#define D_JOKE		(double)0.0
#define TRUE		1
#define FALSE		0
#define boolean		int

#include <stdio.h>
#include <math.h>
#include "../../random.h"


double randomseedvalue = D_JOKE;	/* default seed = 0.02021965 */
char *randomversion = V_RANDOM;

/* local vars for random generator */
static double oldrand[R_MAXRAND];
static int jrand;

/* local vars for randomnormaldeviate */
static double rndx2;
static int rndcalcflag;


/*
 *	advance_random -- create next batch of R_MAXRAND (55) random numbers
 */
static void
advance_random ()
{
    int j;
    double new_random;

    for (j = 0; j < 24; j++) {
	new_random = oldrand[j] - oldrand[j + 31];

	if (new_random < D_ZERO)
	    new_random += D_ONE;

	oldrand[j] = new_random;
    }

    for (j = 24; j < R_MAXRAND; j++) {
	new_random = oldrand[j] - oldrand[j - 24];

	if (new_random < D_ZERO)
	    new_random += D_ONE;

	oldrand[j] = new_random;
    }
}

/*
 *	warmup_random -- get random off and running
 */
static void
warmup_random (randomseed)
    double randomseed;
{
    int i, j;
    double new_random;
    double prev_random;

    oldrand[R_MAXRAND - 1] = randomseed;

    new_random = R_INITIALSEED;
    prev_random = randomseed;

    for (j = 1; j < R_MAXRAND; j++) {
	i = (21 * j) % R_MAXRAND - 1;

	oldrand[i] = new_random;
	new_random = prev_random - new_random;

	if (new_random < D_ZERO)
	    new_random += D_ONE;

	prev_random = oldrand[i];
    }

    advance_random ();
    advance_random ();
    advance_random ();

    jrand = 0;
}

/*
 *	randomize -- initialize the random number generator
 */
void
randomize (batchflag)
    int batchflag;
{
    double randomseed;
    int i;

    /* initialize the random number generator */
    for (i = 0; i < R_MAXRAND; i++)
	oldrand[i] = D_ZERO;

    if (!batchflag) {
	do {
	    printf ("Enter a seed random number between 0.0 and 1.0: ");
	    fscanf (stdin, "%lf", &randomseed);
/*	    if (readln (stdin, "%lf", &randomseed))
		panic (E_WARN, "randomize", "readln: can't read `randomseed'");
*/	} while (randomseed < D_ZERO || randomseed > D_ONE);
    } else {
	randomseed = randomseedvalue;	/* externally set */
    }
    warmup_random (randomseed);
}

/*
 *	randomperc -- fetch a single random number between 0.0 and 1.0
 *		See Knuth, D.E. (1969), vol. 2 for details
 */
double
randomperc ()
{
    if (++jrand == R_MAXRAND) {
	jrand = 0;
	advance_random ();
    }
    return ((double) oldrand[jrand]);
}

/*
 *	flip -- flip a biased coin, true if heads
 */
boolean
flip (probability)
    double probability;
{
    if (probability == D_ONE)
	return (TRUE);
    else
	return ((boolean) (randomperc () <= probability));
}

/*
 *	rnd -- pick a random integer between lo and hi (inclusive)
 */
int
rnd (lo, hi)
    int lo, hi;
{
    return ((int) (randomperc () * (hi - lo) + lo));
}

/*
 *	initrandomnormaldeviate -- initialize
 */
void
initrandomnormaldeviate ()
{
    rndcalcflag = TRUE;
}

/*
 *	randomnormaldeviate -- rnd after ACM algorithm 267 (Box-Muller-Method)
 */
double
randomnormaldeviate ()
{
    double t, rndx1;

    if (rndcalcflag) {
	rndx1 = sqrt (R_X1 * log (randomperc ()));
	t = R_T * randomperc ();
	rndx2 = rndx1 * sin (t);
	rndcalcflag = FALSE;
	return ((double) (rndx1 * cos (t)));
    } else {
	rndcalcflag = TRUE;
	return ((double) rndx2);
    }
}

/*
 *	noise -- normal noise with specified mean & std dev: mu & sigma
 */
double
noise (mu, sigma)
    double mu, sigma;
{
    return ((double) (randomnormaldeviate () * sigma + mu));
}

/*
 *	rndreal -- real (double) random number between specified limits
 */
double
rndreal (lo, hi)
    double lo, hi;
{
    return ((double) (randomperc () * (hi - lo) + lo));
}

#ifdef TEST
main (argc, argv)
int argc;
char **argv;
{
	int i;

	randomize(TRUE);

	for (i = 0; i < 1000; i++)
		printf ("%.8lf\n", (double)randomperc ());
	return 0;
}
#endif
