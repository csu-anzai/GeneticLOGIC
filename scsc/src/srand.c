
/* srand.c -- srand(3) random number generator routines

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
static char *rcsid = "$Id: srand.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"
#include "srand.h"

double randomseedvalue = D_HALF;	/* default seed = 0.5 */
char *randomversion = V_RANDOM;

/* local vars for randomnormaldeviate */
static double rndx2;
static int rndcalcflag;


/*
 *    randomize -- initialize the random number generator
 */
void
randomize (batchflag)
     int batchflag;
{
  double randomseed;

  if (!batchflag)
    {
      do
	{
	  printf ("Enter a seed random number between 0.0 and 1.0: ");
	  if (readln (stdin, "%lf", &randomseed))
	    panic (E_WARN, "randomize", "readln: can't read `randomseed'");
	}
      while (randomseed < D_ZERO || randomseed > D_ONE);
    }
  else
    {
      randomseed = randomseedvalue;	/* externally set */
    }
  srand ((int) (randomseed * 100000.0));
}

/*
 *    randomperc -- fetch a single random number between 0.0 and 1.0
 *              See UN*X manual page on srand(3)
 */
double
randomperc ()
{
  return ((double) ((double) rand () / (double) R_MAXSRANDVAL));
}

/*
 *    flip -- flip a biased coin, true if heads
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
 *    rnd -- pick a random integer between lo and hi (inclusive)
 */
int
rnd (lo, hi)
     int lo, hi;
{
  return ((int) (randomperc () * (hi - lo) + lo));
}

/*
 *    initrandomnormaldeviate -- initialize
 */
void
initrandomnormaldeviate ()
{
  rndcalcflag = TRUE;
}

/*
 *    randomnormaldeviate -- rdn after ACM algorithm 267 (Box-Muller-Method)
 */
double
randomnormaldeviate ()
{
  double t, rndx1;

  if (rndcalcflag)
    {
      rndx1 = sqrt (R_X1 * log (randomperc ()));
      t = R_T * randomperc ();
      rndx2 = rndx1 * sin (t);
      rndcalcflag = FALSE;
      return ((double) (rndx1 * cos (t)));
    }
  else
    {
      rndcalcflag = TRUE;
      return ((double) rndx2);
    }
}

/*
 *    noise -- normal noise with specified mean & std dev: mu & sigma
 */
double
noise (mu, sigma)
     double mu, sigma;
{
  return ((double) (randomnormaldeviate () * sigma + mu));
}

/*
 *    rndreal -- real (double) random number between specified limits
 */
double
rndreal (lo, hi)
     double lo, hi;
{
  return ((double) (randomperc () * (hi - lo) + lo));
}
