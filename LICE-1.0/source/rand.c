/*
 *	LICE - LInear Cellular Evolution strategy
 *
 *	Copyright (C) 1994 Joachim Sprave
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *	See README for details of LICE.
 *	Send bugs (better: bug descriptions) and comments to
 *
 * /---------------------------------------------------------------------\
 * | Joachim Sprave                  joe@ls11.informatik.uni-dortmund.de |
 * |                      //////\\                                       |
 * | Univ. Dortmund      /        \        44221 Dortmund                |
 * | Dept. CS           _|  _   _ |_       Tel.: +49-231-755 4678        |
 * | Systems Analysis  |.|-(.)-(.)+.|      Fax : +49-231-755 2450        |
 * \------------------  \|    J   |/  -----------------------------------/
 *                       \   ---  /
 *                        \      /
 *                         "####"
 */


#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "lice.h"

#define NRAND_SAMPLES 5

extern long t_rand;

#define Uniform randreal

static int Seed = 0;

static int isinit = 0;

void
initrandom(int seed)
{
    Seed = seed;
}

double
randreal(void)
{
    double result;

    if (!isinit) {
	srand(Seed);
	isinit = 1;
    }
    result = ((double) rand());
    result /= RAND_MAX;

    return (result);
}

int
randint(int lo, int hi)
{
    return (lo + randreal() * (hi - lo + 1));
}

/* the next function is output from f2c !!! */

double
Gauss(double sigma)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    static double d, u, x, y, u0, u1, u2;


/* 	SIGMA	--> standard deviation */

/* L1: */
    u = Uniform();
    u0 = Uniform();
    if (u >= .919544406) {
	goto L2;
    }
    x = (u0 + u * .825339283) * 2.40375766 - 2.11402808;
    goto L10;
  L2:
    if (u < .965487131) {
	goto L4;
    }
  L3:
    u1 = Uniform();
    y = sqrt(4.46911474 - log(u1) * 2.);
    u2 = Uniform();
    if (y * u2 > 2.11402808) {
	goto L3;
    }
    goto L9;
  L4:
    if (u < .949990709) {
	goto L6;
    }
  L5:
    u1 = Uniform();
    y = u1 * .273629336 + 1.84039875;
    u2 = Uniform();
    if (exp(y * -.5 * y) * .39894228 - .443299126 + y * .209694057 < u2 *
	.0427025816) {
	goto L5;
    }
    goto L9;
  L6:
    if (u < .925852334) {
	goto L8;
    }
  L7:
    u1 = Uniform();
    y = u1 * 1.55066917 + .289729574;
    u2 = Uniform();
    if (exp(y * -.5 * y) * .39894228 - .443299126 + y * .209694057 < u2 *
	.0159745227) {
	goto L7;
    }
    goto L9;
  L8:
    u1 = Uniform();
    y = u1 * .289729574;
    u2 = Uniform();
    if (exp(y * -.5 * y) * .39894228 - .382544556 < u2 * .0163977244) {
	goto L8;
    }
  L9:
    x = y;
    if (u0 >= .5) {
	x = -y;
    }
  L10:
    ret_val = sigma * x;
    return ret_val;
}

double
normrand(double m, double sigma)
{
    return m + Gauss(sigma);
}
