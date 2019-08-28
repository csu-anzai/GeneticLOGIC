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

/*
 *	init.c
 *	------
 *
 *	initialize the population structures.
 */

#include <time.h>
#include <string.h>
#include <stdlib.h>


#include "lice.h"

void
initpop(Field * f)
{
    int i, k;

    Trace("initpop");
    f->genno = f->calls = 0;
    for (i = 0; i < f->popsize; i++) {


	for (k = 0; k < f->vars; k++) {
	    f->new[i].x[k] = f->old[i].x[k] =
		f->min_init_x + randreal() * (f->max_init_x - f->min_init_x);
	}
	for (k = 0; k < f->sigmas; k++) {
	    f->new[i].sigma[k] = f->old[i].sigma[k] =
		f->min_init_s + randreal() * (f->max_init_s - f->min_init_s);
	}
	f->new[i].fit = f->old[i].fit = eval(f->new[i].x, f->vars);
	f->calls++;
    }
    if ((f->best.x = (double *) calloc(f->vars, sizeof(double))) == NULL) {
	fprintf(stderr, "malloc failed in %s\n", __FILE__);
	exit(1);
    }
    if ((f->best.sigma = (double *) calloc(f->sigmas, sizeof(double))) == NULL) {
	fprintf(stderr, "malloc failed in %s\n", __FILE__);
	exit(1);
    }
}




Population
newpop(int size, int n_sigma, int nvars)
{
    Population h;
    int i;

    Trace("newpop");
    h = (Individual *) calloc(size, sizeof(Individual));
    if (h == NULL) {
	fprintf(stderr, "malloc failed in %s\n", __FILE__);
	exit(1);
    }
    for (i = 0; i < size; i++) {
	h[i].x = (double *) calloc(nvars, sizeof(double));
	h[i].sigma = (double *) calloc(n_sigma, sizeof(double));
    }

    return (h);
}
