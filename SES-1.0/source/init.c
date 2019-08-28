/*
 *      SES - Simple Evolution Strategy
 *
 *      Copyright (C) 1994 Joachim Sprave
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *      See README for details of ES.
 *      Send bugs (better: bug descriptions) and comments to
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
#include <time.h>
#include <string.h>
#include <stdlib.h>


#include "es.h"

void
initpop(Field * f)
{
    int i, k;

    f->genno = f->calls = 0;

    for (i = 0; i < f->mu + f->lambda; i++) {


	for (k = 0; k < f->vars; k++) {
	    f->pop[i].x[k] = f->min_init_x
		+ randreal() * (f->max_init_x - f->min_init_x);
	}
	for (k = 0; k < f->sigmas; k++) {
	    f->pop[i].sigma[k] = f->min_init_s
		+ randreal() * (f->max_init_s - f->min_init_s);
	}
	f->pop[i].fit = eval(f->pop[i].x, f->vars);
	f->calls++;
    }
}


Population
newpop(int size, int n_sigma, int nvars)
{
    Population h;
    int i;

    h = (Individual *) calloc(size, sizeof(Individual));
    if (h == NULL) {
	fprintf(stderr, "malloc failed in %s\n", __FILE__);
	exit(1);
    }
    for (i = 0; i < size; i++) {
	h[i].x = (double *) calloc(nvars, sizeof(double));
	h[i].sigma = (double *) calloc(n_sigma, sizeof(double));

	if (h[i].sigma == NULL) {
	    fprintf(stderr, "malloc failed in %s\n", __FILE__);
	    exit(1);
	}
    }

    return (h);
}
