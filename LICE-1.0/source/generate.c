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
 *	generate.c
 *	----------
 *	
 *	Generate a new generation (what else ?)
 */

#include <time.h>
#include <stdlib.h>
#include "lice.h"


void
icopy(Individual * target, Individual * source, int n_sigma, int nvars)
{
    target->fit = source->fit;
    memcpy(target->sigma, source->sigma, n_sigma * sizeof(double));
    memcpy(target->x, source->x, nvars * sizeof(double));
}



void
generate(Field * f)
{
    int i, k;
    Population tmp;
    static Individual child;

    Trace("generate");

    if (child.x == NULL) {
	child.x = (double *) calloc(f->vars, sizeof(double));
	child.sigma = (double *) calloc(f->sigmas, sizeof(double));

	if (child.x == NULL || child.sigma == NULL) {
	    fprintf(stderr, "malloc failed in %s\n", __FILE__);
	    exit(1);
	}
    }
    tmp = f->new;
    f->new = f->old;
    f->old = tmp;

    for (i = 0; i < f->popsize; i++) {
	if (f->select == plus) {
	    icopy(&f->new[i], &f->old[i], f->sigmas, f->vars);
	}
	for (k = 0; k < f->children; k++) {
	    crossover(f, &child, i);
	    mutate(f, &child);
	    child.fit = eval(child.x, f->vars);
	    f->calls++;
	    if ((f->select != plus && k == 0) || better(&child, &f->new[i]))
		icopy(&f->new[i], &child, f->sigmas, f->vars);
	}
    }
}
