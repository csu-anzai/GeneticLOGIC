/*
 *	SES - Simple Evolution Strategy
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
 *	See README for details of ES.
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

/*	cross.c
 *	-------
 *
 */

#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "es.h"

void
reco_discrete(double *res, double *x1, double *x2, int n)
{
    int i;

    for (i = 0; i < n; i++)
	if (rand() < (RAND_MAX / 2))
	    res[i] = x1[i];
	else
	    res[i] = x2[i];
}


void
reco_xs_discrete(double *xres, double *sres,
		 double *x1, double *x2,
		 double *s1, double *s2,
		 int xn, int sn)
{
    int i;

    for (i = 0; i < xn; i++) {
	if (rand() < (RAND_MAX / 2)) {
	    xres[i] = x1[i];
	    if (i < sn)
		sres[i] = s1[i];
	} else {
	    xres[i] = x2[i];
	    if (i < sn)
		sres[i] = s2[i];
	}
    }
}

void
reco_arith_inter(double *res, double *x1, double *x2, int n)
{
    int i;

    for (i = 0; i < n; i++)
	res[i] = 0.5 * (x1[i] + x2[i]);
}

void
reco_geom_inter(double *res, double *x1, double *x2, int n)
{
    int i;

    for (i = 0; i < n; i++)
	res[i] = sqrt(x1[i] * x2[i]);
}


void
crossover(Field * f, Individual * child)
{
    int i;
    Individual *parent1, *parent2;

    parent1 = &f->pop[rand() % f->mu];
    parent2 = &f->pop[rand() % f->mu];

    if (f->reco_x == discrete && f->reco_s == discrete) {
	reco_xs_discrete(child->x, child->sigma,
			 parent1->x, parent2->x,
			 parent1->sigma, parent2->sigma,
			 f->vars, f->sigmas);
    } else {

	switch (f->reco_x) {
	case discrete:
	    reco_discrete(child->x, parent1->x, parent2->x, f->vars);
	    break;
	case inter_a:
	    reco_arith_inter(child->x, parent1->x, parent2->x, f->vars);
	    break;
	case none:
	default:
	    memcpy(child->x, parent1->x, f->vars * sizeof(double));

	    break;
	}

	switch (f->reco_s) {
	case discrete:
	    reco_discrete(child->sigma,
			  parent1->sigma, parent2->sigma, f->sigmas);
	    break;
	case inter_a:
	    reco_arith_inter(child->sigma,
			     parent1->sigma, parent2->sigma, f->sigmas);
	    break;
	case inter_g:
	    reco_geom_inter(child->sigma,
			    parent1->sigma, parent2->sigma, f->sigmas);
	    break;
	case none:
	default:
	    memcpy(child->sigma, parent1->sigma, f->sigmas * sizeof(double));

	    break;
	}
    }
}
