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
 *	mutate.c
 *	--------
 *
 *	standard ES mutation
 */

#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "lice.h"

void
mutate(Field * f, Individual * current)
{
    int i, j;
    double alpha;

    alpha = normrand(0.0, f->tau0);

    for (i = 0; i < f->sigmas; i++) {
	current->sigma[i] *= exp(alpha + normrand(0.0, f->taui));
	if (f->smax && current->sigma[i] > f->smax)
	    current->sigma[i] = f->smax;
	else if (f->smin && current->sigma[i] < f->smin)
	    current->sigma[i] = f->smin;
    }

    for (i = j = 0; i < f->vars; i++) {
	current->x[i] = normrand(current->x[i], current->sigma[j]);
	if (f->sigmas > 1)
	    j++;
    }
}
