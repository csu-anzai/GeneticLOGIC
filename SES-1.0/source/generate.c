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
#include <stdlib.h>
#include "es.h"


#define BASE(i) (base +(i)*size)

void 
sort(void *base, int n, int size, int (*cmp) ())
{
    int i, j;
    int ready = 0;
    char *tmp = (char *) malloc(size);

    for (i = 0; !ready && i < n - 1; i++) {
	ready = 1;
	for (j = n - 1; j > i; j--) {
	    if (cmp(BASE(j), BASE(j - 1)) < 0) {
		memcpy(tmp, BASE(j), size);
		memcpy(BASE(j), BASE(j - 1), size);
		memcpy(BASE(j - 1), tmp, size);
		ready = 0;
	    }
	}
    }
    free(tmp);
}

/*
 * for debugging purposes only
 */
void 
showpop(Field * f, char *s)
{
    int i;

    fprintf(stderr, "==================== %s\n", s);
    fprintf(stderr, "%5d: (%3d%c%3d) %5d %5d\n",
	    f->genno, f->mu, f->select == plus ? '+' : ',',
	    f->lambda, f->vars, f->sigmas);
    fprintf(stderr, "--------------------\n");
    for (i = 0; i < f->mu; i++)
	fprintf(stderr, "%d %10g %06X %06X\n",
		f->pop[i].fit.valid,
		f->pop[i].fit.value,
		f->pop[i].x,
		f->pop[i].sigma);
    fprintf(stderr, "--------------------\n");
    for (; i < f->mu + f->lambda; i++)
	fprintf(stderr, "%d %10g %06X %06X\n",
		f->pop[i].fit.valid,
		f->pop[i].fit.value,
		f->pop[i].x,
		f->pop[i].sigma);
}

int 
indcmp(Individual * a, Individual * b)
{
    if (a->fit.valid == b->fit.valid)
	return (a->fit.value < b->fit.value ? -1 : 1);
    else
	return (a->fit.valid ? -1 : 1);
}

void 
generate(Field * f)
{
    int i;
    Individual tmp, *child;

    child = f->pop + f->mu;

    for (i = 0; i < f->lambda; i++, child++) {
	crossover(f, child);
	mutate(f, child);
	child->fit = eval(child->x, f->vars);
    }

    if (f->select == plus) {
	sort(f->pop, f->mu + f->lambda, sizeof(Individual), indcmp);
    } else {
	sort(f->pop + f->mu, f->lambda, sizeof(Individual), indcmp);
	for (i = 0; i < f->mu; i++) {
	    tmp = f->pop[i];
	    f->pop[i] = f->pop[f->mu + i];
	    f->pop[f->mu + i] = tmp;
	}
    }
}
