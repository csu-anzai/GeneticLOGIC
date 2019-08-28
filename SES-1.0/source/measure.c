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
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "es.h"


#define SQR(x) ((x)*(x))

static Individual king;

static fname[256];

int 
better(Individual * a, Individual * b)
{
    if (a->fit.valid == b->fit.valid)
	return (a->fit.value < b->fit.value);
    else
	return (a->fit.valid);
}

void 
showy(Field * f)
{
    static FILE *gpcmd = NULL;

    if (gpcmd == NULL) {
	gpcmd = popen("gnuplot", "w");
	fprintf(gpcmd, "set term x11\n");

#if !defined(GP_NOLOG)
	fprintf(gpcmd, "set logscale y\n");
#endif
    }
    fprintf(gpcmd, "plot '%s' using 2:6 title 'best' with lines, "
	    "'%s' using 2:8 title 'worst' with lines\n",
	    f->logfile, f->logfile);
    fflush(gpcmd);
}

void 
showxs(Field * f)
{
    static FILE *gpcmdx = NULL;
    static FILE *gpcmds = NULL;

    FILE *gpdatx;
    FILE *gpdats;
    int i, j, k;

    double x;

    double s_min, s_max;
    double x_min, x_max;


    if (gpcmdx == NULL) {
	gpcmdx = popen("gnuplot -geometry 300x180-0-240", "w");
	fprintf(gpcmdx, "set term x11\n");

#if !defined(GP_NOLOG)
	fprintf(gpcmdx, "set logscale y\n");
#endif

	gpcmds = popen("gnuplot -geometry 300x180-0-0", "w");
	fprintf(gpcmds, "set term x11\n");
	fprintf(gpcmds, "set logscale y\n");
    }
    gpdatx = fopen("xvals.dat", "w");
    gpdats = fopen("sigma.dat", "w");

    for (k = 0; k < f->vars; k++) {
	if (k < f->sigmas)
	    s_min = s_max = f->pop[0].sigma[k];
	x_min = x_max = fabs(f->pop[0].x[k]);
	for (i = 0; i < f->mu; i++) {
	    if (k < f->sigmas) {
		if (f->pop[i].sigma[k] < s_min) {
		    s_min = f->pop[i].sigma[k];
		}
		if (f->pop[i].sigma[k] > s_max) {
		    s_max = f->pop[i].sigma[k];
		}
	    }

#if !defined(GP_NOLOG)
	    x = fabs(f->pop[i].x[k]);
#else
	    x = f->pop[i].x[k];
#endif

	    if (x < x_min) {
		x_min = x;
	    }
	    if (x > x_max) {
		x_max = x;
	    }
	}
	fprintf(gpdatx, "%3d %lf\n%3d %lf\n\n", k + 1, x_min, k + 1, x_max);
	fprintf(gpdats, "%3d %lf\n%3d %lf\n\n", k + 1, s_min, k + 1, s_max);
    }
    fclose(gpdatx);
    fclose(gpdats);

#if !defined(GP_NOLOG)
    fprintf(gpcmdx, "plot [0:%d] 'xvals.dat' title 'abs(x[i])' with lines\n", f->vars + 1);
#else
    fprintf(gpcmdx, "plot [0:%d] 'xvals.dat' title 'x[i]' with lines\n", f->vars + 1);
#endif

    fprintf(gpcmds, "plot [0:%d] 'sigma.dat' title 'sigma[i]' with lines\n", f->vars + 1);
    fflush(gpcmdx);
    fflush(gpcmds);
}

int
measure(Field * f)
{
    Individual *current, *best, *worst;
    int i, j, k;
    double avg;
    double s_min, s_max;

    s_min = s_max = f->pop[0].sigma[0];
    best = worst = &(f->pop[0]);
    avg = f->pop[0].fit.value;
    for (i = 1; i < f->mu; i++) {

	current = &(f->pop[i]);
	if (better(current, best))
	    best = current;
	if (better(worst, current))
	    worst = current;
	avg += current->fit.value;

	for (k = 0; k < f->sigmas; k++) {
	    if (f->pop[i].sigma[k] > s_max) {
		s_max = f->pop[i].sigma[k];
	    }
	    if (f->pop[i].sigma[k] < s_min) {
		s_min = f->pop[i].sigma[k];
	    }
	}
    }
    f->calls += f->mu + f->lambda;
    avg = avg / f->mu;

    if (f->genno == 0 || better(best, &king)) {
	king = *best;
	fprintf(bstfile, "%d %d %g\t", f->genno, king.fit.valid, king.fit.value);
	for (i = 0; i < f->vars; i++)
	    fprintf(bstfile, " %g", king.x[i]);
	fprintf(bstfile, "\n");
    }
    fprintf(logfile, "%d\t%d\t%g\t%g\t%d\t%g\t%d\t%g\n",
	    f->genno,
	    f->calls,
	    s_min, s_max,
	    best->fit.valid, best->fit.value,
	    worst->fit.valid, worst->fit.value);



    /* If there was an improvement dump the best individual so far
   */

    if (f->flushlog && (f->genno + 1) % f->flushlog == 0) {
	fflush(logfile);
	if (f->showy)
	    showy(f);
    }
    if (f->showxs && (f->genno + 1) % f->showxs == 0)
	showxs(f);
}
