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
 *	measure.c
 *	---------
 *	evaluate new individuals, calculate averages, dump
 *	useful information to the log-file and bst-file.
 *	If <showxs> > 0 or <showy> > 0 invoke gnuplot (must
 *	be installed separately). When gnuplot dumps core
 *	file set <showxs> and/or <showy> to larger values.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include "lice.h"


#define SQR(x) ((x)*(x))

static fname[256];

int
better(Individual * a, Individual * b)
{
    if (a->fit.valid == b->fit.valid)
	return (a->fit.value < b->fit.value);
    else
	return (a->fit.valid);
}

int
equal(Individual * a, Individual * b)
{
    return a->fit.valid == b->fit.valid && a->fit.value == b->fit.value;
}

void
showy(Field * f)
{
    static FILE *gpcmd = NULL;

    if (gpcmd == NULL) {
	gpcmd = popen("gnuplot", "w");
	fprintf(gpcmd, "set term x11\n");
	fprintf(gpcmd, "set logscale y\n");
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
	fprintf(gpcmdx, "set logscale y\n");
	gpcmds = popen("gnuplot -geometry 300x180-0-0", "w");
	fprintf(gpcmds, "set term x11\n");
	fprintf(gpcmds, "set logscale y\n");
    }
    gpdatx = fopen("xvals.dat", "w");
    gpdats = fopen("sigma.dat", "w");

    for (k = 0; k < f->vars; k++) {
	if (k < f->sigmas)
	    s_min = s_max = f->new[0].sigma[k];
	x_min = x_max = fabs(f->new[0].x[k]);
	for (i = 0; i < f->popsize; i++) {
	    if (k < f->sigmas) {
		if (f->new[i].sigma[k] < s_min) {
		    s_min = f->new[i].sigma[k];
		}
		if (f->new[i].sigma[k] > s_max) {
		    s_max = f->new[i].sigma[k];
		}
	    }
	    x = fabs(f->new[i].x[k]);
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
    fprintf(gpcmdx, "plot [0:%d] 'xvals.dat' title 'abs(x[i])' with lines\n", f->vars + 1);
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


    s_min = s_max = f->new[0].sigma[0];
    best = worst = &(f->new[0]);
    avg = f->new[0].fit.value;
    for (i = 1; i < f->popsize; i++) {

	current = &(f->new[i]);
	if (better(current, best))
	    best = current;
	if (better(worst, current))
	    worst = current;
	avg += current->fit.value;

	for (k = 0; k < f->sigmas; k++) {
	    if (f->new[i].sigma[k] > s_max) {
		s_max = f->new[i].sigma[k];
	    }
	    if (f->new[i].sigma[k] < s_min) {
		s_min = f->new[i].sigma[k];
	    }
	}
    }
    f->calls += f->popsize * f->children;
    avg = avg / f->popsize;

    if (f->genno == 0 || better(best, &f->best)) {
	icopy(&f->best, best, f->sigmas, f->vars);
	fprintf(bstfile, "%d %d %g\t", f->genno, f->best.fit.valid, f->best.fit.value);
	for (i = 0; i < f->vars; i++)
	    fprintf(bstfile, " %g", f->best.x[i]);
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
