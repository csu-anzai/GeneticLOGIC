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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>


#include "es.h"
#include "getparam.h"

param_t param[] =
{
#include "params.h"
};

FILE *logfile;
FILE *bstfile;

static Field home;


int 
main(int argc, char **argv)
{
    char fname[20];
    char line[80];



    home.seed = getpid();
    home.dmpint = 0;
    home.flushlog = 0;
    home.maxcalls = 0;

    sprintf(home.logfile, "");

    getparam(&home, param, argv);

    fprintf(stderr, "seed = %d\n", home.seed);
    initrandom(home.seed);

    if (strcmp(home.logfile, "stdout") == 0) {
	logfile = stdout;
    } else {
	if (strcmp(home.logfile, "") == 0) {
	    sprintf(home.logfile, "%s.log", home.run);
	}
	if ((logfile = fopen(home.logfile, "w")) == NULL) {
	    fprintf(stderr, "cannot open %s for writing\n",
		    home.logfile);
	    exit(1);
	}
    }
    sprintf(fname, "%s.bst", home.run);
    if ((bstfile = fopen(fname, "w")) == NULL) {
	fprintf(stderr, "cannot open %s for writing\n", fname);
	exit(1);
    }
    if (home.sigmas)
	home.sigmas = home.vars;
    else
	home.sigmas = 1;

    home.pop = newpop(home.mu + home.lambda, home.sigmas, home.vars);

    cycle(&home);


    fclose(logfile);
    fclose(bstfile);

    return (EXIT_SUCCESS);
}
