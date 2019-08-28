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
#include <unistd.h>
#include <time.h>

#include "es.h"

#include "pixmon.h"

extern unsigned char scale2color();

void
dump(Field * f)
{
    FILE *dmp;
    int i, k;
    static char dumpname[256];
    static char command[256];
    static int isinit = 0;
    int size;
    static unsigned char *map = NULL;
    static FILE *pixmon;

    if (f->pixmon) {
	ImgHdr hdr;

	if (map == NULL) {
	    map = (unsigned char *) calloc(f->mu, sizeof(unsigned char));

	    if (map == NULL) {
		fprintf(stderr, "%s: not enough memory for images: "
			"pixmon-option ingnored", f->name);
		f->pixmon = 0;
	    } else {
		sprintf(command, 
		 "pixmon -geometry -0+0 -width 300 -height 400 -dx %d -dy %d",
			f->mu, f->maxgen + 1);
		pixmon = popen(command, "wb");
		if (pixmon == NULL) {
		    fprintf(stderr, "%s: pixmon not found:"
			    "pixmon-option ingnored", f->name);
		    f->pixmon = 0;
		}
	    }
	}
	INIT_IMGHDR(hdr);
	hdr.type = PIX_DATA | PIX_FLUSH;

	hdr.x = 0;
	hdr.y = f->genno;

	hdr.dx = f->mu;
	hdr.dy = f->maxgen - f->genno + 1;

	for (i = 0; i < f->mu; i++) {
	    map[i] = scale2color(&f->pop[i], f->vars);
	}
	size = hdr.dy * f->mu;
	hdr.sizelo = size % 0x10000;
	hdr.sizehi = size / 0x10000;

	if (fwrite(&hdr, sizeof(hdr), 1, pixmon) != 1)
	    fprintf(stderr, "%s: got trouble writing the PIXMON-header...\n", 
		f->name);
	for (i = 0; i < hdr.dy; i++)
	    if (fwrite(map, f->mu, 1, pixmon) != 1)
		fprintf(stderr, "%s: got trouble writing the PIXMON-data...\n",
		    f->name);
	fflush(pixmon);
    }
    if (f->dmpint == 0 || f->genno % f->dmpint != 0)
	return;

    sprintf(dumpname, "%s.dmp", f->name);
    if (!isinit) {
	dmp = fopen(dumpname, "w");
	isinit = 1;
    } else {
	dmp = fopen(dumpname, "a");
    }

    for (i = 0; i < f->mu; i++) {
	fprintf(dmp, "%4d %d %4d %g\t",
		f->genno, i, f->pop[i].fit.valid, f->pop[i].fit.value);

	for (k = 0; k < f->vars; k++) {
	    fprintf(dmp, " %g", f->pop[i].x[k]);
	}
	fprintf(dmp, "\n\t\t\t");

	for (k = 0; k < f->sigmas; k++) {
	    fprintf(dmp, " %g", f->pop[i].sigma[k]);
	}
	fprintf(dmp, "\n\n");
    }
    fclose(dmp);
}
