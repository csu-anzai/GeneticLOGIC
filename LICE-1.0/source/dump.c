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
 *	dump.c
 *	------
 *
 *	dump the entire population every <dumpint> generations.
 *	<dumpint> = 0 means no dump at all.
 *	Additionally, produce pixmon output if pixmon is actived
 *	and available.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "lice.h"

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
	    map = (unsigned char *) calloc(f->popsize, sizeof(unsigned char));

	    if (map == NULL) {
		fprintf(stderr, "%s: not enough memory for images: "
			"pixmon-option ingnored", f->name);
		f->pixmon = 0;
	    } else {
		sprintf(command, "pixmon -geometry -0+0 -width %d -height %d -dx %d -dy %d",
			MIN(f->popsize,600), MIN(f->maxgen+1,400), 
			f->popsize, f->maxgen + 1);
		pixmon = popen(command, "wb");
		if (pixmon == NULL) {
		    fprintf(stderr, "%s: pixmon not found:"
			    "pixmon-option ingnored", f->name);
		    f->pixmon = 0;
		}
	    }
	    EXIT_IMGHDR(hdr);
	    fwrite(&hdr, sizeof(hdr), 1, pixmon);
	}
	INIT_IMGHDR(hdr);
	hdr.type = PIX_DATA | PIX_FLUSH;

	hdr.x = 0;
	hdr.y = f->genno;

	hdr.dx = f->popsize;

#if defined(ZOOM)
	hdr.dy = f->maxgen - f->genno + 1;
#else
	hdr.dy = 1;
#endif

	for (i = 0; i < f->popsize; i++) {
	    map[i] = scale2color(&f->new[i], f->vars);
	}
	size = hdr.dy * f->popsize;
	hdr.sizelo = size % 0x10000;
	hdr.sizehi = size / 0x10000;

	if (fwrite(&hdr, sizeof(hdr), 1, pixmon) != 1)
	    fprintf(stderr, "%s: got trouble writing the PIXMON-header...\n", f->name);
	for (i = 0; i < hdr.dy; i++)
	    if (fwrite(map, f->popsize, 1, pixmon) != 1)
		fprintf(stderr, "%s: got trouble writing the PIXMON-data...\n", f->name);
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

    for (i = 0; i < f->popsize; i++) {
	fprintf(dmp, "g: %4d %d %4d %g\n",
		f->genno, i, f->new[i].fit.valid, f->new[i].fit.value);

	fprintf(dmp, "x: ");
	for (k = 0; k < f->vars; k++) {
	    fprintf(dmp, " %g", f->new[i].x[k]);
	}
	fprintf(dmp, "\n\t\t\t");

	fprintf(dmp, "s: ");
	for (k = 0; k < f->sigmas; k++) {
	    fprintf(dmp, " %g", f->new[i].sigma[k]);
	}
	fprintf(dmp, "\n\n");
    }
    fclose(dmp);
}
