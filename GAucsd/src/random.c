
/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "define.h"

long Rstate[RAND_DEG] = {   0x9a319039,
	0x32d9c024, 0x9b663182, 0x5da1f342, 0xde3b81e0, 0xdf0a6fb5,
	0xf103bc02, 0x48f340fb, 0x7449e56b, 0xbeb1dbb0, 0xab5c5918,
	0x946554fd, 0x8c2e680f, 0xeb3d799f, 0xb11ee0b7, 0x2d436b86,
	0xda672e2a, 0x1588ca88, 0xe369735d, 0x904f35f7, 0xd7158fd6,
	0x6fa6f051, 0x616e6b96, 0xac94efdc, 0x36413f93, 0xc622c298,
	0xf5a42ab8, 0x8a88d77b, 0xf5ad9d0e, 0x8999220b, 0x27fb47b9
};

static long *fptr = &Rstate[3];
static long *rptr = &Rstate[0];
static long *end_ptr = &Rstate[RAND_DEG];

int Getptr()
{
	return(fptr - &Rstate[0]);
}

Setptr(p) int p;
{
	fptr = &Rstate[p];
	rptr = fptr - 3;
	if (rptr < &Rstate[0]) rptr += RAND_DEG;
}

Srand(x) unsigned int *x;
{
	register int i;
	double Rand();

	Rstate[0] = (long) *x;
	for (i = 1; i < RAND_DEG; i++)
		Rstate[i] = 1103515245 * Rstate[i - 1] + 12345;
	*x = (unsigned int) (1103515245 * Rstate[RAND_DEG - 1] + 12345);

	fptr = &Rstate[3];
	rptr = &Rstate[0];

	for (i = 0; i < 10*RAND_DEG; i++) (void) Rand();
}

double Rand()
{
	long i;
	
	*fptr += *rptr;
	i = (*fptr >> 1) & LONG_MAX;
	if (++fptr >= end_ptr)
	{
		fptr = &Rstate[0];
		++rptr;
	}
	else
		if (++rptr >= end_ptr) rptr = &Rstate[0];
	return(i/(LONG_MAX + 1.0));
}

