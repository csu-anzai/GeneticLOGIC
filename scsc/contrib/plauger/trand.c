
/* arand.c -- test J.P. Plauger's ANSI C random number generator

   Copyright (C) 1993 Joerg Heitkoetter

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/* $Id: arand.c,v 1.1 1993/02/04 11:33:40 joke Exp $ */

#include <stdio.h>
#include "random.h"

int
main (argc, argv)
    int argc;
    char **argv;
{
    unsigned numbers = 1;
    unsigned i;

    if (argc == 2)
	numbers = atoi (argv[1]);

    for (i = 0; i < numbers; i++) {

#ifdef INTEGER
	printf ("%8d", rand ());
#endif

#ifdef REAL
	printf ("%8.5lf", (double)rand ()/(double)RAND_MAX);
#endif

#ifndef TEST
	if ((i+1) % 10 == 0)
#endif

	    putc ('\n', stdout);
    }

    putc ('\n', stdout);

    return 0;
}
