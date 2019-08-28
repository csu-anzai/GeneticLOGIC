
/* round.c -- sample program of PASCAL equivalent round()

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

/* compile this with: ``cc -DTEST -o round round.c'' to get a working
   demo program running */

#include <stdio.h>
extern double atof ();

int
round (val)
    double val;
{
    char buf[80];

    sprintf (buf, "%.0lf", val);	/* rounding: 0..5 -> 0, 6..9 -> 1 */
    return ((int) atof (buf));
}

int
round2 (val)
    double val;
{
    if ((val - (int) val) >= (double) 0.5)
	return ((int) (val + (double) 1.0));
    else
	return ((int) val);
}

#ifdef TEST
main (argc, argv)
    int argc;
    char **argv;
{
    while (--argc) {
	printf ("round(%s)  = %d\n", argv[argc], round (atof (argv[argc])));
	printf ("round2(%s) = %d\n", argv[argc], round2 (atof (argv[argc])));
    }
    return (0);
}
#endif
