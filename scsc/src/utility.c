
/* utility.c -- utility procedures and functions

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

#ifndef lint
static char *rcsid = "$Id: utility.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"


/*
 *    poweri -- compute x^i
 */
double
poweri (x, i)
     double x;
     int i;
{
  int j;
  double powertmp = D_ONE;

  if (i == 0)
    powertmp = D_ONE;
  else if (i > 0)
    for (j = i; j > 0; j--)
      powertmp *= x;
  else
    for (j = i; j < 0; j++)
      powertmp /= x;

  return ((double) powertmp);
}

/*
 *    my_max -- returns max of x and y
 */
double
my_max (x, y)
     double x, y;
{
  if (x > y)
    return ((double) x);
  else
    return ((double) y);
}

/*
 *    my_min -- returns minimum of x and y
 */
double
my_min (x, y)
     double x, y;
{
  if (x < y)
    return ((double) x);
  else
    return ((double) y);
}

/*
 *    avg -- returns average of two real values
 */
double
avg (x, y)
     double x, y;
{
  return ((double) (D_HALF * (x + y)));
}

/*
 *    round -- returns integer part of real value (see also round.c)
 */
int
round (v)
     double v;
{
  if ((v - (int) v) >= D_HALF)
    return ((int) (v + D_ONE));
  else
    return ((int) v);
}

/*
 *    readln -- read ONE value from FILE fp, and skip to next line
 */
int
readln (fp, fmt, v)
     FILE *fp;
     char *fmt;
     void *v;
{
  int c;

  if (fscanf (fp, fmt, v) != 1)
    return (-1);		/* failure */

  /* skip anything until eoln */
  while ((c = fgetc (fp)) != EOF && c != '\n');

  return (0);
}

/*
 *    halt -- test for keypress and query for halt flag
 */
boolean
halt ()
{
  char c;

  if (KEYPRESSED)
    {
      printf ("Halt (y/n)? ");
      readln (stdin, "%c", &c);
      if ((c == 'y') || (c == 'Y'))
	return (TRUE);
      else
	return (FALSE);
    }
  return (FALSE);
}
