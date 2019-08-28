
/* io.c -- input/output handling routines

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
static char *rcsid = "$Id: io.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"


/*
 *    page -- flush to top of form (form feed)
 */
void
page (fp)
     FILE *fp;
{
  fprintf (fp, "%c\n", FORMFEED);
}

/*
 *    open_input -- open an input stream to a named file
 */
FILE *
open_input (batchflag, message, filename)
     int batchflag;
     char *message;
     char *filename;
{
  FILE *input = NULL;

  if (batchflag)
    {
      if ((input = fopen (filename, "r")) == NULL)
	{
	  panic (E_FATAL, "open_input", "can't open `%s'", filename);
	}
    }
  else
    {
      while (input == NULL)
	{
	  printf ("Enter %s filename: ", message);

	  if (readln (stdin, "%s", filename))
	    panic (E_WARN, "open_input", "readln: %s", "can't read input filename");

	  if ((input = fopen (filename, "r")) == NULL)
	    {
	      panic (E_WARN, "open_input", "can't open `%s'", filename);
	    }
	}
    }
  return (input);
}

/*
 *    open_output -- open an output stream to a named file
 */
FILE *
open_output (batchflag, message, filename)
     int batchflag;
     char *message;
     char *filename;
{
  FILE *output = NULL;

  if (batchflag)
    {
      if ((output = fopen (filename, "w")) == NULL)
	{
	  panic (E_FATAL, "open_output", "can't open `%s'", filename);
	}
    }
  else
    {
      while (output == NULL)
	{
	  printf ("Enter %s filename: ", message);

	  if (readln (stdin, "%s", filename))
	    panic (E_WARN, "open_output", "readln: %s", "can't read output filename");

	  if ((output = fopen (filename, "w")) == NULL)
	    {
	      panic (E_WARN, "open_output", "can't open `%s'", filename);
	    }
	}
    }
  return (output);
}
