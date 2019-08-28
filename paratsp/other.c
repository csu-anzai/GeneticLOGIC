/* $Id: other.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : other.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef PARIX
#include <malloc.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "trace.h"
#include "other.h"


/*****************************************************************************/
/* Extended malloc                                                           */
/*****************************************************************************/
char *emalloc(n, err)
  unsigned long n;	/* size of allocated memory */
  BOOLEAN err;		/* error flag */
{ char *p = NULL;

  trace("emalloc() entered");

  p = (char *) malloc(n);

  if (err && (p == NULL))
  { sprintf(Msg, "EMalloc: Out of Memory... (No space available)");
    critical_error(ERR_OUT_OF_MEMORY, Msg);
  }

  trace("emalloc() completed");

  return(p);
}


/*****************************************************************************/
/* Faculty function                                                          */
/*****************************************************************************/
double facul(n)
  int n;		/* argument */
{ register int i;
  BOOLEAN ok = TRUE;
  double res = 1.0;

  trace("facul() entered");

  if (n < 0)
  { res = -1.0;
  }
  else
  { for (i = 2; (i <= n) && ok; i++)
    { if (res < MAXDOUBLE / i)
      { res *= i;
      }
      else
      { ok = FALSE;
        res = 0.0;
      }
    }
  }

  trace("facul() completed");

  return(res);
}


/*** end of file ***/
