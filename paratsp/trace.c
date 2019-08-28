/* $Id: trace.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : trace.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "trace.h"


/*****************************************************************************/
/* Trace function                                                            */
/*****************************************************************************/
#ifndef TRACE_1
void trace(s)
  char *s;		/* trace message */
{
  if (TraceFlag)
  {
#ifdef PARIX
    printf("Proc[#%d] ", MyProcID);
#endif
    printf(s);
    printf("\n");
    fflush(stdout);
#ifdef TRACE_3
    getchar();
#endif
  }
}
#endif


/*** end of file ***/
