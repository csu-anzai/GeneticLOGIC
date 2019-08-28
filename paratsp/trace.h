/* $Id: trace.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : trace.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_TRACE_H
#define TSP_TRACE_H


#ifdef TRACE_1

#include <stdio.h>

#ifdef PARIX
#define trace(s) if (TraceFlag) { printf("Proc[#%d] ", MyProcID); \
  printf(s); printf("\n"); fflush(stdout); }
#else
#define trace(s) if (TraceFlag) { printf(s); printf("\n"); fflush(stdout); }
#endif

#else

#ifdef USE_PROTO
extern void trace(char *);
#else
extern void trace();
#endif

#endif


#endif


/*** end of file ***/
