/* $Id: other.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : other.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_OTHER_H
#define TSP_OTHER_H

#include "define.h"


#ifdef USE_PROTO
extern char *emalloc(unsigned long, BOOLEAN);
extern double facul(int);
#else
extern char *emalloc();
extern double facul();
#endif


#endif


/*** end of file ***/
