/* $Id: fopen.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : fopen.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_FOPEN_H
#define TSP_FOPEN_H

#include <stdio.h>
#include "define.h"


#ifdef USE_PROTO
extern FILE *file_open(char *, char *, BOOLEAN);
#else
extern FILE *file_open();
#endif


#endif


/*** end of file ***/
