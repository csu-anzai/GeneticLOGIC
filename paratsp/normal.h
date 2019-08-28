/* $Id: normal.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : normal.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_NORMAL_H
#define TSP_NORMAL_H

#include "define.h"


#ifdef USE_PROTO
extern void normalize(CHROM *);
#else
extern void normalize();
#endif


#endif


/*** end of file ***/
