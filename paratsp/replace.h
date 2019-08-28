/* $Id: replace.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : replace.h                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_REPLACE_H
#define TSP_REPLACE_H

#include "define.h"


#ifdef USE_PROTO
extern void re_random(POPULATION *, POPULATION *, BIT *);
extern void re_crowding(POPULATION *, POPULATION *, BIT *);
extern void replace(POPULATION *, POPULATION *, BIT *);
#else
extern void re_random();
extern void re_crowding();
extern void replace();
#endif


#endif


/*** end of file ***/
