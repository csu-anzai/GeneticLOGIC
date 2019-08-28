/* $Id: boltzman.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : boltzman.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_BOLTZMAN_H
#define TSP_BOLTZMAN_H

#include "define.h"


#ifdef USE_PROTO
extern int boltzmann_select(POPULATION *, int *);
#else
extern int boltzmann_select();
#endif


#endif


/*** end of file ***/
