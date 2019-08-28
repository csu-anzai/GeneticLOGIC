/* $Id: best.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : best.h                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_BEST_H
#define TSP_BEST_H

#include "define.h"


#ifdef USE_PROTO
extern void copy_best(BESTCHROM *, CHROM *);
extern void save_best(CHROM *);
extern void print_best(char *, char *);
#else
extern void copy_best();
extern void save_best();
extern void print_best();
#endif


#endif


/*** end of file ***/
