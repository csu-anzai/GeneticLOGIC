/* $Id: chrom.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : chrom.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_CHROM_H
#define TSP_CHROM_H

#include "define.h"


#ifdef USE_PROTO
extern void order_copy(CHROM *, CHROM *);
extern BOOLEAN order_comp(POPULATION *, int, CHROM *);
extern POPULATION *alloc_pop(int);
extern void free_pop(POPULATION *);
extern BESTCHROM *alloc_best(int);
extern void free_best(BESTCHROM *);
#else
extern void order_copy();
extern BOOLEAN order_comp();
extern POPULATION *alloc_pop();
extern void free_pop();
extern BESTCHROM *alloc_best();
extern void free_best();
#endif


#endif


/*** end of file ***/
