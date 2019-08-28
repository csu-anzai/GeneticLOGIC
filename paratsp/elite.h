/* $Id: elite.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : elite.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_ELITE_H
#define TSP_ELITE_H

#include "define.h"


#ifdef USE_PROTO
extern int es_random(BIT *, int);
extern int es_weakest(POPULATION *, BIT *);
extern int es_first_weaker(POPULATION *, BIT *, int, CHROM *);
extern int elite_select(POPULATION *, BIT *, int, CHROM *);
extern void elite(void);
#else
extern int es_random();
extern int es_weakest();
extern int es_first_weaker();
extern int elite_select();
extern void elite();
#endif


#endif


/*** end of file ***/
