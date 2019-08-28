/* $Id: cross.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : cross.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_CROSS_H
#define TSP_CROSS_H

#include "define.h"


#ifdef USE_PROTO
extern void cr_edge_recombination(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_interval_PMX(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_goldberg_PMX(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_goldberg_OX(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_goldberg_CX(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_uniform_order_based(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_nearest_neighbour(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_cheapest_insertion(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_farthest_insertion(CHROM *, CHROM *, CHROM *, CHROM *);
extern void cr_random(CHROM *, CHROM *, CHROM *, CHROM *);
extern void crossover(CHROM *, CHROM *, CHROM *, CHROM *);
#else
extern void cr_edge_recombination();
extern void cr_interval_PMX();
extern void cr_goldberg_PMX();
extern void cr_goldberg_OX();
extern void cr_goldberg_CX();
extern void cr_uniform_order_based();
extern void cr_nearest_neighbour();
extern void cr_cheapest_insertion();
extern void cr_farthest_insertion();
extern void cr_random();
extern void crossover();
#endif


#endif


/*** end of file ***/
