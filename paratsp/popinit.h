/* $Id: popinit.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : popinit.h                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_POPINIT_H
#define TSP_POPINIT_H

#include "define.h"


#ifdef USE_PROTO
extern void in_random(CHROM *);
extern void in_tour_file(CHROM *);
extern void in_nearest_neighbour(CHROM *);
extern void in_cheapest_insertion(CHROM *);
extern void in_farthest_insertion(CHROM *);
extern void in_without_random(CHROM *);
extern void in_all_random(CHROM *);
extern void init_pop(void);
#else
extern void in_random();
extern void in_tour_file();
extern void in_nearest_neighbour();
extern void in_cheapest_insertion();
extern void in_farthest_insertion();
extern void in_without_random();
extern void in_all_random();
extern void init_pop();
#endif


#endif


/*** end of file ***/
