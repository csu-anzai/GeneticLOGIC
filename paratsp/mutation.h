/* $Id: mutation.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : mutation.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_MUTATION_H
#define TSP_MUTATION_H

#include "define.h"


#ifdef USE_PROTO
extern void move_mutation(CHROM *, TOUR, TOUR);
extern void swap_mutation(CHROM *, TOUR, TOUR);
extern void invert_mutation(CHROM *, TOUR, TOUR);
extern void mu_constant_move_mutation(CHROM *);
extern void mu_constant_swap_mutation(CHROM *);
extern void mu_constant_invert_mutation(CHROM *);
extern void mu_adapted_move_mutation(CHROM *);
extern void mu_adapted_swap_mutation(CHROM *);
extern void mu_adapted_invert_mutation(CHROM *);
extern void mu_constant_rand_mutation(CHROM *);
extern void mu_adapted_rand_mutation(CHROM *);
extern void mu_constant_localopt_mutation(CHROM *);
extern void mu_adapted_localopt_mutation(CHROM *);
extern void mu_constant_randall_mutation(CHROM *);
extern void mu_adapted_randall_mutation(CHROM *);
extern void mutation(CHROM *);
#else
extern void move_mutation();
extern void swap_mutation();
extern void invert_mutation();
extern void mu_constant_move_mutation();
extern void mu_constant_swap_mutation();
extern void mu_constant_invert_mutation();
extern void mu_adapted_move_mutation();
extern void mu_adapted_swap_mutation();
extern void mu_adapted_invert_mutation();
extern void mu_constant_rand_mutation();
extern void mu_adapted_rand_mutation();
extern void mu_constant_localopt_mutation();
extern void mu_adapted_localopt_mutation();
extern void mu_constant_randall_mutation();
extern void mu_adapted_randall_mutation();
extern void mutation();
#endif


#endif


/*** end of file ***/
