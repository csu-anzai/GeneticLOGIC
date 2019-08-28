/* $Id: brselect.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : brselect.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_BRSELECT_H
#define TSP_BRSELECT_H

#include "define.h"


#ifdef USE_PROTO
extern int num_offsprings(void);
extern void bs_inv_roulette(POPULATION *, int *, int);
extern void bs_gauss(POPULATION *, int *, int); 
extern void bs_random(POPULATION *, int *, int);
extern void bs_proportional(POPULATION *, int *, int);
extern void bs_whitley(POPULATION *, int *, int);
extern void bs_linear_rank(POPULATION *, int *, int);
extern void bs_inv_linear_rank(POPULATION *, int *, int);
extern void bs_boltzmann(POPULATION *, int *, int);
extern void breeder_select(POPULATION *, int *, int);
#else
extern int num_offsprings();
extern void bs_inv_roulette();
extern void bs_gauss(); 
extern void bs_random();
extern void bs_proportional();
extern void bs_whitley();
extern void bs_linear_rank();
extern void bs_inv_linear_rank();
extern void bs_boltzmann();
extern void breeder_select();
#endif


#endif


/*** end of file ***/
