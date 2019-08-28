/* $Id: eval.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : eval.h                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_EVAL_H
#define TSP_EVAL_H

#include "define.h"


#ifdef USE_PROTO
extern void alloc_line(void);
extern void free_line(void);
extern void set_line(TOUR, TOUR, LINE);
extern LINE get_line(TOUR, TOUR);
extern LINE calc_line(TOUR, TOUR);
extern void calc_all_lines(void);
extern double eval_lin_2opt(TOUR, TOUR, TOUR, TOUR);
extern double eval_2opt(CHROM *);
extern double eval_or_kopt(CHROM *, BOOLEAN, int);
extern void evaluate(CHROM *);
#else
extern void alloc_line();
extern void free_line();
extern void set_line();
extern LINE get_line();
extern LINE calc_line();
extern void calc_all_lines();
extern double eval_lin_2opt();
extern double eval_2opt();
extern double eval_or_kopt();
extern void evaluate();
#endif


#endif


/*** end of file ***/
