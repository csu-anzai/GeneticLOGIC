/* $Id: dump.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : dump.h                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_DUMP_H
#define TSP_DUMP_H

#include <stdio.h>
#include "define.h"


#ifdef USE_PROTO
extern void create_pgm(void);
extern void dump_ind(FILE *, int, CHROM *, TOUR);
extern void dump_pop(POPULATION *, int, int, char *);
extern void dump_tour(int);
#else
extern void create_pgm();
extern void dump_ind();
extern void dump_pop();
extern void dump_tour();
#endif


#endif


/*** end of file ***/
