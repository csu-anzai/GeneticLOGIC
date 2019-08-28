/* $Id: optlocal.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : optlocal.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_OPTLOCAL_H
#define TSP_OPTLOCAL_H

#include "define.h"


#ifdef USE_PROTO
extern void op_lin_2opt(CHROM *);
extern void op_2opt(CHROM *);
extern void op_2quick(CHROM *);
extern void op_or_kopt(CHROM *, int);
extern void op_all(CHROM *);
extern void opt_local(CHROM *);
#else
extern void op_lin_2opt();
extern void op_2opt();
extern void op_2quick();
extern void op_or_kopt();
extern void op_all();
extern void opt_local();
#endif


#endif


/*** end of file ***/
