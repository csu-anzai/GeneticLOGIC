/* $Id: generate.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : generate.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_GENERATE_H
#define TSP_GENERATE_H

#include "define.h"


#ifdef USE_PROTO
extern void sort_pop(POPULATION *);
extern void dump_infos(void);
extern void generate(void);
#else
extern void sort_pop();
extern void dump_infos();
extern void generate();
#endif


#endif


/*** end of file ***/
