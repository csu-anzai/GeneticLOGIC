/* $Id: bit.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : bit.h                                                         */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_BIT_H
#define TSP_BIT_H

#include "define.h"


#ifdef USE_PROTO
extern BIT *alloc_bit(unsigned);
extern void free_bit(BIT *);
extern void clear_bits(BIT *);
extern void invert_bits(BIT *);
extern void set_bit(BIT *, unsigned);
extern BOOLEAN get_bit(BIT *, unsigned);
extern unsigned set_next_bit(BIT *, unsigned);
extern unsigned get_next_0_bit(BIT *, unsigned);
extern unsigned get_next_1_bit(BIT *, unsigned);
#else
extern BIT *alloc_bit();
extern void free_bit();
extern void clear_bits();
extern void invert_bits();
extern void set_bit();
extern BOOLEAN get_bit();
extern unsigned set_next_bit();
extern unsigned get_next_0_bit();
extern unsigned get_next_1_bit();
#endif


#endif


/*** end of file ***/
