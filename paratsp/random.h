/* $Id: random.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : random.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_RANDOM_H
#define TSP_RANDOM_H


#ifdef USE_PROTO
extern void init_rnd(void);
extern double equal_random(void);
extern double equal_double_random(double);
extern unsigned equal_unsigned_random(unsigned);
extern unsigned long equal_long_random(unsigned long);
extern void equal_random_int_vec(unsigned *, unsigned, unsigned);
extern double normal_random(void);
extern double normal_double_random(double);
extern unsigned normal_unsigned_random(unsigned);
extern unsigned long normal_long_random(unsigned long);
#else
extern void init_rnd();
extern double equal_random();
extern double equal_double_random();
extern unsigned equal_unsigned_random();
extern unsigned long equal_long_random();
extern void equal_random_int_vec();
extern double normal_random();
extern double normal_double_random();
extern unsigned normal_unsigned_random();
extern unsigned long normal_long_random();
#endif
 

#endif


/*** end of file ***/
