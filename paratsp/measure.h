/* $Id: measure.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : measure.h                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_MEASURE_H
#define TSP_MEASURE_H


#ifdef USE_PROTO
extern void measure(void);
extern double avgval(double *, int);
extern double varval(double *, int);
extern double skwval(double *, int, double);
#else
extern void measure();
extern double avgval();
extern double varval();
extern double skwval();
#endif


#endif


/*** end of file ***/
