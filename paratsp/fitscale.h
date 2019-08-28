/* $Id: fitscale.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : fitscale.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_FITSCALE_H
#define TSP_FITSCALE_H


#ifdef USE_PROTO
extern double fi_linear(double);
extern double fi_expo(double);
extern double fitness_scale(double);
#else
extern double fi_linear();
extern double fi_expo();
extern double fitness_scale();
#endif


#endif


/*** end of file ***/
