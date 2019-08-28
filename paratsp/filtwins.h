/* $Id: filtwins.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : filtwins.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_FILTWINS_H
#define TSP_FILTWINS_H


#ifdef USE_PROTO
extern void filter_twins(CHROM *, POPULATION *, int);
#else
extern void filter_twins();
#endif


#endif


/*** end of file ***/
