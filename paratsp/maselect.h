/* $Id: maselect.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : maselect.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_MASELECT_H
#define TSP_MASELECT_H


#ifdef USE_PROTO
extern int ms_random(void);
extern int ms_position(void);
extern int mate_select(void);
#else
extern int ms_random();
extern int ms_position();
extern int mate_select();
#endif


#endif


/*** end of file ***/
