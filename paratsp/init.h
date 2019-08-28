/* $Id: init.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : init.h                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_INIT_H
#define TSP_INIT_H


#ifdef USE_PROTO
extern void init_gen_files(char *);
extern int crt_dir(char *);
extern void init_files(void);
extern void initialize(void);
#else
extern void init_gen_files();
extern int crt_dir();
extern void init_files();
extern void initialize();
#endif


#endif


/*** end of file ***/
