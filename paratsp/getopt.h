/* $Id: getopt.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : getopt.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_GETOPT_H
#define TSP_GETOPT_H


extern int my_optind;
extern int my_opterr;
extern char *my_optarg;


#ifdef USE_PROTO
extern int get_options(int, char **, char *);
#else
extern int get_options();
#endif


#endif


/*** end of file ***/
