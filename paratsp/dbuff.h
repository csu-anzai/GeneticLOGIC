/* $Id: dbuff.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : dbuff.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_DBUFF_H
#define TSP_DBUFF_H

#include "define.h"


#ifdef USE_PROTO
extern BUFFER *create_buffer(char *, unsigned);
extern void delete_buffer(BUFFER *);
extern void change_fmt(BUFFER *, int, char *);
extern void change_buffer(BUFFER *, char *);
extern void enter_buffer(double *, BUFFER *, int, int);
extern BOOLEAN full_buffer(BUFFER *);
extern void store_buffer(double *, BUFFER *, int);
extern void flush_buffer(BUFFER *, int, int);
extern void write_pfmbuf(int *, int);
#else
extern BUFFER *create_buffer();
extern void delete_buffer();
extern void change_fmt();
extern void change_buffer();
extern void enter_buffer();
extern BOOLEAN full_buffer();
extern void store_buffer();
extern void flush_buffer();
extern void write_pfmbuf();
#endif


#endif


/*** end of file ***/
