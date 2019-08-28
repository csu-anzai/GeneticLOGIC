/* $Id: schema.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : schema.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_SCHEMA_H
#define TSP_SCHEMA_H


#ifdef USE_PROTO
extern void alloc_schema(void);
extern void free_schema(void);
extern void schema(void);
#else
extern void alloc_schema();
extern void free_schema();
extern void schema();
#endif


#endif


/*** end of file ***/
