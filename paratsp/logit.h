/* $Id: logit.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : logit.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_LOGIT_H
#define TSP_LOGIT_H

#include <stdio.h>
#include "define.h"


#ifdef USE_PROTO
extern void log_string(FILE *, char *, char *);
extern void log_int(FILE *, char *, int);
extern void log_double(FILE *, char *, double);
extern void log_num_string(FILE *, char *, char *, BOOLEAN);
extern void log_num_int(FILE *, char *, int, BOOLEAN);
extern void log_num_double(FILE *, char *, double, BOOLEAN);
#else
extern void log_string();
extern void log_int();
extern void log_double();
extern void log_num_string();
extern void log_num_int();
extern void log_num_double();
#endif


#endif


/*** end of file ***/
