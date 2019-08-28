/* $Id: commpop.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : commpop.h                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_COMMPOP_H
#define TSP_COMMPOP_H

#include "define.h"


#ifdef USE_PROTO
extern void pm_token(void);
extern void pm_pollen(void);
extern void pm_neighbour(void);
extern void comm_pop(void);
#else
extern void pm_token();
extern void pm_pollen();
extern void pm_neighbour();
extern void comm_pop();
#endif


#endif


/*** end of file ***/
