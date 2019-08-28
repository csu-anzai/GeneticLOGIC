/* $Id: graphics.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : graphics.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_GRAPHICS_H
#define TSP_GRAPHICS_H

#include "define.h"


#define SEND_EOF	-1	/* end of graph file */


#ifdef USE_PROTO
extern BOOLEAN send_nodes_graph(void);
extern BOOLEAN send_tour_graph(double, MYREP *);
extern void init_X_graph(char *);
extern void close_X_graph(void);
extern TOUR receive_nodes(TOWN **);
extern int receive_tour(TOUR, int *, double *, MYREP *);
#else
extern BOOLEAN send_nodes_graph();
extern BOOLEAN send_tour_graph();
extern void init_X_graph();
extern void close_X_graph();
extern TOUR receive_nodes();
extern int receive_tour();
#endif


#endif


/*** end of file ***/
