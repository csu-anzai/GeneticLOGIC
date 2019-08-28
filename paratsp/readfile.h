/* $Id: readfile.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : readfile.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_READFILE_H
#define TSP_READFILE_H

#include <stdio.h>
#include "define.h"


/* types of tsp-file */

#define TYP_UNKNOWN			0
#define TYP_TSP				1
#define TYP_ATSP			2
#define TYP_CVRP			3
#define TYP_TOUR			4
#define TYP_GRAPH			5

#define GRT_UNKNOWN			0
#define GRT_COMPLETE_GRAPH		1
#define GRT_SPARSE_GRAPH		2

#define EDT_UNKNOWN			0
#define EDT_UNDIRECTED			1
#define EDT_DIRECTED			2

#define EWT_UNKNOWN			0
#define EWT_EXPLICIT			1
#define EWT_EUC_2D			2
#define EWT_EUC_3D			3
#define EWT_MAX_2D			4
#define EWT_MAX_3D			5
#define EWT_MAN_2D			6
#define EWT_MAN_3D			7
#define EWT_GEO				8
#define EWT_ATT				9
#define EWT_XRAY1			10
#define EWT_XRAY2			11
#define EWT_SPECIAL			12
#define EWT_HOTO			13

#define EWF_UNKNOWN			0
#define EWF_FULL_MATRIX			1
#define EWF_UPPER_ROW			2
#define EWF_LOWER_ROW			3
#define EWF_UPPER_DIAG_ROW		4
#define EWF_LOWER_DIAG_ROW		5
#define EWF_UPPER_COL			6
#define EWF_LOWER_COL			7
#define EWF_UPPER_DIAG_COL		8
#define EWF_LOWER_DIAG_COL		9
#define EWF_WEIGHT_LIST			10
#define EWF_FUNCTION			11

#define EDF_UNKNOWN			0
#define EDF_ADJ_LIST			1
#define EDF_EDGE_LIST			2

#define NDT_UNKNOWN			0
#define NDT_WEIGHTED_NODES		1
#define NDT_UNWEIGHTED_NODES		2

#define NCT_UNKNOWN			0
#define NCT_TWOD_COORDS			1
#define NCT_THREED_COORDS		2
#define NCT_NO_COORDS			3

#define DDT_UNKNOWN			0
#define DDT_COORD_DISPLAY		1
#define DDT_TWOD_DISPLAY		2
#define DDT_NO_DISPLAY			3


#ifdef USE_PROTO
extern void alloc_town(TOUR);
extern void alloc_tour(TOUR);
extern void free_town(void);
extern void free_tour(void);
extern void read_nodes(void);
extern void read_TSP_file(char *);
extern void read_tour(void);
extern void read_TOUR_file(char *);
extern void print_towns(FILE *);
#else
extern void alloc_town();
extern void alloc_tour();
extern void free_town();
extern void free_tour();
extern void read_nodes();
extern void read_TSP_file();
extern void read_tour();
extern void read_TOUR_file();
extern void print_towns();
#endif


#endif


/*** end of file ***/
