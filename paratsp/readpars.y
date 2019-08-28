/* $Id: readpars.y,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : readpars.y                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

%{

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "readfile.h"

%}

%union { int izahl;
         float fzahl;
         char *str;
         int kdo;
       }

%token <izahl> Y_IZAHL
%token <fzahl> Y_FZAHL
%token <str> Y_NAME_STR, Y_COMMENT_STR
%token <kdo> Y_EOF, Y_LF, Y_DP, Y_CHAR
%token <kdo> Y_TYPE, Y_DIMENSION, Y_CAPACITY, Y_GRAPH_TYPE 
%token <kdo> Y_EDGE_TYPE, Y_EDGE_WEIGHT_TYPE, Y_EDGE_WEIGHT_FORMAT
%token <kdo> Y_EDGE_DATA_FORMAT, Y_NODE_TYPE, Y_NODE_COORD_TYPE
%token <kdo> Y_COORD1_OFFSET, Y_COORD1_SCALE, Y_COORD2_OFFSET, Y_COORD2_SCALE
%token <kdo> Y_COORD3_OFFSET, Y_COORD3_SCALE, Y_DISPLAY_DATA_TYPE
%token <kdo> Y_TSP, Y_ATSP, Y_CVRP, Y_TOUR, Y_GRAPH
%token <kdo> Y_COMPLETE_GRAPH, Y_SPARSE_GRAPH
%token <kdo> Y_UNDIRECTED, Y_DIRECTED
%token <kdo> Y_EXPLICIT, Y_EUC_2D, Y_EUC_3D, Y_MAX_2D, Y_MAX_3D, Y_MAN_2D
%token <kdo> Y_MAN_3D, Y_GEO, Y_ATT, Y_XRAY1, Y_XRAY2, Y_SPECIAL, Y_HOTO
%token <kdo> Y_FULL_MATRIX, Y_UPPER_ROW, Y_LOWER_ROW, Y_UPPER_DIAG_ROW
%token <kdo> Y_LOWER_DIAG_ROW, Y_UPPER_COL, Y_LOWER_COL, Y_UPPER_DIAG_COL
%token <kdo> Y_LOWER_DIAG_COL, Y_WEIGHT_LIST, Y_FUNCTION
%token <kdo> Y_ADJ_LIST, Y_EDGE_LIST
%token <kdo> Y_WEIGHTED_NODES, Y_UNWEIGHTED_NODES
%token <kdo> Y_TWOD_COORDS, Y_THREED_COORDS, Y_NO_COORDS
%token <kdo> Y_COORD_DISPLAY, Y_TWOD_DISPLAY, Y_NO_DISPLAY
%token <kdo> Y_NODE_COORD_SECTION, Y_DEPOT_SECTION, Y_DEMAND_SECTION
%token <kdo> Y_FIXED_EDGES_SECTION, Y_DISPLAY_DATA_SECTION
%token <kdo> Y_NODE_WEIGHT_SECTION, Y_TOUR_SECTION, Y_EDGE_DATA_SECTION
%token <kdo> Y_EDGE_WEIGHT_SECTION

%start tsplib

%%

/* Aufbau einer TSPLIB-Datei */
tsplib		: specification	data Y_EOF	{ YYACCEPT; }
		;
specification	: epsilon			{}
		| specification spec_line Y_LF	{}
		;
spec_line	: Y_NAME_STR			{ strcpy(FDat.nam, $1); }
		| Y_TYPE Y_DP type		{}
		| Y_COMMENT_STR			{ strcpy(FDat.com, $1); }
		| Y_DIMENSION Y_DP Y_IZAHL	{ FDat.dim = $3; }
		| Y_CAPACITY Y_DP Y_IZAHL	{ FDat.cap = $3; }
		| Y_GRAPH_TYPE Y_DP graph_type	{}
		| Y_EDGE_TYPE Y_DP edge_type	{}
		| Y_EDGE_WEIGHT_TYPE Y_DP edge_weight_typ	{}
		| Y_EDGE_WEIGHT_FORMAT Y_DP edge_weight_for	{}
		| Y_EDGE_DATA_FORMAT Y_DP edge_data_forma	{}
		| Y_NODE_TYPE Y_DP node_type	{}
		| Y_NODE_COORD_TYPE Y_DP node_coord_type	{}
		| Y_COORD1_OFFSET Y_DP Y_FZAHL	{ FDat.c1o = $3; }
		| Y_COORD1_SCALE Y_DP Y_FZAHL	{ FDat.c1s = $3; }
		| Y_COORD2_OFFSET Y_DP Y_FZAHL	{ FDat.c2o = $3; }
		| Y_COORD2_SCALE Y_DP Y_FZAHL	{ FDat.c2s = $3; }
		| Y_COORD3_OFFSET Y_DP Y_FZAHL	{ FDat.c3o = $3; }
		| Y_COORD3_SCALE Y_DP Y_FZAHL	{ FDat.c3s = $3; }
		| Y_DISPLAY_DATA_TYPE Y_DP display_data_ty	{}
		| epsilon			{}
		;
type		: Y_TSP			{ FDat.typ = TYP_TSP; }
		| Y_ATSP		{ FDat.typ = TYP_ATSP; }
		| Y_CVRP		{ FDat.typ = TYP_CVRP; }
		| Y_TOUR		{ FDat.typ = TYP_TOUR; }
		| Y_GRAPH		{ FDat.typ = TYP_GRAPH; }
		;
graph_type	: Y_COMPLETE_GRAPH	{ FDat.grt = GRT_COMPLETE_GRAPH; }
		| Y_SPARSE_GRAPH	{ FDat.grt = GRT_SPARSE_GRAPH; }
		;
edge_type	: Y_UNDIRECTED		{ FDat.edt = EDT_UNDIRECTED; }
		| Y_DIRECTED		{ FDat.edt = EDT_DIRECTED; }
		;
edge_weight_typ	: Y_EXPLICIT		{ FDat.ewt = EWT_EXPLICIT; }
		| Y_EUC_2D		{ FDat.ewt = EWT_EUC_2D; }
		| Y_EUC_3D		{ FDat.ewt = EWT_EUC_3D; }
		| Y_MAX_2D		{ FDat.ewt = EWT_MAX_2D; }
		| Y_MAX_3D		{ FDat.ewt = EWT_MAX_3D; }
		| Y_MAN_2D		{ FDat.ewt = EWT_MAN_2D; }
		| Y_MAN_3D		{ FDat.ewt = EWT_MAN_3D; }
		| Y_GEO			{ FDat.ewt = EWT_GEO; }
		| Y_ATT			{ FDat.ewt = EWT_ATT; }
		| Y_XRAY1		{ FDat.ewt = EWT_XRAY1; }
		| Y_XRAY2		{ FDat.ewt = EWT_XRAY2; }
		| Y_SPECIAL		{ FDat.ewt = EWT_SPECIAL; }
		| Y_HOTO		{ FDat.ewt = EWT_HOTO; }
		;
edge_weight_for	: Y_FULL_MATRIX		{ FDat.ewf = EWF_FULL_MATRIX; }
		| Y_UPPER_ROW		{ FDat.ewf = EWF_UPPER_ROW; }
		| Y_LOWER_ROW		{ FDat.ewf = EWF_LOWER_ROW; }
		| Y_UPPER_DIAG_ROW	{ FDat.ewf = EWF_UPPER_DIAG_ROW; }
		| Y_LOWER_DIAG_ROW	{ FDat.ewf = EWF_LOWER_DIAG_ROW; }
		| Y_UPPER_COL		{ FDat.ewf = EWF_UPPER_COL; }
		| Y_LOWER_COL		{ FDat.ewf = EWF_LOWER_COL; }
		| Y_UPPER_DIAG_COL	{ FDat.ewf = EWF_UPPER_DIAG_COL; }
		| Y_LOWER_DIAG_COL	{ FDat.ewf = EWF_LOWER_DIAG_COL; }
		| Y_WEIGHT_LIST		{ FDat.ewf = EWF_WEIGHT_LIST; }
		| Y_FUNCTION		{ FDat.ewf = EWF_FUNCTION; }
		;
edge_data_forma	: Y_ADJ_LIST		{ FDat.edf = EDF_ADJ_LIST; }
		| Y_EDGE_LIST		{ FDat.edf = EDF_EDGE_LIST; }
		;
node_type	: Y_WEIGHTED_NODES	{ FDat.ndt = NDT_WEIGHTED_NODES; }
		| Y_UNWEIGHTED_NODES	{ FDat.ndt = NDT_UNWEIGHTED_NODES; }
		;
node_coord_type	: Y_TWOD_COORDS		{ FDat.nct = NCT_TWOD_COORDS; }
		| Y_THREED_COORDS	{ FDat.nct = NCT_THREED_COORDS; }
		| Y_NO_COORDS		{ FDat.nct = NCT_NO_COORDS; }
		;
display_data_ty	: Y_COORD_DISPLAY	{ FDat.ddt = DDT_COORD_DISPLAY; }
		| Y_TWOD_DISPLAY	{ FDat.ddt = DDT_TWOD_DISPLAY; }
		| Y_NO_DISPLAY		{ FDat.ddt = DDT_NO_DISPLAY; }
		;
data		: Y_NODE_COORD_SECTION Y_LF	{ read_nodes(); }
		| Y_DEPOT_SECTION Y_LF		{}
		| Y_DEMAND_SECTION Y_LF		{}
		| Y_FIXED_EDGES_SECTION Y_LF	{}
		| Y_DISPLAY_DATA_SECTION Y_LF	{}
		| Y_NODE_WEIGHT_SECTION Y_LF	{}
		| Y_TOUR_SECTION Y_LF		{ read_tour(); }
		| Y_EDGE_DATA_SECTION Y_LF	{}
		| Y_EDGE_WEIGHT_SECTION Y_LF	{}
		;
epsilon		: /* leer */			{}
		;

%%

int yyerror(s)
  char *s;
{
  sprintf(Msg, "Parser: Syntax error in file '%s', line %ld", ParserFile,
    ParserLine);
  critical_error(ERR_FILE_SYNTAX, Msg);

  return(0);
}


/*** end of file ***/
