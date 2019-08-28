/* $Id: readscan.l,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : readscan.l                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

%{

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "other.h"
#include "readpars.h"


extern long ParserLine;


char yystr[MAX_ZLEN];

#ifdef USE_PROTO
extern int read_line(char *s);
extern void read_string(void);
#else
extern int read_line();
extern void read_string();
#endif

%}

leer			[ \t]
ziffer			[0-9]
zint			[+-]?{ziffer}+
zfloat			{zint}(\.{ziffer}+)?((E|e){zint})?
buchst			[A-Za-z_]
name			{buchst}({buchst}|{ziffer})+

%%

{leer}+			/* Ueberlesen */
\n			{ ParserLine++;
                          return(yylval.kdo = Y_LF);
                        }
":"			return(yylval.kdo = Y_DP);
"EOF"			return(yylval.kdo = Y_EOF);
{zint}			{ yylval.izahl = (int) atoi(yytext);
			  return(Y_IZAHL);
			}
{zfloat}		{ yylval.fzahl = (float) atof(yytext);
			  return(Y_FZAHL);
			}
"NAME"{leer}*":"	{ read_string();
			  return(Y_NAME_STR);
			}
"COMMENT"{leer}*":"	{ read_string();
			  return(Y_COMMENT_STR);
			}
"TYPE"			return(yylval.kdo = Y_TYPE);
"DIMENSION"		return(yylval.kdo = Y_DIMENSION);
"CAPACITY"		return(yylval.kdo = Y_CAPACITY);
"GRAPH_TYPE"		return(yylval.kdo = Y_GRAPH_TYPE);
"EDGE_TYPE"		return(yylval.kdo = Y_EDGE_TYPE);
"EDGE_WEIGHT_TYPE"	return(yylval.kdo = Y_EDGE_WEIGHT_TYPE);
"EDGE_WEIGHT_FORMAT"	return(yylval.kdo = Y_EDGE_WEIGHT_FORMAT);
"EDGE_DATA_FORMAT"	return(yylval.kdo = Y_EDGE_DATA_FORMAT);
"NODE_TYPE"		return(yylval.kdo = Y_NODE_TYPE);
"NODE_COORD_TYPE"	return(yylval.kdo = Y_NODE_COORD_TYPE);
"COORD1_OFFSET"		return(yylval.kdo = Y_COORD1_OFFSET);
"COORD1_SCALE"		return(yylval.kdo = Y_COORD1_SCALE);
"COORD2_OFFSET"		return(yylval.kdo = Y_COORD2_OFFSET);
"COORD2_SCALE"		return(yylval.kdo = Y_COORD2_SCALE);
"COORD3_OFFSET"		return(yylval.kdo = Y_COORD3_OFFSET);
"COORD3_SCALE"		return(yylval.kdo = Y_COORD3_SCALE);
"DISPLAY_DATA_TYPE"	return(yylval.kdo = Y_DISPLAY_DATA_TYPE);
"TSP"			return(yylval.kdo = Y_TSP);
"ATSP"			return(yylval.kdo = Y_ATSP);
"CVRP"			return(yylval.kdo = Y_CVRP);
"TOUR"			return(yylval.kdo = Y_TOUR);
"GRAPH"			return(yylval.kdo = Y_GRAPH);
"COMPLETE_GRAPH"	return(yylval.kdo = Y_COMPLETE_GRAPH);
"SPARSE_GRAPH"		return(yylval.kdo = Y_SPARSE_GRAPH);
"UNDIRECTED"		return(yylval.kdo = Y_UNDIRECTED);
"DIRECTED"		return(yylval.kdo = Y_DIRECTED);
"EXPLICIT"		return(yylval.kdo = Y_EXPLICIT);
"EUC_2D"		return(yylval.kdo = Y_EUC_2D);
"EUC_3D"		return(yylval.kdo = Y_EUC_3D);
"MAX_2D"		return(yylval.kdo = Y_MAX_2D);
"MAX_3D"		return(yylval.kdo = Y_MAX_3D);
"MAN_2D"		return(yylval.kdo = Y_MAN_2D);
"MAN_3D"		return(yylval.kdo = Y_MAN_3D);
"GEO"			return(yylval.kdo = Y_GEO);
"ATT"			return(yylval.kdo = Y_ATT);
"XRAY1"			return(yylval.kdo = Y_XRAY1);
"XRAY2"			return(yylval.kdo = Y_XRAY2);
"SPECIAL"		return(yylval.kdo = Y_SPECIAL);
"HOTO"			return(yylval.kdo = Y_HOTO);
"FULL_MATRIX"		return(yylval.kdo = Y_FULL_MATRIX);
"UPPER_ROW"		return(yylval.kdo = Y_UPPER_ROW);
"LOWER_ROW"		return(yylval.kdo = Y_LOWER_ROW);
"UPPER_DIAG_ROW"	return(yylval.kdo = Y_UPPER_DIAG_ROW);
"LOWER_DIAG_ROW"	return(yylval.kdo = Y_LOWER_DIAG_ROW);
"UPPER_COL"		return(yylval.kdo = Y_UPPER_COL);
"LOWER_COL"		return(yylval.kdo = Y_LOWER_COL);
"UPPER_DIAG_COL"	return(yylval.kdo = Y_UPPER_DIAG_COL);
"LOWER_DIAG_COL"	return(yylval.kdo = Y_LOWER_DIAG_COL);
"WEIGHT_LIST"		return(yylval.kdo = Y_WEIGHT_LIST);
"FUNCTION"		return(yylval.kdo = Y_FUNCTION);
"ADJ_LIST"		return(yylval.kdo = Y_ADJ_LIST);
"EDGE_LIST"		return(yylval.kdo = Y_EDGE_LIST);
"WEIGHTED_NODES"	return(yylval.kdo = Y_WEIGHTED_NODES);
"UNWEIGHTED_NODES"	return(yylval.kdo = Y_UNWEIGHTED_NODES);
"TWOD_COORDS"		return(yylval.kdo = Y_TWOD_COORDS);
"THREED_COORDS"		return(yylval.kdo = Y_THREED_COORDS);
"NO_COORDS"		return(yylval.kdo = Y_NO_COORDS);
"COORD_DISPLAY"		return(yylval.kdo = Y_COORD_DISPLAY);
"TWOD_DISPLAY"		return(yylval.kdo = Y_TWOD_DISPLAY);
"NO_DISPLAY"		return(yylval.kdo = Y_NO_DISPLAY);
"NODE_COORD_SECTION"	return(yylval.kdo = Y_NODE_COORD_SECTION);
"DEPOT_SECTION"		return(yylval.kdo = Y_DEPOT_SECTION);
"DEMAND_SECTION"	return(yylval.kdo = Y_DEMAND_SECTION);
"FIXED_EDGES_SECTION"	return(yylval.kdo = Y_FIXED_EDGES_SECTION);
"DISPLAY_DATA_SECTION"	return(yylval.kdo = Y_DISPLAY_DATA_SECTION);
"NODE_WEIGHT_SECTION"	return(yylval.kdo = Y_NODE_WEIGHT_SECTION);
"TOUR_SECTION"		return(yylval.kdo = Y_TOUR_SECTION);
"EDGE_DATA_SECTION"	return(yylval.kdo = Y_EDGE_DATA_SECTION);
"EDGE_WEIGHT_SECTION"	return(yylval.kdo = Y_EDGE_WEIGHT_SECTION);
.			return(yylval.kdo = Y_CHAR);

%%

int read_line(s)
  char *s;
{ int pos = 0;
  int ch;

  do
  { ch = input();
    yystr[pos++] = (char) ch;
  }
  while ((ch != '\n') && (ch != EOF));

  yystr[pos] = '\0';
  strcpy(s, yystr);
  ParserLine++;

  return((ch == EOF) ? 1 : 0);
}

void read_string()
{ int pos = 0;
  int ch;

  do
  { ch = input();
    yystr[pos++] = (char) ch;
  }
  while ((ch != '\n') && (ch != EOF));

  if (ch == '\n') unput('\n');
  yystr[--pos] = '\0';

  yylval.str = yystr;
}


/*** end of file ***/
