
/*  A Bison parser, made from readpars.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	Y_IZAHL	258
#define	Y_FZAHL	259
#define	Y_NAME_STR	260
#define	Y_COMMENT_STR	261
#define	Y_EOF	262
#define	Y_LF	263
#define	Y_DP	264
#define	Y_CHAR	265
#define	Y_TYPE	266
#define	Y_DIMENSION	267
#define	Y_CAPACITY	268
#define	Y_GRAPH_TYPE	269
#define	Y_EDGE_TYPE	270
#define	Y_EDGE_WEIGHT_TYPE	271
#define	Y_EDGE_WEIGHT_FORMAT	272
#define	Y_EDGE_DATA_FORMAT	273
#define	Y_NODE_TYPE	274
#define	Y_NODE_COORD_TYPE	275
#define	Y_COORD1_OFFSET	276
#define	Y_COORD1_SCALE	277
#define	Y_COORD2_OFFSET	278
#define	Y_COORD2_SCALE	279
#define	Y_COORD3_OFFSET	280
#define	Y_COORD3_SCALE	281
#define	Y_DISPLAY_DATA_TYPE	282
#define	Y_TSP	283
#define	Y_ATSP	284
#define	Y_CVRP	285
#define	Y_TOUR	286
#define	Y_GRAPH	287
#define	Y_COMPLETE_GRAPH	288
#define	Y_SPARSE_GRAPH	289
#define	Y_UNDIRECTED	290
#define	Y_DIRECTED	291
#define	Y_EXPLICIT	292
#define	Y_EUC_2D	293
#define	Y_EUC_3D	294
#define	Y_MAX_2D	295
#define	Y_MAX_3D	296
#define	Y_MAN_2D	297
#define	Y_MAN_3D	298
#define	Y_GEO	299
#define	Y_ATT	300
#define	Y_XRAY1	301
#define	Y_XRAY2	302
#define	Y_SPECIAL	303
#define	Y_HOTO	304
#define	Y_FULL_MATRIX	305
#define	Y_UPPER_ROW	306
#define	Y_LOWER_ROW	307
#define	Y_UPPER_DIAG_ROW	308
#define	Y_LOWER_DIAG_ROW	309
#define	Y_UPPER_COL	310
#define	Y_LOWER_COL	311
#define	Y_UPPER_DIAG_COL	312
#define	Y_LOWER_DIAG_COL	313
#define	Y_WEIGHT_LIST	314
#define	Y_FUNCTION	315
#define	Y_ADJ_LIST	316
#define	Y_EDGE_LIST	317
#define	Y_WEIGHTED_NODES	318
#define	Y_UNWEIGHTED_NODES	319
#define	Y_TWOD_COORDS	320
#define	Y_THREED_COORDS	321
#define	Y_NO_COORDS	322
#define	Y_COORD_DISPLAY	323
#define	Y_TWOD_DISPLAY	324
#define	Y_NO_DISPLAY	325
#define	Y_NODE_COORD_SECTION	326
#define	Y_DEPOT_SECTION	327
#define	Y_DEMAND_SECTION	328
#define	Y_FIXED_EDGES_SECTION	329
#define	Y_DISPLAY_DATA_SECTION	330
#define	Y_NODE_WEIGHT_SECTION	331
#define	Y_TOUR_SECTION	332
#define	Y_EDGE_DATA_SECTION	333
#define	Y_EDGE_WEIGHT_SECTION	334



#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "readfile.h"


typedef union { int izahl;
         float fzahl;
         char *str;
         int kdo;
       } YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		124
#define	YYFLAG		-32768
#define	YYNTBASE	80

#define YYTRANSLATE(x) ((unsigned)(x) <= 334 ? yytranslate[x] : 94)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
    76,    77,    78,    79
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     4,     6,    10,    12,    16,    18,    22,    26,    30,
    34,    38,    42,    46,    50,    54,    58,    62,    66,    70,
    74,    78,    82,    84,    86,    88,    90,    92,    94,    96,
    98,   100,   102,   104,   106,   108,   110,   112,   114,   116,
   118,   120,   122,   124,   126,   128,   130,   132,   134,   136,
   138,   140,   142,   144,   146,   148,   150,   152,   154,   156,
   158,   160,   162,   164,   166,   168,   170,   173,   176,   179,
   182,   185,   188,   191,   194,   197
};

static const short yyrhs[] = {    81,
    92,     7,     0,    93,     0,    81,    82,     8,     0,     5,
     0,    11,     9,    83,     0,     6,     0,    12,     9,     3,
     0,    13,     9,     3,     0,    14,     9,    84,     0,    15,
     9,    85,     0,    16,     9,    86,     0,    17,     9,    87,
     0,    18,     9,    88,     0,    19,     9,    89,     0,    20,
     9,    90,     0,    21,     9,     4,     0,    22,     9,     4,
     0,    23,     9,     4,     0,    24,     9,     4,     0,    25,
     9,     4,     0,    26,     9,     4,     0,    27,     9,    91,
     0,    93,     0,    28,     0,    29,     0,    30,     0,    31,
     0,    32,     0,    33,     0,    34,     0,    35,     0,    36,
     0,    37,     0,    38,     0,    39,     0,    40,     0,    41,
     0,    42,     0,    43,     0,    44,     0,    45,     0,    46,
     0,    47,     0,    48,     0,    49,     0,    50,     0,    51,
     0,    52,     0,    53,     0,    54,     0,    55,     0,    56,
     0,    57,     0,    58,     0,    59,     0,    60,     0,    61,
     0,    62,     0,    63,     0,    64,     0,    65,     0,    66,
     0,    67,     0,    68,     0,    69,     0,    70,     0,    71,
     8,     0,    72,     8,     0,    73,     8,     0,    74,     8,
     0,    75,     8,     0,    76,     8,     0,    77,     8,     0,
    78,     8,     0,    79,     8,     0,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    61,    63,    64,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
    83,    84,    85,    87,    88,    89,    90,    91,    93,    94,
    96,    97,    99,   100,   101,   102,   103,   104,   105,   106,
   107,   108,   109,   110,   111,   113,   114,   115,   116,   117,
   118,   119,   120,   121,   122,   123,   125,   126,   128,   129,
   131,   132,   133,   135,   136,   137,   139,   140,   141,   142,
   143,   144,   145,   146,   147,   149
};

static const char * const yytname[] = {   "$","error","$illegal.","Y_IZAHL",
"Y_FZAHL","Y_NAME_STR","Y_COMMENT_STR","Y_EOF","Y_LF","Y_DP","Y_CHAR","Y_TYPE",
"Y_DIMENSION","Y_CAPACITY","Y_GRAPH_TYPE","Y_EDGE_TYPE","Y_EDGE_WEIGHT_TYPE",
"Y_EDGE_WEIGHT_FORMAT","Y_EDGE_DATA_FORMAT","Y_NODE_TYPE","Y_NODE_COORD_TYPE",
"Y_COORD1_OFFSET","Y_COORD1_SCALE","Y_COORD2_OFFSET","Y_COORD2_SCALE","Y_COORD3_OFFSET",
"Y_COORD3_SCALE","Y_DISPLAY_DATA_TYPE","Y_TSP","Y_ATSP","Y_CVRP","Y_TOUR","Y_GRAPH",
"Y_COMPLETE_GRAPH","Y_SPARSE_GRAPH","Y_UNDIRECTED","Y_DIRECTED","Y_EXPLICIT",
"Y_EUC_2D","Y_EUC_3D","Y_MAX_2D","Y_MAX_3D","Y_MAN_2D","Y_MAN_3D","Y_GEO","Y_ATT",
"Y_XRAY1","Y_XRAY2","Y_SPECIAL","Y_HOTO","Y_FULL_MATRIX","Y_UPPER_ROW","Y_LOWER_ROW",
"Y_UPPER_DIAG_ROW","Y_LOWER_DIAG_ROW","Y_UPPER_COL","Y_LOWER_COL","Y_UPPER_DIAG_COL",
"Y_LOWER_DIAG_COL","Y_WEIGHT_LIST","Y_FUNCTION","Y_ADJ_LIST","Y_EDGE_LIST","Y_WEIGHTED_NODES",
"Y_UNWEIGHTED_NODES","Y_TWOD_COORDS","Y_THREED_COORDS","Y_NO_COORDS","Y_COORD_DISPLAY",
"Y_TWOD_DISPLAY","Y_NO_DISPLAY","Y_NODE_COORD_SECTION","Y_DEPOT_SECTION","Y_DEMAND_SECTION",
"Y_FIXED_EDGES_SECTION","Y_DISPLAY_DATA_SECTION","Y_NODE_WEIGHT_SECTION","Y_TOUR_SECTION",
"Y_EDGE_DATA_SECTION","Y_EDGE_WEIGHT_SECTION","tsplib","specification","spec_line",
"type","graph_type","edge_type","edge_weight_typ","edge_weight_for","edge_data_forma",
"node_type","node_coord_type","display_data_ty","data","epsilon",""
};
#endif

static const short yyr1[] = {     0,
    80,    81,    81,    82,    82,    82,    82,    82,    82,    82,
    82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
    82,    82,    82,    83,    83,    83,    83,    83,    84,    84,
    85,    85,    86,    86,    86,    86,    86,    86,    86,    86,
    86,    86,    86,    86,    86,    87,    87,    87,    87,    87,
    87,    87,    87,    87,    87,    87,    88,    88,    89,    89,
    90,    90,    90,    91,    91,    91,    92,    92,    92,    92,
    92,    92,    92,    92,    92,    93
};

static const short yyr2[] = {     0,
     3,     1,     3,     1,     3,     1,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     0
};

static const short yydefact[] = {    76,
    76,     2,     4,     6,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    23,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    67,    68,    69,    70,    71,    72,    73,    74,    75,     3,
     1,    24,    25,    26,    27,    28,     5,     7,     8,    29,
    30,     9,    31,    32,    10,    33,    34,    35,    36,    37,
    38,    39,    40,    41,    42,    43,    44,    45,    11,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    12,    57,    58,    13,    59,    60,    14,    61,    62,    63,
    15,    16,    17,    18,    19,    20,    21,    64,    65,    66,
    22,     0,     0,     0
};

static const short yydefgoto[] = {   122,
     1,    31,    67,    72,    75,    89,   101,   104,   107,   111,
   121,    32,     2
};

static const short yypact[] = {-32768,
    -5,-32768,-32768,-32768,    -4,    27,    49,    54,    66,    67,
    68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
    78,    80,    81,    82,    83,    84,    85,    86,    87,    88,
    89,    91,-32768,    20,    96,    97,    23,    24,   -14,   -13,
     0,     1,   -63,    98,    99,   100,   101,   102,   103,   -15,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   108,   109,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   110
};


#define	YYLAST		111


static const short yytable[] = {     3,
     4,   108,   109,   110,    34,     5,     6,     7,     8,     9,
    10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
    20,    21,    76,    77,    78,    79,    80,    81,    82,    83,
    84,    85,    86,    87,    88,    35,    90,    91,    92,    93,
    94,    95,    96,    97,    98,    99,   100,    62,    63,    64,
    65,    66,   118,   119,   120,    70,    71,    36,    73,    74,
   102,   103,    37,   105,   106,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    38,    39,    40,    41,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    61,    68,    69,
     0,   112,   113,   114,   115,   116,   117,   123,   124,     0,
    33
};

static const short yycheck[] = {     5,
     6,    65,    66,    67,     9,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    37,    38,    39,    40,    41,    42,    43,    44,
    45,    46,    47,    48,    49,     9,    50,    51,    52,    53,
    54,    55,    56,    57,    58,    59,    60,    28,    29,    30,
    31,    32,    68,    69,    70,    33,    34,     9,    35,    36,
    61,    62,     9,    63,    64,    71,    72,    73,    74,    75,
    76,    77,    78,    79,     9,     9,     9,     9,     9,     9,
     9,     9,     9,     9,     9,     9,     9,     8,     8,     8,
     8,     8,     8,     8,     8,     8,     8,     7,     3,     3,
    -1,     4,     4,     4,     4,     4,     4,     0,     0,    -1,
     1
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */


/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif


int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
{ YYACCEPT; ;
    break;}
case 2:
{;
    break;}
case 3:
{;
    break;}
case 4:
{ strcpy(FDat.nam, yyvsp[0].str); ;
    break;}
case 5:
{;
    break;}
case 6:
{ strcpy(FDat.com, yyvsp[0].str); ;
    break;}
case 7:
{ FDat.dim = yyvsp[0].izahl; ;
    break;}
case 8:
{ FDat.cap = yyvsp[0].izahl; ;
    break;}
case 9:
{;
    break;}
case 10:
{;
    break;}
case 11:
{;
    break;}
case 12:
{;
    break;}
case 13:
{;
    break;}
case 14:
{;
    break;}
case 15:
{;
    break;}
case 16:
{ FDat.c1o = yyvsp[0].fzahl; ;
    break;}
case 17:
{ FDat.c1s = yyvsp[0].fzahl; ;
    break;}
case 18:
{ FDat.c2o = yyvsp[0].fzahl; ;
    break;}
case 19:
{ FDat.c2s = yyvsp[0].fzahl; ;
    break;}
case 20:
{ FDat.c3o = yyvsp[0].fzahl; ;
    break;}
case 21:
{ FDat.c3s = yyvsp[0].fzahl; ;
    break;}
case 22:
{;
    break;}
case 23:
{;
    break;}
case 24:
{ FDat.typ = TYP_TSP; ;
    break;}
case 25:
{ FDat.typ = TYP_ATSP; ;
    break;}
case 26:
{ FDat.typ = TYP_CVRP; ;
    break;}
case 27:
{ FDat.typ = TYP_TOUR; ;
    break;}
case 28:
{ FDat.typ = TYP_GRAPH; ;
    break;}
case 29:
{ FDat.grt = GRT_COMPLETE_GRAPH; ;
    break;}
case 30:
{ FDat.grt = GRT_SPARSE_GRAPH; ;
    break;}
case 31:
{ FDat.edt = EDT_UNDIRECTED; ;
    break;}
case 32:
{ FDat.edt = EDT_DIRECTED; ;
    break;}
case 33:
{ FDat.ewt = EWT_EXPLICIT; ;
    break;}
case 34:
{ FDat.ewt = EWT_EUC_2D; ;
    break;}
case 35:
{ FDat.ewt = EWT_EUC_3D; ;
    break;}
case 36:
{ FDat.ewt = EWT_MAX_2D; ;
    break;}
case 37:
{ FDat.ewt = EWT_MAX_3D; ;
    break;}
case 38:
{ FDat.ewt = EWT_MAN_2D; ;
    break;}
case 39:
{ FDat.ewt = EWT_MAN_3D; ;
    break;}
case 40:
{ FDat.ewt = EWT_GEO; ;
    break;}
case 41:
{ FDat.ewt = EWT_ATT; ;
    break;}
case 42:
{ FDat.ewt = EWT_XRAY1; ;
    break;}
case 43:
{ FDat.ewt = EWT_XRAY2; ;
    break;}
case 44:
{ FDat.ewt = EWT_SPECIAL; ;
    break;}
case 45:
{ FDat.ewt = EWT_HOTO; ;
    break;}
case 46:
{ FDat.ewf = EWF_FULL_MATRIX; ;
    break;}
case 47:
{ FDat.ewf = EWF_UPPER_ROW; ;
    break;}
case 48:
{ FDat.ewf = EWF_LOWER_ROW; ;
    break;}
case 49:
{ FDat.ewf = EWF_UPPER_DIAG_ROW; ;
    break;}
case 50:
{ FDat.ewf = EWF_LOWER_DIAG_ROW; ;
    break;}
case 51:
{ FDat.ewf = EWF_UPPER_COL; ;
    break;}
case 52:
{ FDat.ewf = EWF_LOWER_COL; ;
    break;}
case 53:
{ FDat.ewf = EWF_UPPER_DIAG_COL; ;
    break;}
case 54:
{ FDat.ewf = EWF_LOWER_DIAG_COL; ;
    break;}
case 55:
{ FDat.ewf = EWF_WEIGHT_LIST; ;
    break;}
case 56:
{ FDat.ewf = EWF_FUNCTION; ;
    break;}
case 57:
{ FDat.edf = EDF_ADJ_LIST; ;
    break;}
case 58:
{ FDat.edf = EDF_EDGE_LIST; ;
    break;}
case 59:
{ FDat.ndt = NDT_WEIGHTED_NODES; ;
    break;}
case 60:
{ FDat.ndt = NDT_UNWEIGHTED_NODES; ;
    break;}
case 61:
{ FDat.nct = NCT_TWOD_COORDS; ;
    break;}
case 62:
{ FDat.nct = NCT_THREED_COORDS; ;
    break;}
case 63:
{ FDat.nct = NCT_NO_COORDS; ;
    break;}
case 64:
{ FDat.ddt = DDT_COORD_DISPLAY; ;
    break;}
case 65:
{ FDat.ddt = DDT_TWOD_DISPLAY; ;
    break;}
case 66:
{ FDat.ddt = DDT_NO_DISPLAY; ;
    break;}
case 67:
{ read_nodes(); ;
    break;}
case 68:
{;
    break;}
case 69:
{;
    break;}
case 70:
{;
    break;}
case 71:
{;
    break;}
case 72:
{;
    break;}
case 73:
{ read_tour(); ;
    break;}
case 74:
{;
    break;}
case 75:
{;
    break;}
case 76:
{;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}


int yyerror(s)
  char *s;
{
  sprintf(Msg, "Parser: Syntax error in file '%s', line %ld", ParserFile,
    ParserLine);
  critical_error(ERR_FILE_SYNTAX, Msg);

  return(0);
}


/*** end of file ***/
