/* $Id: define.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : define.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_DEFINE_H
#define TSP_DEFINE_H

#include <values.h>


#define SWAP(x1, x2) { x1 ^= x2; x2 ^= x1; x1 ^= x2; }
#define MAX(x1, x2) (x1 >= x2) ? x1 : x2


#ifndef NULL
#define NULL	((void *) 0)
#endif

#ifndef TRUE
#define TRUE	(1)
#endif

#ifndef FALSE
#define FALSE	(0)
#endif

#ifndef BOOLEAN
#define BOOLEAN unsigned int
#endif

/* consts */

#define BUFCNT			3
#define CONVDIV			20
#define DATACOL			12
#define DIRSEP			"/"
#define DISP_PROC		0
#define EPS			1.0e-9
#define FILTER_MUTPROB		2.0			/* Wert > 1.0 */
#define LOGLEN			39		
#define LOGO			"ParaTSP 1.0"
#define MAX_ANGLE		40
#define MAX_FILTER		20
#define MAX_NEIGHBOUR		12
#define MAX_SEED		MAXINT
#define MAX_STR			128
#define MAX_TOWN		MAXINT
#define MAX_WINDFORCE		12
#define MAX_ZLEN		80
#define MULLINE			1
#define NO_LOGFILE		-1
#define NO_NEIGHBOUR		-1
#define PARATSP			"ParaTSP 1.0\t(c) 1994  Holger Totzke"
#define ROWS			100
#define SGLLINE			0

/* limits for parameter */

#define MIN_CtrlParam		0.8
#define MAX_CtrlParam		1.0
#define MIN_EtaMax		1.0
#define MAX_EtaMax		2.0
#define MIN_MinQuality		0.0
#define MAX_MinQuality		MAXLONG
#define MAX_PopNum		((1 << MAX_NEIGHBOUR) - 1)
#define MIN_PopSize		2
#define MAX_PopSize		MAXINT
#define MAX_TotalExperiments	1000
#define MAX_TotalGenerations	MAXINT
#define MAX_TotalTrials		MAXINT

#define MAX_MATES		MAX_PopSize

/* types of parameters */

#define BRS_INV		'I'
#define BRS_GAU		'G'
#define BRS_RND		'R'
#define BRS_PRO		'P'
#define BRS_LIR		'L'
#define BRS_ILR		'N'
#define BRS_WHI		'W'
#define BRS_BOL		'B'

#define ELI_NOP		'_'
#define ELI_NOR		'N'
#define ELI_MUT		'M'

#define FIT_NOP		'_'
#define FIT_LIN		'L'
#define FIT_EXP		'E'

#define POP_RND		'R'
#define POP_TOU		'T'
#define POP_NEA		'N'
#define POP_CHE		'C'
#define POP_FAR		'F'
#define POP_ALT		'W'
#define POP_ALL		'A'

#define ELS_RND		'R'
#define ELS_WEA		'W'
#define ELS_FIW		'F'

#define MUT_NOP		'_'
#define MUT_CON		'C'
#define MUT_ADA		'A'
#define MUT_SWP		'S'
#define MUT_MOV		'M'
#define MUT_INV		'I'
#define MUT_RND		'R'
#define MUT_OPL		'T'
#define MUT_ALL		'L'

#define REP_RND		'R'
#define REP_CRW		'C'

#define MAS_RND		'R'
#define MAS_POS		'P'

#define OPL_NOP		'_'
#define OPL_LO2		'L'
#define OPL_OP2		'T'
#define OPL_2QU		'Q'
#define OPL_ORA		'A'
#define OPL_1OR		'1'
#define OPL_2OR		'2'
#define OPL_3OR		'3'

#define CRO_NOP		'_'
#define CRO_EDG		'E'
#define CRO_INT		'I'
#define CRO_PMX		'P'
#define CRO_OX		'O'
#define CRO_CX		'C'
#define CRO_UNI		'U'
#define CRO_NEA		'N'
#define CRO_CHE		'H'
#define CRO_FAR		'F'
#define CRO_RND		'R'

#define PAR_ISL		'i'
#define PAR_TOK		't'
#define PAR_POL		'p'
#define PAR_NEI		'n'

#define TOP_RIN		'r'
#define TOP_PIP		'p'
#define TOP_GRI		'g'
#define TOP_TOR		'o'
#define TOP_HYP		'h'
#define TOP_TRE		't'

#define RND_MAR		'm'
#define RND_PRG		'p'

/* predefined values for parameters */

#define DEF_TspFileName		"tsp"
#define DEF_BreederSelect	BRS_INV
#define DEF_CrossRate		0.6
#define DEF_MutDim		5.0
#define DEF_Elite		ELI_NOR
#define DEF_FitnessScale	FIT_NOP
#define DEF_GapSize		1.0
#define DEF_EliteHold		1
#define DEF_PopInit		POP_RND
#define DEF_EliteSelect		ELS_WEA
#define DEF_Mutation		"AM"
#define DEF_NormNum		1
#define DEF_GA_Options		""
#define DEF_PopSize		50
#define DEF_CrowFactor		2
#define DEF_Replace		REP_RND
#define DEF_MateSelect		MAS_RND
#define DEF_OptLocal		OPL_NOP
#define DEF_WindowSize		5
#define DEF_Crossover		CRO_EDG
#define DEF_MutRate		0.005
#define DEF_FitParA		1
#define DEF_FitParB		1
#define DEF_FitParC		1
#define DEF_FitParD		1
#define DEF_ChnLen		5
#define DEF_LowerIndex		1
#define DEF_EtaMax		1.1
#define DEF_CtrlParam		0.99
#define DEF_UpperIndex		50
#define DEF_CrossInt		1
#define DEF_TourFileName	"tour"
#define DEF_PgmFreq		0
#define DEF_GraphicsFlag	FALSE
#define DEF_DisplayFlag		FALSE
#define DEF_TotalExperiments	1
#define DEF_DumpFreq		0
#define DEF_TotalGenerations	100
#define DEF_InFile		""
#define DEF_OutFlag		TRUE
#define DEF_MinQuality		0.0
#define DEF_SaveSize		1
#define DEF_Options		"an"
#define	DEF_CommInt		10
#define	DEF_ParModel		PAR_ISL
#define	DEF_IndNum		1
#define	DEF_PopNum		1
#define DEF_ProcNum		1
#define DEF_PollenDirection	0
#define	DEF_Topology		TOP_RIN
#define DEF_LowerWindForce	0
#define DEF_UpperWindForce	MAX_WINDFORCE
#define DEF_QueryFlag		FALSE
#define DEF_RandomType		RND_MAR
#define DEF_OrigSeed		0
#define DEF_TotalTrials		1000
#define DEF_PopDisplay		1
#define DEF_Interval		100
#define DEF_ReportFlag		TRUE
#define DEF_Suffix		""
#define DEF_MaxSpin		2


/* global types */

typedef float TOWN_COORD;
typedef struct {
  TOWN_COORD x, y, z;
  char *name;
} TOWN;

typedef unsigned TOUR;

typedef long LINE;

typedef struct {
  char nam[MAX_ZLEN];
  int typ;
  char com[MAX_ZLEN];
  TOUR dim;
  int cap;
  int grt;
  int edt;
  int ewt;
  int ewf;
  int edf;
  int ndt;
  int nct;
  float c1o;
  float c1s;
  float c2o;
  float c2s;
  float c3o;
  float c3s;
  int ddt;
} FILEDAT;

typedef int ELITE;

typedef char BITFIELD;
typedef struct {
  unsigned len;
  BITFIELD *field;
} BIT;

typedef struct {
  TOUR n;
  TOUR *job;
} MYREP;

typedef struct {
  MYREP *myrep;
  int myGeneration;
  int myTrial;
  double mutprob;
  double mutdim;
  double quality;
  double fitness;
  BOOLEAN needsEval;
} CHROM;

typedef struct {
  int size;
  double fitsum;
  double invfitsum;
  int *fitvec;
  CHROM *rep;
} POPULATION;

typedef struct {
  MYREP *myrep;
  int myGeneration;
  int myTrial;
  double quality;
  double fitness;
} BESTCHROM;

typedef struct {
  int size;
  int cols;
  char file[MAX_STR];
  char **fmt;
  double *val[ROWS];
} BUFFER;

typedef BUFFER *BUFARRAY[BUFCNT];


#endif


/*** end of file ***/
