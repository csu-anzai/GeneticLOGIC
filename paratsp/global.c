/* $Id: global.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : global.c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef USE_CURSES
#include <curses.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "readfile.h"
#include "trace.h"
#include "global.h"


#ifdef USE_CURSES
WINDOW *MyWin = NULL;		/* output window */
#endif

/* parallel variables */

int MyPopNum = 0;		/* number of populations on processor */
int MyProcID = 0;		/* processor ID */
int NProcs = 1;			/* number of processors */
int NProcsOrig = 1;		/* number of processors on partition */
int P = 0;			/* loop variable for population */

/* input file variables */

FILEDAT FDat = { "", TYP_UNKNOWN, "", 0, 0, GRT_COMPLETE_GRAPH,
                 EDT_UNDIRECTED, EWT_UNKNOWN, EWF_UNKNOWN, EDF_UNKNOWN,
                 NDT_UNWEIGHTED_NODES, NCT_NO_COORDS, 0.0, 1.0, 0.0, 1.0,
                 0.0, 1.0, DDT_NO_DISPLAY
               };		/* tsp-file data */
char *ParserFile = NULL;	/* parser file name */
long ParserLine = 0;		/* parser file line */

/* other global variables */

BOOLEAN AllLocalOpt = TRUE;	/* local opt. for all individuals flag */
double *AvgBestPerf = NULL;	/* average fitness of bestset */
double *AvgCurrPerf = NULL;	/* average fitness of population */
double *Best = NULL;		/* best fitness */
double *BestCurrPerf = NULL;	/* best fitness of population */
BESTCHROM **BestSet = NULL;	/* best individuals of optimization */
int *BestSize = NULL;		/* size of bestset */
BOOLEAN CalcLine = FALSE;	/* calculate distance flag */
int *Conv = NULL;		/* convergence of population */
int CrossOff = 1;		/* number of offsprings for crossover */
int *CurrDump = NULL;		/* counter for dumps */
BOOLEAN DoneAllFlag = FALSE;	/* end of optimization flag */
BOOLEAN *DoneFlag = NULL;	/* end of population flag */
int Experiment = 0;		/* experiment counter */
int *Generation = NULL;		/* generation counter */
int GridXDim = 0;		/* dimension of grid/torus */
unsigned InitSeed = 0;		/* init seed for random number generator */
TOUR *InitTour = NULL;		/* init tour for population init */
TOUR InitTourLen = 0;		/* length of init tour */
BOOLEAN KeyFlag = FALSE;	/* key pressed flag */
LINE **LinePtr = NULL;		/* distance array */
int *Lost = NULL;		/* number of losts */
int *MatePool = NULL;		/* mating pool */
int MaxNumMates = 0;		/* number of entries in mating pool */
int **MyNeighbour = NULL;	/* neighbours in topology */
int NeighbourNum = 0;		/* number of neighbours in topology */
POPULATION **NewPop = NULL;	/* population of childs */
int NumCross = 0;		/* number of crossovers in population */
int NumMates = 0;		/* number of matings */
int NumOff = 0;			/* number of offsprings for crossover */
double *Offline = NULL;		/* offline fitness */
double *OffSum = NULL;		/* sum of offline */
POPULATION **OldPop = NULL;	/* population of parents */
double *Online = NULL;		/* online fitness */
double *OnSum = NULL;		/* sum of online */
TOUR OrderLen = 0;		/* length of tours */
int *Plateau = NULL;		/* trial counter for next output */
BOOLEAN Report = FALSE;		/* report is running flag */
BOOLEAN Running = FALSE;	/* paratsp is running flag */
TOUR *SchemaTour = NULL;	/* schema tour */
unsigned Seed = 0;		/* seed for random number generator */
int *Spin = NULL;		/* number of gens since eval occured */
double *TotalBest = NULL;	/* total for best */
double *TotalOffline = NULL;	/* total for offline */
double *TotalOnline = NULL;	/* total for online */
TOUR TownNum = 0;		/* number of towns */
TOWN *Town = NULL;		/* town coordinates */
int *Trials = NULL;		/* counter of trials */
double **Win = NULL;		/* circular queue of recent worsts */
double *Worst = NULL;		/* worst fitness seen so far */
double *WorstCurrPerf = NULL;	/* worst fitness in population */

/* output variables */

char AllLogFile[MAX_STR];	/* log messages for all populations */
char BestFile[MAX_STR];		/* best tour */
char CkptFile[MAX_STR];		/* checkpoint informations */
char DumpFile[MAX_STR];		/* dumps */
char Extension[MAX_STR];	/* subdirectory */
char GraphFile[MAX_STR];	/* graph file for xtsp */
char LogFile[MAX_STR];		/* log messages for a population */
char MinFile[MAX_STR];		/* best tour */
char Msg[MAX_STR];		/* message */
char OutFile[MAX_STR];		/* output file for results */
char PgmFile[MAX_STR];		/* pgm file */
char PopFile[MAX_STR];		/* population dump */
char SchemaFile[MAX_STR];	/* schema file */
char TourFile[MAX_STR];		/* tour file for tour dumps */
char ValFile[BUFCNT][MAX_STR];	/* value files */
BUFFER **PfmBuf = NULL;		/* fitness buffer */
BUFARRAY *ValBuf = NULL;	/* value buffer */

/* ParaTSP parameter */

char	TspFileName[MAX_STR];	/* tsp input file */
char	BreederSelect;		/* type of breeder selection */
double	CrossRate;		/* crossover rate */
double	MutDim;			/* mutation step rate */
char	Elite;			/* type of elitism */
char	FitnessScale;		/* type of fitness scaling */
double	GapSize;		/* gap size */
int	EliteHold;		/* number of elites */
char	PopInit;		/* type of population init */
char	EliteSelect;		/* type of elite selection */
char	Mutation[2];		/* type of mutation */
TOUR	NormNum;		/* normalize number */
char	GA_Options[MAX_STR];	/* GA-options */
BOOLEAN	CrossTwoOff;		/* two crossover childs flag */
BOOLEAN	Normalize;		/* normalize flag */
BOOLEAN	FilterTwins;		/* filter twins flag */
int	PopSize;		/* population size */
int	CrowFactor;		/* crowding factor */
char	Replace;		/* type of replace scheme */
char	MateSelect;		/* type of mate selection */
char	OptLocal;		/* type of local optimization */
int	WindowSize;		/* window size */
char	Crossover;		/* type of crossover */
double	MutRate;		/* mutation rate */
int	FitParA;		/* fitness parameter a */
int	FitParB;		/* fitness parameter b */
int	FitParC;		/* fitness parameter c */
int	FitParD;		/* fitness parameter d */
int	ChnLen;			/* markov chain length */
int	LowerIndex;		/* lower index for breeder selection */
double	EtaMax;			/* breeder selection parameter */
double	CtrlParam;		/* temperature control parameter */
int	UpperIndex;		/* upper index for breeder selection */
TOUR	CrossInt;		/* crossover interval */
char	TourFileName[MAX_STR];	/* init tour file name */
int	PgmFreq;		/* pgm dump interval */
BOOLEAN	GraphicsFlag;		/* graph flag */
BOOLEAN	DisplayFlag;		/* display flag */
int	TotalExperiments;	/* number of experiments */
int	DumpFreq;		/* dump interval */
int	TotalGenerations;	/* number of generations */
char	InFile[MAX_STR];	/* parameter input file name */
BOOLEAN	OutFlag = FALSE;	/* file output flag */
double	MinQuality;		/* minimal tour length to abort */
int	SaveSize;		/* number of individuals to save in bestset */
char	Options[MAX_STR];	/* other options */
BOOLEAN	AllFlag;		/* evaluate all flag */
BOOLEAN	LastFlag;		/* dump last generation flag */
BOOLEAN	NoTermFlag;		/* simple termination flag */
BOOLEAN	PrintPopFlag;		/* print population flag */
BOOLEAN	SchemaFlag;		/* schema flag */
BOOLEAN	TraceFlag = FALSE;	/* trace flag */
BOOLEAN	VarFlag;		/* var file flag */
int	CommInt;		/* communication interval */
char	ParModel;		/* parallel model */
int	IndNum;			/* number of individuals to send */
int	PopNum;			/* number of populations */
int	ProcNum;		/* number of processors */
int	PollenDirection;	/* pollen spread direction */
char	Topology;		/* topology for neighbour model */
int	LowerWindForce;		/* lower limit for wind force */
int	UpperWindForce;		/* upper limit for wind force */
BOOLEAN	QueryFlag;		/* get suffix flag */
char	RandomType;		/* type of random number generator */
unsigned OrigSeed;		/* original seed for random number generator */
int	TotalTrials;		/* number of trials */
int	PopDisplay;		/* population no. to display */
int	Interval;		/* data collection interval */
BOOLEAN ReportFlag;		/* run report flag */
char	Suffix[MAX_STR];	/* suffix */
int	MaxSpin;		/* number of gens without evaluations */


/*****************************************************************************/
/* Get file name with full path                                              */
/*****************************************************************************/
char *get_fname(fname)
  char *fname;		/* file name */
{ char s[MAX_STR];

  trace("get_fname() entered");

  if (PopNum > 1)
  { sprintf(s, "%s%d%s%s", Extension, P * NProcs + MyProcID + 1, DIRSEP,
      fname);
  }
  else
  { sprintf(s, "%s%s%s", Suffix, DIRSEP, fname);
  }

  trace("get_fname() completed");

  return(s);
}


/*****************************************************************************/
/* Print system error                                                        */
/*****************************************************************************/
void sys_perror(s)
  char *s;
{
  trace("sys_perror() entered");

#ifdef PARIX
  if (MyProcID == DISP_PROC)
  { perror(s);
  }
#else
  perror(s);
#endif

  trace("sys_perror() completed");
}


/*** end of file ***/
