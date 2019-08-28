/* $Id: global.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : global.h                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_GLOBAL_H
#define TSP_GLOBAL_H

#ifdef USE_CURSES
#include <curses.h>
#endif

#include "define.h"


#ifdef USE_CURSES
extern WINDOW *MyWin;
#endif

extern int MyPopNum;
extern int MyProcID;
extern int NProcs;
extern int NProcsOrig;
extern int P;

extern FILEDAT FDat;
extern char *ParserFile;
extern long ParserLine;

extern BOOLEAN AllLocalOpt;
extern double *AvgBestPerf;
extern double *AvgCurrPerf;
extern double *Best;
extern double *BestCurrPerf;
extern BESTCHROM **BestSet;
extern int *BestSize;
extern BOOLEAN CalcLine;
extern int *Conv;
extern int CrossOff;
extern int *CurrDump;
extern BOOLEAN DoneAllFlag;
extern BOOLEAN *DoneFlag;
extern int Experiment;
extern int *Generation;
extern int GridXDim;
extern unsigned InitSeed;
extern TOUR *InitTour;
extern TOUR InitTourLen;
extern BOOLEAN KeyFlag;
extern LINE **LinePtr;
extern int *Lost;
extern int *MatePool;
extern int MaxNumMates;
extern int **MyNeighbour;
extern int NeighbourNum;
extern POPULATION **NewPop;
extern int NumCross;
extern int NumMates;
extern int NumOff;
extern double *Offline;
extern double *OffSum;
extern POPULATION **OldPop;
extern double *Online;
extern double *OnSum;
extern TOUR OrderLen;
extern int *Plateau;
extern BOOLEAN Report;
extern BOOLEAN Running;
extern TOUR *SchemaTour;
extern unsigned Seed;
extern int *Spin;
extern double *TotalBest;
extern double *TotalOffline;
extern double *TotalOnline;
extern TOWN *Town;
extern TOUR TownNum;
extern int *Trials;
extern double **Win;
extern double *Worst;
extern double *WorstCurrPerf;

extern char AllLogFile[MAX_STR];
extern char BestFile[MAX_STR];
extern char CkptFile[MAX_STR];
extern char DumpFile[MAX_STR];
extern char Extension[MAX_STR];
extern char GraphFile[MAX_STR];
extern char LogFile[MAX_STR];
extern char MinFile[MAX_STR];
extern char Msg[MAX_STR];
extern char OutFile[MAX_STR];
extern char PgmFile[MAX_STR];
extern char PopFile[MAX_STR];
extern char SchemaFile[MAX_STR];
extern char TourFile[MAX_STR];
extern char ValFile[3][MAX_STR];
extern BUFFER **PfmBuf;
extern BUFARRAY *ValBuf;

extern char	TspFileName[MAX_STR];
extern char	BreederSelect;
extern double	CrossRate;
extern double	MutDim;
extern char	Elite;
extern char	FitnessScale;
extern double	GapSize;
extern int	EliteHold;
extern char	PopInit;
extern char	EliteSelect;
extern char	Mutation[2];
extern TOUR	NormNum;
extern char	GA_Options[MAX_STR];
extern BOOLEAN	CrossTwoOff;
extern BOOLEAN	Normalize;
extern BOOLEAN	FilterTwins;
extern int	PopSize;
extern int	CrowFactor;
extern char	Replace;
extern char	MateSelect;
extern char	OptLocal;
extern int	WindowSize;
extern char	Crossover;
extern double	MutRate;
extern int	FitParA;
extern int	FitParB;
extern int	FitParC;
extern int	FitParD;
extern int	ChnLen;
extern int	LowerIndex;
extern double	EtaMax;
extern double	CtrlParam;
extern int	UpperIndex;
extern TOUR	CrossInt;
extern char	TourFileName[MAX_STR];
extern int	PgmFreq;
extern BOOLEAN	GraphicsFlag;
extern BOOLEAN	DisplayFlag;
extern int	TotalExperiments;
extern int	DumpFreq;
extern int	TotalGenerations;
extern char	InFile[MAX_STR];
extern BOOLEAN	OutFlag;
extern double	MinQuality;
extern int	SaveSize;
extern char	Options[MAX_STR];
extern BOOLEAN	AllFlag;
extern BOOLEAN	LastFlag;
extern BOOLEAN	NoTermFlag;
extern BOOLEAN	PrintPopFlag;
extern BOOLEAN	SchemaFlag;
extern BOOLEAN	TraceFlag;
extern BOOLEAN	VarFlag;
extern int	CommInt;
extern char	ParModel;
extern int	IndNum;
extern int	PopNum;
extern int	ProcNum;
extern int	PollenDirection;
extern char	Topology;
extern int	LowerWindForce;
extern int	UpperWindForce;
extern BOOLEAN	QueryFlag;
extern char	RandomType;
extern unsigned	OrigSeed;
extern int	TotalTrials;
extern int	PopDisplay;
extern int	Interval;
extern BOOLEAN	ReportFlag;
extern char	Suffix[MAX_STR];
extern int	MaxSpin;


#ifdef USE_PROTO
extern char *get_fname(char *);
extern void sys_perror(char *);
#else
extern char *get_fname();
extern void sys_perror();
#endif


#endif


/*** end of file ***/
