
/*
 *  file:	global.h
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	global variables for genesis.
 */

#include "define.h"


/* File names */
char	Bestfile[40];	/* file of best structures		*/
char   Ckptfile[40];	/* check point file			*/
int    Curr_dump;	/* suffix of most recent dumpfile	*/
char   Dumpfile[40];	/* current dumpfile (if more than one)	*/
char   Initfile[40];	/* file of initial structures		*/
char   Infile[40];	/* input file				*/
char   Logfile[40];	/* logs starts and restarts		*/
char   Minfile[40];	/* file prefix of bestfile		*/
char   Outfile[40];	/* output file				*/
char   Schemafile[40];	/* file for record a schema's history	*/
char   Templatefile[40];/* file describing gene interpretation	*/

char   Bitstring[Length+1];/* string representation of chromosome	*/
double Vector[Genes];	 /* floating point representation	*/
GENESTRUCT Gene[Genes];    /* pointer to interpretation records	*/
STRUCTURE  Old[Popsize];	/* pointer to population		*/
STRUCTURE  New[Popsize];	/* pointer to population		*/
BESTSTRUCT Bestset[Savesize];/* set of best structures		*/

/* The Input file specifies these parameters */

int    Totalexperiments;/* number of experiments		*/
int    Totaltrials;	/* trials per experiment		*/
double  C_rate;		/* crossover rate			*/
double  M_rate;		/* mutation rate			*/
double  Gapsize;	/* fraction of pop replaced per gen	*/
int    Interval;	/* trials between printing statistics	*/
int    Maxspin;		/* max gens without evals		*/
int    Dump_freq;	/* gens between checkpointing		*/
int    Num_dumps;	/* number of checkpoint files kept	*/
char   Options[40];	/* option flags				*/
unsigned long    Seed;	/* seed for random number generator	*/
unsigned long    OrigSeed; /* original value for random seed	*/
int Pop_temp;
int Length_temp;
int Window_temp;
int Save_temp;

/* data collection and loop control variables */
double  Ave_current_perf;/* ave perf in current generation	*/
double  Best;		/* best performance seen so far		*/
double  Best_current_perf;/* best perf in current generation	*/
int    Best_guy;	/* index of best_current_perf		*/
int    Bestsize;	/* number of currently saved structures */
double  Bias;		/* ave. domination of alleles		*/
int    Bytes;		/* byte-length of packed structures	*/
int    Conv;		/* number of partially coverged genes	*/
char   Doneflag;	/* set when termination conditions hold	*/
int    Experiment;	/* experiment counter			*/
int    Gen;		/* generation counter			*/
unsigned int    Initseed; /* seed used to initialize population	*/
int    Lost;		/* number of totally coverged positions */
int    Mu_next;		/* next mutated position		*/
double  Offline;	/* offline performance			*/
double  Offsum;		/* accumulator for offline performance	*/
double  Online;		/* online performance			*/
double  Onsum;		/* accumulator for online performance	*/
int    Plateau;		/* trial counter for next output	*/
double Rank_min;	/* minimum sampling rate under ranking	*/
int    Spin;		/* number of gens since eval occurred	*/
double  Totbest;	/* total for best			*/
double  Totoffline;	/* total for offline			*/
double  Totonline;	/* total for online			*/
int    Trials;		/* trial counter			*/
double  Window[Windowsize];/* circular queue of recent worsts	*/
double  Worst;		/* worst performance seen so far	*/
double  Worst_current_perf;/* worst perf in current generation	*/

/* flags set according to the Options string */
char   Allflag;		/*  evaluate all structures		*/
char   Bestflag;	/*  print final best value		*/
char   Collectflag; 	/*  collect performance data in outfile */
char   Convflag;	/*  collect convergence data in outfile */
char   Displayflag;	/*  display statistics each generation	*/
char   Dumpflag;	/*  dump after each evaluation		*/
char   Eliteflag;	/*  use elitist selection strategy	*/
char   Floatflag;	/*  convert strings to floating point	*/
char   Grayflag;	/*  use gray code 			*/
char   Initflag;	/*  read initial structures		*/
char   Interflag;	/*  interactive mode			*/
char   Lastflag;	/*  dump last generation		*/
char   Logflag;		/*  log starts and restarts		*/
char   Maxflag;		/*  maximize instead of minimize	*/
char   Offlnflag;	/*  print final offline measure		*/
char   Onlnflag;	/*  print final online measure		*/
char   Rankflag;	/*  used rank-based selection		*/
char   Restartflag;	/*  restart a run			*/
char   Schemflag;	/*  trace history of a schema		*/
char   Traceflag;	/*  trace execution			*/
char   Timeflag;
char   Probflag;
char   Uniflag;
char   Localflag;
int    Time_index = 0;
int    Time_array[10];
/** end of file **/
