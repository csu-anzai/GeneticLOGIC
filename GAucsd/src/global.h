/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
 

/*
 *  file:   global.h
 *
 *  author: John J. Grefenstette
 *
 *  created:    1981
 *
 *  purpose:    global variables for genesis.
 *
 *  modified:   07 may 90  added name_file()  - Nici.
 *              12 jun 92  folded in extern.h - Nici.
 */

#include <stdio.h>
#include <math.h>

#include "define.h"
#include "format.h"

#ifdef  EXTERN
#define GLOBAL(T,N,V) extern T N
#define GARRAY(T,N,I) extern T N[]
#else
#define GLOBAL(T,N,V) T N = V
#define GARRAY(T,N,I) T N[I]
#endif

extern char *calloc();
extern char *malloc();
extern double  Rand();

/* The Input file specifies these parameters */
GLOBAL(int,    Totalexperiments, 1);     /* number of experiments   */
GLOBAL(long,   Totaltrials,   5000L);    /* trials per experiment   */
GLOBAL(int,    Popsize,         50);     /* population size         */
GLOBAL(int,    Length,          30);     /* bit length of structure */
GLOBAL(double, C_rate,           0.6);   /* crossover rate          */
GLOBAL(double, M_rate,           0.005); /* mutation rate           */
GLOBAL(double, Convlev,          0.95);  /* convergence threshold   */
GLOBAL(double, Gapsize,          1.0);   /* fraction of pop replaced per gen */
GLOBAL(int,    Windowsize,      -1);     /* used to update worst performance */
GLOBAL(int,    Interval,       500);     /* trials between statistic reports */
GLOBAL(int,    DPEfreq,          0);     /* DPE smoothing time constant     */
GLOBAL(int,    Savesize,         1);     /* number of structures in minfile */
GLOBAL(int,    Maxspin,          2);     /* max gens without evaluations    */
GLOBAL(double, Maxbias,          0.99);  /* maximum bias to reach           */
GLOBAL(int,    Maxconv,          0);     /* maximum alleles to converge     */
GLOBAL(double, Sigfact,          2.0);   /* sigma scaling factor            */
GLOBAL(int,    Dump_freq,        0);     /* gens between checkpointing      */
GLOBAL(int,    Num_dumps,        1);     /* number of checkpoint files kept */
GLOBAL(char,   Options[20],   "Aclu");   /* option flags                    */
GLOBAL(unsigned int, Seed, 123456789);   /* seed for random numbers         */

#ifndef UTILITY   /* skip the rest if this is for inset.c or report.c */

/* File names */
GARRAY(char, Bestfile,   40);   /* file of best structures  */
GARRAY(char, Ckptfile,   40);   /* name of check point file */
GLOBAL(int,  Curr_dump,   0);   /* suffix of last dumpfile  */
GARRAY(char, Dumpfile,   40);   /* current dumpfile name    */
GARRAY(char, Initfile,   40);   /* file of initial genomes  */
GARRAY(char, Infile,     40);   /* input file               */
GARRAY(char, Logfile,    40);   /* logs starts and restarts */
GARRAY(char, Minfile,    40);   /* file prefix of bestfile  */
GARRAY(char, Outfile,    40);   /* output file              */
GARRAY(char, Schemafile, 40);   /* schema history record    */
GARRAY(char, DPEfile,    40);   /* record of DPE parameters */

GLOBAL(char,       *Buff,    NULL); /* buffer for unpacked gene */
GLOBAL(STRUCTURE,  *Old,     NULL); /* pointer to population    */
GLOBAL(STRUCTURE,  *New,     NULL); /* pointer to population    */
GLOBAL(BESTSTRUCT, *Bestset, NULL); /* set of best structures   */

/* data collection and loop control variables */
GLOBAL(int,     Best_guy,   0);     /* index of best_current_perf       */
GLOBAL(int,     Bestsize,   0);     /* number of currently saved structures */
GLOBAL(int,     Bytes,      0);     /* byte-length of packed structures */
GLOBAL(int,     Full,       0);     /* number of full bytes in genome */
GLOBAL(int,     Slop,       0);     /* number of bits in last byte */
GLOBAL(int,     Conv,       0);     /* number of partially coverged genes   */
GLOBAL(int,     Experiment, 0);     /* experiment counter       */
GLOBAL(int,     Gen,        0);     /* generation counter       */
GLOBAL(long,    Mu_next,    0L);    /* next mutated position    */
GLOBAL(long,    Trials,     0L);    /* trial counter            */
GLOBAL(long,    Plateau,    0L);    /* counter for next output  */
GLOBAL(double,  Offline,    0.0);   /* offline performance      */
GLOBAL(double,  Online,     0.0);   /* online performance       */
GLOBAL(double,  Offsum,     0.0);   /* accumulator for offline performance  */
GLOBAL(double,  Onsum,      0.0);   /* accumulator for online performance   */
GLOBAL(double,  Best,       0.0);   /* best performance seen so far     */
GLOBAL(double,  Bias,       0.0);   /* ave. domination of alleles       */
GLOBAL(int,     Spin,       0);     /* number of gens since eval occurred   */
GLOBAL(int,     Lost,       0);     /* number of totally coverged positions */
GLOBAL(int,     Phenesize,  0);     /* max length of phenotype descriptions */
GLOBAL(int,     Lastzoom,   0);     /* last generation a DPE zoom occurred  */
GLOBAL(int,     Sigcount,   0);     /* counter for termination signals */
GLOBAL(char,    Doneflag,   0);     /* set when termination conditions hold */
GLOBAL(double,  Totbest,    0.0);   /* total for best           */
GLOBAL(double,  Totoffline, 0.0);   /* total for offline        */
GLOBAL(double,  Totonline,  0.0);   /* total for online         */
GLOBAL(double, *DPEhist,    NULL);  /* pointer to DPE history   */
GLOBAL(double, *Window,     NULL);  /* circular queue of recent worsts  */
GLOBAL(double, *Scale,      NULL);  /* loads DPEscale for each experiment */
GLOBAL(double, *Basis,      NULL);  /* loads DPEbasis for each experiment */
GLOBAL(double,  Worst,      0.0);   /* worst performance seen so far    */
GLOBAL(double,  Ave_current_perf,   0.0);  /* avg perf in current gen   */
GLOBAL(double,  Best_current_perf,  0.0);  /* best perf in current gen  */
GLOBAL(double,  Worst_current_perf, 0.0);  /* worst perf in current gen */
GLOBAL(double,  Few,        0.0);   /* replaces former FEW constant */

GARRAY(int,  Pre, CHAR_BIT);  /*   left bit mask */
GARRAY(int, Post, CHAR_BIT);  /*  right bit mask */
GARRAY(int,  Bit, CHAR_BIT);  /* single bit mask */

/* application-specific arguments */
GLOBAL(int,   GArgc, 0);
GARRAY(char, *GArgv, MAXGARG);

extern int    GAgenes,  GAposn[];
extern double GAfact[], GAbase[];

/* flags set according to the Options string */
GLOBAL(char, Allflag,     0);  /* evaluate all structures      */
GLOBAL(char, Aliasflag,   0);  /* avoid aliassing in Ctoi()    */
GLOBAL(char, Bestflag,    0);  /* print final best value       */
GLOBAL(char, Collectflag, 0);  /* performance data in outfile  */
GLOBAL(char, Convflag,    0);  /* convergence data in outfile  */
GLOBAL(char, Dumpflag,    0);  /* dump after each evaluation   */
GLOBAL(char, Eliteflag,   0);  /* elitist selection strategy   */
GLOBAL(char, Initflag,    0);  /* read initial structures      */
GLOBAL(char, Lastflag,    0);  /* dump the last generation     */
GLOBAL(char, Logflag,     0);  /* log starts and restarts      */
GLOBAL(char, Offlnflag,   0);  /* print final offline measure  */
GLOBAL(char, Onlnflag,    0);  /* print final online measure   */
GLOBAL(char, Restartflag, 0);  /* restart from last checkpoint */
GLOBAL(char, Schemflag,   0);  /* trace history of a schema    */
GLOBAL(char, Traceflag,   0);  /* trace program execution      */
GLOBAL(char, Uniflag,     0);  /* super-uniform initialization */

#endif  /* !UTILITY */

#ifndef EXTERN

/* procedure for setting up GAucsd file names: */

name_file(name, ext, base)
char *name, *ext, *base;
{
    if (base) if (*base)
    {
		sprintf(name, "%s.%s", base, ext);

#ifndef NOSTAT
		{
			struct stat buf[1];

			/* if subdir exists, use it */
			if (!stat(ext, buf))
				if ((buf->st_mode & S_IFDIR) == S_IFDIR)
					sprintf(name, "%s%s%s", ext, DS, base);
		}
#endif  /* !NOSTAT */
    }
    else strcpy(name, ext);
}

#endif  /* !EXTERN */

/*** end of file ***/

