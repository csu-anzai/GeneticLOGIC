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
 *  file:	format.h
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	specify the formats for the input and output files
 */
 

/* FORMAT is the format for both reading and printing the input	*/
/* parameters; IN_VARS and OUT_VARS are the corresponding vars	*/

#define N_FORM 15	/* number of variables in FORMAT */

#define FORMAT "\n\
      Experiments = %d\n\
     Total Trials = %ld\n\
  Population Size = %d\n\
 Structure Length = %d\n\
   Crossover Rate = %lf\n\
    Mutation Rate = %lf\n\
   Generation Gap = %lf\n\
   Scaling Window = %d\n\
  Report Interval = %d\n\
 Structures Saved = %d\n\
Max Gens w/o Eval = %d\n\
    Dump Interval = %d\n\
      Dumps Saved = %d\n\
          Options = %s\n\
      Random Seed = %u\n"

#define IN_VARS &Totalexperiments, &Totaltrials, &Popsize, &Length, \
	&C_rate, &M_rate, &Gapsize, &Windowsize, &Interval, &Savesize, \
	&Maxspin, &Dump_freq, &Num_dumps, Options, &Seed

#define OUT_VARS Totalexperiments, Totaltrials, Popsize, Length, \
	C_rate, M_rate, Gapsize, Windowsize, Interval, Savesize, \
	Maxspin, Dump_freq, Num_dumps, Options, Seed

/* FORM_2 is the format for reading and printing optional input	*/
/* parameters; IN_2 and OUT_2 are the corresponding variables	*/

#define FORM_2 "\
     Maximum Bias = %lf\n\
  Max Convergence = %d\n\
   Conv Threshold = %lf\n\
DPE Time Constant = %d\n\
    Sigma Scaling = %lf\n"

#define IN_2  &Maxbias, &Maxconv, &Convlev, &DPEfreq, &Sigfact
#define OUT_2  Maxbias,  Maxconv,  Convlev,  DPEfreq,  Sigfact

/* separator for application-specific arguments */
#define GARGSEP "--\n"


/* FORM_CKPT is the format used for both reading and printing	*/
/* checkpoint data; IN_CKPT and OUT_CKPT are the matching vars	*/

#define N_CKPT 15	/* number of variables in FORM_CKPT */

#define FORM_CKPT "\
  Experiment %d\n\
   Totonline %le\n\
  Totoffline %le\n\
         Gen %d\n\
       Onsum %le\n\
      Offsum %le\n\
      Trials %ld\n\
     Plateau %ld\n\
        Best %le\n\
       Worst %le\n\
        Spin %d\n\
   Curr_dump %d\n\
     Mu_next %ld\n\
    Lastzoom %d\n\
   Next Seed %u\n"

#define IN_CKPT &Experiment, &Totonline, &Totoffline, &Gen, \
	&Onsum, &Offsum, &Trials, &Plateau, &Best, &Worst, &Spin, \
	&Curr_dump, &Mu_next, &Lastzoom, &Seed

#define OUT_CKPT Experiment, Totonline, Totoffline, Gen, \
	Onsum, Offsum, Trials, Plateau, Best, Worst, Spin, \
	Curr_dump, Mu_next, Lastzoom, Seed



/* OUT_F2 is the format for the data produced by 'Measure' to	*/
/* be used by 'report'; OUT_V2 describes the corresponding vars	*/

#define OUT_F2 "%4d %5ld %3d %3d %5.3f % .5e % .5e % e % e\n"

#define OUT_V2 Gen, Trials, Lost, Conv, Bias, \
	Online, Offline, Best, Ave_current_perf


/*	LINE_FIN is the input format of each line of the outfile	*/
/*	used by the report program; LINE_VIN are the matching vars	*/

#define LINE_FIN "%lf %lf %lf %lf %lf %lf %lf %lf %lf"

#define LINE_VIN  &line[0], &line[1], &line[2], &line[3], \
	&line[4], &line[5], &line[6], &line[7], &line[8]

 
/** end of file **/
