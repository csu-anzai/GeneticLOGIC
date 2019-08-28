
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:       format.h
 *
 *  purpose:    specify the formats for the input and output files
 *
 *  modified:   23/4/93 Changed OUT_F2 separators to tabs. (ECP)
 *
 */


/* the input file is read in according to IN_FORMAT and IN_VARS */

#define IN_FORMAT " \
        Processes = %d \
      Experiments = %d \
     Total Trials = %d \
 Total Population = %d \
 Structure Length = %d \
   Crossover Rate = %lf \
    Mutation Rate = %lf \
   Generation Gap = %lf \
   Scaling Window = %d \
  Report Interval = %d \
 Structures Saved = %d \
Max Gens w/o Eval = %d \
    Dump Interval = %d \
      Dumps Saved = %d \
	  Options = %s \
      Random Seed = %lu \
	 Rank Min = %lf "

#define IN_VARS &Processes,&Totalexperiments,&Totaltrials,&Totalpop,&Length,\
	&C_rate,&M_rate,&Gapsize,\
	&Windowsize,&Interval,\
	&Savesize,&Maxspin,\
	&Dump_freq,&Num_dumps,\
	Options,\
	&OrigSeed, &Rank_min



/*      LINE_FIN is the input format of each line of the outfile        */
/*      used by the report program                                      */
#define LINE_FIN "%lf %lf %lf %lf %lf %lf %lf %lf %lf"

#define LINE_VIN &line[0],&line[1],&line[2],&line[3],&line[4],\
	&line[5],&line[6],&line[7],&line[8]


/*      Output formats.         */

/* OUT_FORMAT is the format for printing the input parameters */

#define OUT_FORMAT "\
        Processes = %d\n\
      Experiments = %d\n\
     Total Trials = %d\n\
 Total Population = %d\n\
 Structure Length = %d\n\
   Crossover Rate = %5.3f\n\
    Mutation Rate = %5.3f\n\
   Generation Gap = %5.3f\n\
   Scaling Window = %d\n\
  Report Interval = %d\n\
 Structures Saved = %d\n\
Max Gens w/o Eval = %d\n\
    Dump Interval = %d\n\
      Dumps Saved = %d\n\
	  Options = %s\n\
      Random Seed = %lu\n\
	 Rank Min = %5.3f\n"

/* OUT_VARS are the parameters to be printed according to OUT_FORMAT */

#define OUT_VARS Processes,Totalexperiments,Totaltrials,Totalpop,Length,\
	C_rate,M_rate,Gapsize,\
	Windowsize,Interval,Savesize,Maxspin,\
	Dump_freq,Num_dumps,Options,OrigSeed,Rank_min


/* SEND_FORMAT is the format to send parameters to processes */
#define SEND_FORMAT "\
        Processes = %d\n\
      Experiments = %d\n\
     Total Trials = %d\n\
 Total Population = %d\n\
 Structure Length = %d\n\
   Crossover Rate = %5.3f\n\
    Mutation Rate = %5.3f\n\
   Generation Gap = %5.3f\n\
   Scaling Window = %d\n\
  Report Interval = %d\n\
 Structures Saved = %d\n\
Max Gens w/o Eval = %d\n\
    Dump Interval = %d\n\
      Dumps Saved = %d\n\
	  Options = %s\n\
      Random Seed = %lu\n\
	 Rank Min = %5.3f\n"

/* SEND_VARS are the parameters to be printed according to SEND_FORMAT
 *       the difference with OUT_VARS is that here Seed is used instead
 *       of OrigSeed
 */

#define SEND_VARS Processes,Totalexperiments,Totaltrials,Totalpop,Length,\
	C_rate,M_rate,Gapsize,\
	Windowsize,Interval,Savesize,Maxspin,\
	Dump_freq,Num_dumps,Options,Seed,Rank_min



/* OUT_F2 is the format for the data produced by 'Measure'
 * to be written in the Outfile.
 * OUT_V2 describes the variables.
 */
#define OUT_F2 "%5d\t%5d\t%2d\t%2d\t%5.3f\t%.6e\t%.6e\t%.6e\t%.6e\n"

#define OUT_V2 Gen,Trials,Lost,Conv,Bias,Online,\
	Offline,Best,Ave_current_perf

/* LINK_FORMAT specifies the format used to send and print link info:
 * port, IP address, migration interval, migration rate */
#define LINK_FORMAT "%d %s %d %f "

/*** end of file ***/
