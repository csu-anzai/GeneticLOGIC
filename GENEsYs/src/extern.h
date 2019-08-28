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
/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/

/*
 *	$Id: extern.h,v 1.3 1992/06/19 16:47:33 baeck Exp $
 *	$Log: extern.h,v $
 * Revision 1.3  1992/06/19  16:47:33  baeck
 * Adaptive recombination removed
 *
 * Revision 1.2  1992/06/16  12:23:01  baeck
 * Copyright note added
 *
 * Revision 1.1  1992/06/12  11:36:04  baeck
 * Initial revision
 *
 *
 *
 *  file:    	extern.h
 *
 *  author:    	John J. Grefenstette
 *
 *  created:    1981
 *
 *  purpose:    external declarations for genesis.
 *
 *  modified:   
 *
 *		Thomas Baeck, 18 jun 90 
 *			Variable `C_points' added which allows chosing one 
 *			or two crossoverpoints.
 *
 *		Thomas Baeck, 22 jun 90 
 *			average mutation rate `Avg_M_rate' added. Variable 
 *			`M_bits' added which defines the number of bits to
 *		  	encode the mutation rate.
 *
 *		Thomas Baeck, 09 jul 90 
 *			`Notermflag' added, which restricts the termination 
 *			criterion to the number of Trials.
 *
 *		Thomas Baeck, 24 jul 90 
 *			`Min_M_rate' and `Max_M_rate' added.
 *
 *		Thomas Baeck, 31 jul 90 
 *			`Mu' added to permit a soft elitist selection 
 *
 *		Thomas Baeck, 06 aug 90 
 *			`Popfile' and Printpopflag added for enabling dumps 
 *			of the decoded population.
 *
 *		Thomas Baeck, 18 oct 90 
 *			Proportionflag and Best_prop added.
 *
 *		Thomas Baeck, 06 dec 90
 *			Sfx (Suffix of files) added.
 *
 *		Thomas Baeck, 28 dec 90
 *			adaptive number of crossover points added.
 *
 *		Thomas Baeck, 20 feb 91
 *			Selection scheme choice added.
 *
 *		Thomas Baeck, 04 mar 91
 *			CtlPar for boltzmann selection added.
 */
 
#include "define.h"

 
    /* The Input file specifies these parameters 			*/
 
extern int    Totalexperiments; /* number of experiments        	*/
extern int    Totaltrials;    	/* trials per experiment        	*/
extern int    Popsize;        	/* population size            		*/
extern int    Length;        	/* bit length of a structure    	*/
extern int    ChrLen;        	/* chromosome length			*/
extern double CtlPar;		/* temperature control parameter	*/
extern int    ChnLgt;		/* cooling duration control parameter	*/
extern double Eta_max;		/* expected val. (ranking), Ba 18 oct 90*/
extern double C_rate;        	/* crossover rate            		*/
extern int    C_points;		/* 1 or 2 crossover-points, Ba 18 jun 90*/
extern double AvgRat;		/* average, minimum, maximum mutation	*/
extern double MinRat;		/* rate, Ba 22 jun 90			*/
extern double MaxRat;
extern double M_rate;        	/* mutation rate            		*/
extern int    NbrMttRts;	/* number of mutation rates		*/
extern int    MttLen;		/* Length of mutation genotype		*/
extern int    M_bits;		/* bitno. for A_M_rate, Ba 22 jun 90	*/
extern int    FctDim;		/* suggested function dim. Ba 24 feb 91 */
extern int    FctNbr;		/* function number */
extern int    F_nbr;		/* number of actual object function	*/
extern double BoxDim, 		/* for f_7 */
	      CstVal;
extern double Gapsize;       	/* fraction of pop replaced per gen    	*/
extern int    Windowsize;    	/* used to update worst performance    	*/
extern int    Interval;        	/* trials between printing statistics   */
extern int    Savesize;        	/* number of structures in minfile    	*/
extern int    Maxspin;        	/* max gens without evals        	*/
extern int    Dump_freq;    	/* gens between checkpointing        	*/
extern int    PgmFrq;		/* interval for bitmap dumps		*/
extern char   Options[NSZ];    	/* option flags                		*/
extern char   Sfx[NSZ];		/* suffix of files, Ba 06 dec 90	*/

extern unsigned int Seed;    	/* seed for random number generator    	*/
extern unsigned int OrigSeed; 	/* original value for random seed    	*/
 
	/* Global variables.    					*/
	/* File names: 							*/

extern char   Bestfile[NSZ];   	/* file of best structures        	*/
extern char   Ckptfile[NSZ];   	/* check point file            		*/
extern int    Curr_dump;    	/* suffix of most recent dumpfile    	*/
extern char   Dumpfile[NSZ];   	/* current dumpfile (if more than one)  */
extern char   Initfile[NSZ];   	/* file of initial structures        	*/
extern char   Infile[NSZ];    	/* input file                		*/
extern char   Logfile[NSZ];    	/* logs starts and restarts        	*/
extern char   Minfile[NSZ];    	/* file prefix of bestfile        	*/
extern char   Outfile[NSZ];    	/* output file                		*/
extern char   Popfile[NSZ];	/* population dump, Ba 06 aug 90	*/
extern char   Pgmfile[NSZ];	/* bitmap dump file			*/
extern char   PrbFil[NSZ];	/* selection probabilities */
extern char   Schemafile[NSZ];  /* file for record a schema's history   */
extern char   MttFil[3][NSZ];	/* files for mutation rate measures */
extern char   ValFil[3][NSZ];	/* files for object variable measures */
 
extern STRUCTURE  *Old;        	/* pointer to population        	*/
extern STRUCTURE  *New;        	/* pointer to population        	*/
extern BESTSTRUCT *Bestset;    	/* set of best structures        	*/
extern BUFFER *MttBuf[BUFCNT],	/* Buffers for mutation rates and 	*/
	      *ValBuf[BUFCNT];	/* object variables 			*/
extern BUFFER *PfmBuf;		/* normal performance buffer 		*/
extern BUFFER *PrbBuf;		/* selection probability buffer 	*/
 
	/* Data collection and loop control variables: 			*/

extern double  Ave_current_perf;/* ave perf in current generation    	*/
extern double  AvgBstPfr;	/* ave perf of best mu values		*/
extern double  Best;        	/* best performance seen so far        	*/
extern double Best_current_perf;/* best perf in current generation    	*/
extern int     Best_guy;        /* index of best_current_perf        	*/
extern int     Bestsize;        /* number of currently saved structures */
extern double  Bias;        	/* ave. domination of alleles        	*/
extern double  Best_prop;	/* proportion of best ind., Ba 18 oct 90*/
extern int     Bytes;        	/* byte-length of packed structures    	*/
extern int     Conv;        	/* number of partially coverged genes   */
extern char    Doneflag;        /* set when termination conditions hold */
extern int     Experiment;    	/* experiment counter            	*/
extern int     Gen;        	/* generation counter            	*/
extern unsigned int Initseed;	/* seed used to initialize population   */
extern int     Lost;        	/* number of totally coverged positions */
extern int     Mu_next;       	/* next mutated position        	*/
extern int     Mu;		/* Mu value fo (m,l)-sel., Ba 31 jul 90	*/
extern int     Rho;		/* (Rho, Mu, lambda) -selection		*/
extern double  Offline;        	/* offline performance            	*/
extern double  Offsum;        	/* accumulator for offline performance  */
extern double  Online;        	/* online performance            	*/
extern double  Onsum;        	/* accumulator for online performance   */
extern int     Plateau;        	/* trial counter for next output    	*/
extern double  Totbest;        	/* total for best            		*/
extern double  Totoffline;    	/* total for offline            	*/
extern double  Totonline;    	/* total for online            		*/
extern int     Trials;        	/* trial counter            		*/
extern double  *Window;        	/* circular queue of recent worsts    	*/
extern double  Worst;        	/* worst performance seen so far    	*/
extern double  Worst_current_perf;/* worst perf in current generation   */
extern int     Spin;        	/* number of gens since eval occurred   */
 
	/* flags set according to the Options string:			*/

extern char   Allflag;        	/* evaluate all structures      	*/
extern char   Eliteflag;    	/* use elitist selection strategy    	*/
extern char   Initflag;        	/* read initial structures        	*/
extern char   Lastflag;        	/* dump last generation        		*/
extern char   MttScm;		/* mutation scheme, Ba 20 feb 91	*/
extern char   Notermflag;	/* simplified termination, Ba 09 jul 90	*/ 
extern char   Printpopflag;	/* dump decoded pop., Ba 06 aug 90	*/
extern char   Proportionflag;	/* best statistics, Ba 18 oct 90	*/
extern char   RecScm[2];	/* recombination scheme			*/
extern char   Schemflag;    	/*  trace history of a schema        	*/
extern char   SltScm;		/* selection scheme, 20 feb 91 		*/
extern char   Traceflag;    	/*  trace execution            		*/
extern char   MttFlg;		/* mutation data collection flag */
extern char   VarFlg;		/* variable data collection flag */
extern char   PrbFlg;		/* selection probability collection flag */
 
/** end of file **/
