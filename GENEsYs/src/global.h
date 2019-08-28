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
 *	$Id: global.h,v 1.3 1992/06/19 16:46:41 baeck Exp $
 *	$Log: global.h,v $
 * Revision 1.3  1992/06/19  16:46:41  baeck
 * Adaptive recombination removed
 *
 * Revision 1.2  1992/06/16  12:23:35  baeck
 * Copyright note added
 *
 * Revision 1.1  1992/06/12  11:36:07  baeck
 * Initial revision
 *
 *
 *
 *  file:    	global.h
 *
 *  author:    	John J. Grefenstette
 *
 *  created:    1981
 *
 *  purpose:    global variables for genesis.
 *
 *  modified:	
 *		Thomas Baeck, 18 jun 90 
 *			Variable `C_points' added.  
 *
 *		Thomas Baeck, 22 jun 90 
 *			average mutation rate `Avg_M_rate' added.
 *	
 *		Thomas Baeck, 25 jun 90 
 *			Variable `M_bits' added for encoding the mutation rate.
 *	
 *		Thomas Baeck, 09 jul 90 
 *			`Notermflag' added for simplified termination criterion.
 *
 *		Thomas Baeck, 24 jul 90 
 *			`Max_M_rate' and `Min_M_rate' added.
 *	
 *		Thomas Baeck, 31 jul 90 
 *			`mu' added to permit a soft elitist selection 
 *	
 *		Thomas Baeck, 06 aug 90 
 *			`Popfile' and Printpopflag added for enabling 
 *			dumps of the decoded population.
 *	
 *		Thomas Baeck, 18 oct 90 
 *			'Proportionflag' and 'Best_prop' added.
 *			NSZ as a unit length added. 
 *			`Eta_max' added for Ranking.
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
 *		Thomas Baeck, 20 feb 91
 *			Mutation scheme choice added.
 *
 *		Thomas Baeck, 04 mar 91
 *			CtlPar for boltzmann selection added.
 *
 *		Thomas Baeck, 23 apr 91
 *			Number of mutation rates added.
 */
 
#include "define.h"
 
    /* File names:							*/

char   Bestfile[NSZ];    	/* file of best structures        	*/
char   Ckptfile[NSZ];    	/* check point file            		*/
int    Curr_dump;    		/* suffix of most recent dumpfile    	*/
char   Dumpfile[NSZ];    	/* current dumpfile (if more than one)  */
char   Initfile[NSZ];    	/* file of initial structures        	*/
char   Infile[NSZ];    		/* input file                		*/
char   Logfile[NSZ];   		/* logs starts and restarts        	*/
char   Minfile[NSZ];   		/* file prefix of bestfile        	*/
char   Outfile[NSZ];   		/* output file                		*/
char   Popfile[NSZ];		/* population dumps, Ba 06 aug 90	*/
char   Pgmfile[NSZ];		/* bitmap dumps of best indidivual	*/
char   PrbFil[NSZ];		/* selection probabilities */
char   Schemafile[NSZ];    	/* file for record a schema's history   */
char   ValFil[3][NSZ];		/* files for object variable measures */
char   MttFil[3][NSZ];		/* files for mutation rate measures */
STRUCTURE  *Old;    		/* pointer to population        	*/
STRUCTURE  *New;    		/* pointer to population        	*/
BESTSTRUCT *Bestset;    	/* set of best structures        	*/
BUFFER 	*MttBuf[BUFCNT],	/* Buffers for mutation rates and */
	*ValBuf[BUFCNT];	/* object variables */
BUFFER  *PfmBuf;		/* normal performance buffer */
BUFFER  *PrbBuf;		/* selection probability buffer */

 
    /* The Input file specifies these parameters:			*/
 
int    Totalexperiments;	/* number of experiments        	*/
int    Totaltrials;    		/* trials per experiment        	*/
int    Popsize;        		/* population size            		*/
int    Length;        		/* bit length of a structure        	*/
int    ChrLen;        		/* chromosome length			*/
double Eta_max;			/* expected val. (ranking), Ba 18 oct 90*/
double CtlPar;			/* temperature control parameter	*/
int    ChnLgt;			/* cooling duration control parameter	*/

double C_rate;        		/* crossover rate            		*/
int    C_points;		/* crossover-points, Ba 18 jun 90	*/

double AvgRat;			/* average, minimum, and maximum	*/
double MinRat;			/* mutation rate, Ba 22 jun 90		*/
double MaxRat;
double M_rate;        		/* mutation rate            		*/
int    NbrMttRts;		/* number of mutation rates		*/
int    MttLen;			/* Length of mutation genoype		*/
int    M_bits;			/* bitno, for A_M_rate, Ba 22 jun 90	*/

int    	FctDim;			/* suggested function dim. Ba 24 feb 91 */
int    	FctNbr;			/* function number */
int    	F_nbr;			/* number of actual object function	*/
double	BoxDim, 		/* for f_7 */
	CstVal;
double Gapsize;    		/* fraction of pop replaced per gen    	*/
int    Windowsize;    		/* used to update worst performance    	*/
int    Interval;    		/* trials between printing statistics  	*/
int    Savesize;    		/* number of structures in minfile    	*/
int    Maxspin;        		/* max gens without evals        	*/
int    Dump_freq;    		/* gens between checkpointing        	*/
int    PgmFrq;			/* interval for bitmap dumps		*/
char   Options[NSZ];   		/* option flags                		*/
char   Sfx[NSZ];		/* suffix of files, Ba 06 dec 90	*/

unsigned int    Seed;    	/* seed for random number generator    	*/
unsigned int    OrigSeed; 	/* original value for random seed    	*/
 
	/* Data collection and loop control variables: 			*/

double  Ave_current_perf;	/* ave perf in current generation 	*/
double  AvgBstPfr;		/* ave perf of best mu values		*/
double  Best;        		/* best performance seen so far        	*/
double  Best_current_perf;	/* best perf in current generation    	*/
int     Best_guy;    		/* index of best_current_perf        	*/
int     Bestsize;    		/* number of currently saved structures */
double  Bias;        		/* ave. domination of alleles        	*/
double  Best_prop;		/* proportion of best ind., Ba 18 oct 90*/
int     Bytes;        		/* byte-length of packed structures    	*/
int     Conv;        		/* number of partially coverged genes   */
char    Doneflag;    		/* set when termination conditions hold */
int     Experiment;    		/* experiment counter            	*/
int     Gen;        		/* generation counter            	*/
unsigned int    Initseed; 	/* seed used to initialize population   */
int     Lost;        		/* number of totally coverged positions */
int     Mu_next;        	/* next mutated position        	*/
int	Mu;			/* m-value for (m,l)-sel., Ba 31 jul 90	*/
int     Rho;			/* (Rho, Mu, lambda) -selection		*/
double  Offline;    		/* offline performance            	*/
double  Offsum;        		/* accumulator for offline performance  */
double  Online;        		/* online performance            	*/
double  Onsum;        		/* accumulator for online performance   */
int     Plateau;        	/* trial counter for next output    	*/
int     Spin;        		/* number of gens since eval occurred   */
double  Totbest;    		/* total for best            		*/
double  Totoffline;   		/* total for offline            	*/
double  Totonline;    		/* total for online            		*/
int     Trials;        		/* trial counter            		*/
double  *Window;    		/* circular queue of recent worsts    	*/
double  Worst;        		/* worst performance seen so far    	*/
double  Worst_current_perf;	/* worst perf in current generation    	*/
 
    	/* flags set according to the Options string:			*/

char   Allflag;        		/* evaluate all structures        	*/
char   Eliteflag;    		/* use elitist selection strategy    	*/
char   Initflag;    		/* read initial structures        	*/
char   Lastflag;    		/* dump last generation        		*/
char   MttScm;			/* mutation scheme 			*/
char   Notermflag;		/* simplified termination, Ba 09 jul 90	*/
char   Printpopflag;		/* dump decoded pop., Ba o6 aug 90	*/
char   Proportionflag;		/* best statistics, Ba 18 oct 90	*/
char   RecScm[2];		/* recombination scheme			*/
char   Schemflag;    		/* trace history of a schema        	*/
char   SltScm;			/* selection scheme, 20 feb 91 		*/
char   Traceflag;    		/*  trace execution            		*/
char   MttFlg;			/* mutation data collection flag */
char   VarFlg;			/* variable data collection flag */
char   PrbFlg;			/* selection probability collection flag */
 
/** end of file **/
