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
 *
 *	$Id$
 *	$Log$
 *
 *
 *     file:    init.c
 *
 *   author:    John J. Grefenstette
 *
 *  created:    1981
 *
 *  purpose:    create an initial population of structures,
 *        	and initalize some performance variables.
 *        	This is called at the start of each experiment.
 *
 *  modified:   7 feb 86
 *
 *		Thomas Baeck, 19 jul 90 
 *			respect fopen() errors.
 *
 */
 


#include "extern.h"
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#define 	fopen(a,b)	fOpen((a),(b))

extern FILE *fOpen();
 
void
Initialize()

{
    	/********************************************************/
    	/* needed for an alternative random Number generator: 	*/
    	/* void srand48();					*/
    	/* void rnd_init();					*/
    	/*							*/
    	/********************************************************/

	extern void		Error();

	void			InzFil(),
				InzInd();

    	FILE 	       	       *fp;

        char 			Msg[NSZ];

    	register int 		i,
				j,
				k,
				Num;

	int			x;

    	Trace("Initialize entered");

	InzFil();			/* file name initialization */

    	Doneflag 	= 0; 		/* prepare for new experiment */
    	Curr_dump 	= 0;
    	Bestsize 	= 0;
    	Mu_next 	= 0;
    	Trials 		= Gen 		= 0;
    	Lost 		= Conv 		= 0;
    	Plateau 	= 0;
    	Spin 		= 0;
    	Onsum 		= Offsum 	= 0.0;

    	for (i = 0; i < Windowsize; i++) {
		Window[i] = 0.0;
	}

	i = 0; 
    	if (Initflag) {        	/* get some structures from Initfile */
        	if ((fp = fopen(Initfile, "r")) == NULL) {
            		sprintf(Msg, "Initialize: can't open %s", Initfile);
            		Error(Msg);
        	}
        	/* get the Number of structures in Initfile */
 
        	for (k = 0; fscanf(fp, "%1d", &x) != EOF; k++) 

        	if (k % Length != 0) {
        		fclose(fp);
            		sprintf(Msg, "Init: bad format in %s", Initfile);
            		Error(Msg);
        	}
 
        	/* Num = Number of structures to be read from file */
 
        	Num = (k / Length < Popsize) ? k / Length : Popsize;
        	for (i = 0; i < Num; i++) {
            		for (j = 0; j < Length; j++) {
                		fscanf(fp,"%1d", &x);
                		New[i].Gene[j] = x;
            		}
            		New[i].Needs_evaluation = 1;
        	}
        	fclose(fp);
    	}
 
    	/************************************************************/
    	/* The seed for the random Number generator is saved        */
    	/* after the initialization of the first population    	    */
    	/* in each experiment.  The saved value is used as the      */
    	/* Seed in subsequent experiments.  The reason is:    	    */
    	/* often we will run several experiments with one set       */
    	/* of parameters, and compare the results with several      */
    	/* experiments run with a different set of parameters.      */
    	/* This means that for all runs which use the        	    */
    	/* same population size, the initial populations for        */
    	/* corresponding experiments will be the same.        	    */
    	/************************************************************/

    	if ( Experiment > 0 ) {
		Seed = Initseed;
	}

    	/************************************************************/
    	/* The normal random Number generator can be exchanged by   */
    	/* e.g. srand48() and rnd_init(). This is commented out     */
    	/* here, but could be implemented later by a new option     */
    	/* which allows chosing between different generators.       */
    	/*    							    */
    	/* srand48((long) Seed);				    */
    	/* rnd_init(Seed);					    */
    	/*							    */
    	/************************************************************/

	InzInd(Popsize - i);		/* init remaining slots randomly */
 
    	Initseed = Seed;
    	Trace("Initialize completed");

} /* end Initialize() */



void
InzInd(InzNbr)

register int	InzNbr;		/* number of worst individuals to be */
				/* initialized. */

{	
	extern double	DcdMttRat();

	int		i,
			j;	

	/* 
	 * 	Randomly intialize InzNbr individuals. 
	 * 	Applications are mainly: InzNbr= Popsize and InzNbr= Popsize-1
	 */

    	for (i = Popsize - InzNbr; i < Popsize; i++) {   
        	for (j = 0; j < Length; j++) {
            		New[i].Gene[j] = Randint(0,1);
		}
		if (NbrMttRts > 0) {
			for (j = 0; j < MttLen; j++) {
				New[i].MttGen[j] = Randint(0,1);
			}
			for (j = 0; j < NbrMttRts; j++) {
				New[i].MttRts[j] = 
					DcdMttRat(&(New[i].MttGen[M_bits * j]));
			}
		}
        	New[i].Needs_evaluation = 1;
    	}

} /* end InzInd */
 

 
void
InzVal()

{
	/*
 	 *	set the global variables to their default values
	 */

	char		*strcpy();

	FctNbr			= FUN_DEF;
	Totalexperiments	= EXP_DEF;
	FctDim			= DIM_DEF;
	Totaltrials		= TRL_DEF;
	Popsize			= PSZ_DEF;
	Length			= LGT_DEF;
	ChrLen			= INTSIZE;
	C_rate			= CRT_DEF;
	C_points		= CPT_DEF;
	M_rate			= MRT_DEF;
	M_bits			= MBT_DEF;
	NbrMttRts		= NBR_DEF;
	MttScm			= MTT_DEF;
	Mu			= NOB_DEF;
	Rho			= RHO_DEF;
	Eta_max			= ETA_DEF;
	SltScm			= SLT_DEF;
	CtlPar			= TMP_DEF;
	ChnLgt			= CHN_DEF;
	Gapsize			= GAP_DEF;
	Windowsize		= WSZ_DEF;
	Interval		= RPI_DEF;
	Savesize		= STR_DEF;
	Maxspin			= MGE_DEF;
	Dump_freq		= DPI_DEF;
	OrigSeed		= RDS_DEF;
	PgmFrq			= PGM_DEF;

	strcpy(RecScm, REC_DEF);
	strcpy(Options, OPT_DEF);
	strcpy(Sfx, "");

} /* end InzVal() */




void
InzFil()

{
	extern void		ChgBuf();

	char			Msg[NSZ];

	register int		i;

    	if (Totalexperiments > 1) {
        	sprintf(Bestfile, "%s.%d", Minfile, Experiment + 1);
		for (i = 0; i < BUFCNT; i++) {
			if (NbrMttRts > 0) {
				sprintf(Msg, "%s.%d", MttFil[i], Experiment+1);
				ChgBuf(MttBuf[i], Msg);
			}
			if (FctDim > 0) {
				sprintf(Msg, "%s.%d", ValFil[i], Experiment+1);
				ChgBuf(ValBuf[i], Msg);
			}
		}
	}

} /* end InzFil */




void
InzGenFil(Ext)
char			*Ext;

{
	int		 CrtDir();

	char		 Msg[NSZ],
			*strcpy();

	if (CrtDir(Ext)) {
		perror(_GS);
		exit(1);
	}
	sprintf(Infile,     "%s%s", "in.", Ext);
	sprintf(Msg, 	    "%s%s", Ext, _PTHSEP);
	sprintf(Outfile,    "%s%s", Msg, "out");
	sprintf(Ckptfile,   "%s%s", Msg, "ckpt");
	sprintf(Minfile,    "%s%s", Msg, "min");
	sprintf(Logfile,    "%s%s", Msg, "log");
	sprintf(Initfile,   "%s%s", Msg, "init");
	sprintf(Schemafile, "%s%s", Msg, "schema");
	sprintf(Popfile,    "%s%s", Msg, "pop");
	sprintf(PrbFil,     "%s%s", Msg, "prb");
	sprintf(Pgmfile,    "%s%s", Msg, "pgm");
	sprintf(MttFil[0],  "%s%s", Msg, "mavg");
	sprintf(MttFil[1],  "%s%s", Msg, "mvar");
	sprintf(MttFil[2],  "%s%s", Msg, "mskw");
	sprintf(ValFil[0],  "%s%s", Msg, "vavg");
	sprintf(ValFil[1],  "%s%s", Msg, "vvar");
	sprintf(ValFil[2],  "%s%s", Msg, "vskw");
    	strcpy(Bestfile, Minfile);

} /* end InzGenFil */




/*
 *	make recursively a directory, if it does not yet exist,
 *	return(-1) if there's any error
 */

int
CrtDir(dir)
char	*dir;				/* directory name */
{
	struct	stat	 sbuf;

	char		*p,
			 Msg[NSZ];

	int		 rc;


	while (stat(dir,&sbuf) < 0) {
		switch(errno) {
			case ENOENT:	/* dir does not exist */
			case ENOTDIR:	/* uncomplete path to dir */
				if ((p= strrchr(dir,'/')) != NULL) {
					*p= '\0';
					rc= CrtDir(dir);
					*p= '/';
					if (rc != 0) {
						return(-1);
					}
				}

				if (mkdir(dir,0775) < 0) {
					sprintf(Msg, "Cannot mkdir(%s)", dir);
					perror(Msg);
					return(-1);
				}
				break;

			default:	/* should not happen */
				sprintf(Msg, "Cannot stat(%s)", dir);
				perror(Msg);
				return(-1);
		}
	}

	if (!S_ISDIR(sbuf.st_mode)) {
		sprintf(Msg, "%s: not a directory", dir);
		perror(Msg);
		return(-1);
	}

	return(0);

} /* end CrtDir */


/*** end of file ***/
