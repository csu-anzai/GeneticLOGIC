
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:       init.c
 *
 *  purpose:    get initial population of structures,
 *              and initalize some performance variables.
 *              This is called at the start of each experiment.
 *
 *  modified:   7 feb 86
 *              12 nov 66:  pass Length to Pack()
 *              23 sep 90:  handle floating point representation in initfile
 *		01 jul 93:  send/receive initial population, initialize
 *			    migration structures (ECP)
 */

/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 */


#include "extern.h"
extern int Sockets[];

void Initialize()
{
	int i;		/* loop control	*/
	register int j;
	int opt;
	char msg[80];

	Trace("Initialize entered");
	Dtrace("initialize");

	if (Totalexperiments > 1)
		sprintf(Bestfile, "%s.%d", Minfile, Experiment+1);

	/* prepare for new experiment */
	Doneflag = 0;
	Curr_dump = 0;
	Bestsize = 0;
	/* set next mutation location */
	if (M_rate < 1.0)
	  Mu_next = (int)(ceil (log(Rand()) / log(1.0 - M_rate)));
	else
	  Mu_next = 1;

	Trials = Gen = 0;
	Lost = Conv = 0;
	Plateau = 0;
	Spin = 0;
	Onsum = Offsum = 0.0;
	for (i=0; i<Windowsize; i++) Window[i] = 0.0;

	/* initialize migration structures */
	for (i=0; i<Linkrecv; i++) LinkRecTable[i].in_use = 1;
	for (i=0; i<Links; i++) Linktable[i].in_use = 1;

	LastMigrant = 0;

	/* set up initial population */

	i = 0;                  /* current structure */

	if (Initflag)
		if (My_id==0)
			i = SendPop();
		else
			i = ReceivePop();

	if (Traceflag){
	sprintf(msg, "Process %d gets %d initial structures (%d)\n", My_id, i, Popsize);
	Trace(msg);
	}

	/********************************************************/
	/* The seed for the random number generator is saved	*/
	/* after the initialization of the first population	*/
	/* in each experiment.  The saved value is used as the	*/
	/* Seed in subsequent experiments.  The reason is:	*/
	/* often we will run several experiments with one set	*/
	/* of parameters, and compare the results with several	*/
	/* experiments run with a different set of parameters.	*/
	/* This means that for all runs which use the		*/
	/* same population size, the initial populations for	*/
	/* corresponding experiments will be the same.		*/
	/********************************************************/
	if ( Experiment > 0 ) Seed = Initseed;

	for (; i < Popsize; i++)   /* initialize remainder randomly */
	{
		for (j = 0; j < Length; j++)
		{
			if (Randint(0,1))
				Bitstring[j] = '1';
			else
				Bitstring[j] = '0';
		}
		Pack(Bitstring , New[i].Gene , Length);
		New[i].Needs_evaluation = 1;
	}

	Initseed = Seed;

	Trace("Initialize completed");
}

/*** end of file ***/
