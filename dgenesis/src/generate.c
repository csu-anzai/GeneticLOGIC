
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	generate.c
 *
 *  purpose:	One generation consists of
 *			(1) forming a new population of structures.
 *			(2) evaluating the population.
 *			(3) gathering performance statistics.
 *
 *  modified:  7 feb 86
 *
 *             2 dec 86: call Measure() before Done() so that
 *             we can quit upon convergence.  Measure() nows
 *             calls Converge().
 *
 *             1 jul 93: add communication point. (ECP)
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
#include <sys/timeb.h>
#include <errno.h>

extern int Sockets[];


void Generate()
{
	static int rsflag = 1;	/* flag cleared after restart		 */
	STRUCTURE *temp;	/* for swapping population pointers	 */
	register int i;		/* for marking structures		 */
	register int j;		/* loop control	 */
	static int nNewMigrants = 0;	/* migrants received in this gen	 */
	int migflag;		/* Migset alredy made in this gen	 */


	if (Traceflag)
		printf("                    Gen %d\n", Gen);
	Trace("Generate entered");


	/* create a new population */

	if (Restartflag && rsflag) {
		/* this is a restart so read checkpoint file */
		Restart();
		rsflag = 0;	/* disable local restart flag. */
		Converge();
	} else if (Gen == 0) {	/* this is a fresh experiment */
		Initialize();	/* form an initial population */
		nNewMigrants = 0;
		if (My_id)
			RecvStart();
		else
			SendStart();
		Spin++;

		if (Processes > 1)
			for (i = 1; i < MAXSOCKETS; i++)
				ClearSocket(Sockets[i]);

	} else {		/* form a new population from */
		/* the old one via genetic operators */

		Select(nNewMigrants);
		nNewMigrants = 0;
		Mutate();
		Crossover();
		if (Eliteflag)
			Elitist();
		if (Allflag)	/* mark structures for evaluation */
			for (i = 0; i < Popsize; i++)
				New[i].Needs_evaluation = 1;
		Spin++;


#if UNIX
		if (Processes > 1) {
			/* communication point */
			migflag = 1;
			for (j = 0; j < Links; j++)
				/* send migrants if comm point reached */
				if ((Gen % Linktable[j].migration_int == 0)) {
					if (migflag) {
						MakeMigset();
						migflag = 0;
					}
					SendMigrants(j);
				}

			/* receive migrants from other processes */
			if (Syncflag) 
				nNewMigrants = ReceiveMigrantsSync();
			else 
				nNewMigrants = ReceiveMigrantsAsync();
			

			if (nNewMigrants) {
				/*printf("(%d) nNewMigrants %d\n", My_id, nNewMigrants); */
				AppendMigrants(nNewMigrants);
			}
#endif
		}
	}


	/* evaluate the newly formed population */
	Evaluate(nNewMigrants);

	/* gather performance statistics */
	Measure(nNewMigrants);

	/* check termination condition for this experiment	 */
	Doneflag = Done();

	/* checkpoint if appropriate */
	if (Num_dumps && Dump_freq && Gen % Dump_freq == 0) {
		if (Num_dumps > 1) {
			sprintf(Dumpfile, "dump.%d", Curr_dump);
			Curr_dump = (Curr_dump + 1) % Num_dumps;
			Checkpoint(Dumpfile);
		}
		Checkpoint(Ckptfile);
	} else {
		if (Doneflag) {
			if (Lastflag)
				Checkpoint(Ckptfile);
			else if (Savesize)
				Printbest();
		}
	}

	/* swap pointers for next generation */
	temp = Old;
	Old = New;
	New = temp;

	/* update generation counter */
	Gen++;

	Trace("Generate completed");
}

/***  end of file ***/
