
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	main.c
 *
 *  purpose:	main program for genesis.
 *
 *  modified:	28 mar 86
 *		15 sep 90 - to use display routines
 *		15 aug 93 - added FreeMem to release dynamic memory used. (ECP)
 */

#include "global.h"

extern PROCESS *ProcTable;
extern int Sockets[];



void FreeMem()
{
	register int i;		/* loop control */
	LINKSTRUCT *aux, *auxnext;

	Trace("FreeMem entered");

	if (Vector) free(Vector);
	if (Bitstring) free(Bitstring);

        /* free populations */
	for (i=0; i<Popsize + Migrecv; i++){
		if (Old[i].Gene) free(Old[i].Gene);
		if (New[i].Gene) free(New[i].Gene);
	}
	if (Old) free(Old);
	if (New) free(New);

        /* free migration structures */
	for (i=0; i<Migrecv; i++)
		if (Migrants[i].Gene) free(Migrants[i].Gene);
	if (Migrants) free(Migrants);

        for (i=0; i<Migsend; i++)
        	if (Migset[i].Gene) free(Migset[i].Gene);
        if (Migset) free (Migset);

	if (Window) free(Window);

	for (i=0; i<Savesize; i++)
		if (Bestset[i].Gene) free(Bestset[i].Gene);
	if (Bestset) free(Bestset);

	if (Gene) free(Gene);

	/* free process table */
	if (My_id == 0 && ProcTable){
		for (i=0; i<Processes; i++){
			aux = ProcTable[i].links;
			while (aux){
				auxnext = aux->next_link;
				if (aux) free(aux);
				aux = auxnext;
			}
		}
		free(ProcTable);
	}

	/* free link tables */
	if (Linktable) free(Linktable);
	if (LinkRecTable) free(LinkRecTable);
	
	Trace("FreeMem completed");
}



main(argc,argv)
int argc;
char *argv[];
{
	FILE *fp;
	long clock;
	long time();
	char *ctime();
	void die();
	char ErrMsg[80];
	int i;

	/* see input.c for the use of command line args */
	Input(argc,argv);

	if (Displayflag) {
		initscr();
		signal(SIGINT, die);
		clear();
		refresh();

		if (Interflag)
			Interactive(); /* never returns */

		/* this point is reached only if Interflag is OFF */
		move(1,0);
		printw("run until Trials = %d", Totaltrials);
		move(1, 35);
		printw("executing: ");
		refresh();
	}

	do		/* one experiment */
	{
		if (Traceflag)
			printf("Experiment %d\n", Experiment);

		do	/* see generate.c for main GA loop */
		{
			Generate();
		}
		while (!Doneflag);

		if (Traceflag)
		printf("Online %e   Offline %e    Best %e\n",
			Online, Offline, Best);

		/* accumulate performance measurements */
		Totonline += Online;
		Totoffline += Offline;
		Totbest += Best;

		/* get ready for next experiment */
		Experiment++;
		Gen = 0;

#if UNIX
		if (Processes > 1){
			SendByeLinks();

			if (My_id) SendEnd();
			else RecvEnd();
		}
#endif

	}
	while (Experiment < Totalexperiments);


	/* compute and print final performance measures */

	Totonline /= Totalexperiments;
	Totoffline /= Totalexperiments;
	Totbest /= Totalexperiments;
	if (Onlnflag)
		printf("Online %e\n", Totonline);
	if (Offlnflag)
		printf("Offline %e\n", Totoffline);
	if (Bestflag)
		printf("Best %e\n", Totbest);
	if (Logflag && My_id == 0)
	{
		fp = fopen(Logfile, "a");
		fprintf(fp, "Online %e    ", Totonline);
		fprintf(fp, "Offline %e   ", Totoffline);
		fprintf(fp, "Best %e\n", Totbest);
		time(&clock);
		fprintf(fp, "%s\n", ctime(&clock));
		fclose(fp);
	}

	if (Displayflag) {
		move(23,0);
		die();
	}


	/* free dynamic memory used */
	FreeMem();


	if (My_id == 0){
		if ((fp = fopen(Endfile, "w")) == NULL){
			sprintf(ErrMsg, "main: can't create Endfile %s", Endfile);
			IOError(ErrMsg);
		}
		fclose(fp);
	}

#if UNIX
	for (i=0; i<MAXSOCKETS; i++) close(Sockets[i]);
#endif

	return 0;
}

/*** end of file ***/
