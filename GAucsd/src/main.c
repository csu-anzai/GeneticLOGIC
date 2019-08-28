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
 *  file:	main.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	main program for genesis.
 *
 *  modified:	28 mar 86
 */

#include "global.h"

extern void Input();
extern void Generate();


main(argc,argv)
int argc;
char *argv[];
{
	FILE *fp, *fopen();
	long clock;
	long time();
	char *ctime();

	/* see input.c for the use of command line args */
	Input(argc,argv);

	do		/* one run */
	{
		if (Traceflag) 
		{
			if (Restartflag) printf("Restart\n");
			else printf("Run #%d\n", Experiment); 
		}

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

		/* get ready for next run */
		Experiment++;
		Gen = 0;

	} 
	while (Experiment <= Totalexperiments);

	/* compute and print final performance measures */

	Totonline /= Totalexperiments;
	Totoffline /= Totalexperiments;
	Totbest /= Totalexperiments;
	if (Onlnflag)
		printf("%e\n", Totonline);
	if (Offlnflag)
		printf("%e\n", Totoffline);
	if (Bestflag)
		printf("%e\n", Totbest);
	if (Logflag)
	{
		fp = fopen(Logfile, "a");
		fprintf(fp, "Online %e    ", Totonline);
		fprintf(fp, "Offline %e   ", Totoffline);
		fprintf(fp, "Best %e\n", Totbest);
		time(&clock);
		fprintf(fp, "%s\n", ctime(&clock));
		fclose(fp);
	}
}

/** end of file **/
