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
 *	$Id: main.c,v 1.2 1992/06/16 12:13:30 baeck Exp $
 *	$Log: main.c,v $
 * Revision 1.2  1992/06/16  12:13:30  baeck
 * Copyright note added
 *
 * Revision 1.1  1992/06/12  11:35:09  baeck
 * Initial revision
 *
 *
 *
 *      file:   main.c
 *
 *    author:   John J. Grefenstette
 *
 *   created:   1981
 *
 *   purpose:   main program for genesis.
 *
 *  modified:   28 mar 86
 *
 *		Thomas Baeck, 19 jul 90 
 *			respect fopen() errors.
 */
 
#include <time.h>
#include "global.h"
#include <unistd.h>

#define	fopen(a,b)	fOpen((a),(b))

	/* The commands for report generation, plotfile-generation	*/
	/* (see gaxt for details), directory creation and file moving.	*/

#define CMD1		"report -i in.%s"
#define CMD2		"mv in.%s %s"

extern FILE *fOpen(); 
 
 
main(argc,argv)
int 		argc;
char 	      **argv;

{
	extern void 	Input(),
			FreStg(),
			Error(),
			Generate();

    	FILE 	       *fp;

	char 		Cmd[NSZ],
			Msg[NSZ],
    		       *ctime(),
		       *strcpy();

    	time_t		clock,
    			time();

	void		IntLog(),
			DblLog(),
			StrLog();
 
    	Input(argc,argv);		/* command line processing */

        if ((fp = fopen(Logfile, "a")) != NULL) {  /* log activity */
        	time(&clock);
		strcpy(Msg, ctime(&clock));
		StrLog(fp, "Started", Msg);
		fclose(fp);
	}
	else {
              	sprintf(Msg,"Input: can't open %s", Logfile);
               	Error(Msg);
        }

	sprintf(Cmd, CMD2, Sfx, Sfx);	/* move in - file */
	system(Cmd);

    	do {      				/* one experiment 	*/
        	if (Traceflag) {
            		IntLog(stdout, "Experiment", Experiment);
		}

        	do {  		/* see generate.c for main GA loop 	*/
			Generate();
        	} while (!Doneflag);
 
        	if (Traceflag) {	/* trace performance values */
        		IntLog(stdout, "Online", Online);
        		IntLog(stdout, "Offline", Offline);
        		IntLog(stdout, "Best", Best);
		}
 
        	Totonline  += Online;	/* accumulate performance meas. */
        	Totoffline += Offline;
        	Totbest    += Best;

        	if ((fp = fopen(Logfile, "a")) != NULL) {  /* log activity */
			IntLog(fp, "Generation", Gen);  
			IntLog(fp, "Experiment", Experiment);  
			IntLog(fp, "Structures dumped", 
				    (Savesize) ? Savesize : 1);
			fclose(fp);
		}
		else {
              		sprintf(Msg,"Input: can't open %s", Logfile);
               		Error(Msg);
        	}

		DmpPop(Old, Gen - 1, (Savesize) ? Savesize : 1, Logfile);

        	Experiment++;		/* ready for next experiment */
        	Gen = 0;
 
    	} while (Experiment < Totalexperiments);
 
    	Totonline  /= Totalexperiments;	/* compute final performance */
    	Totoffline /= Totalexperiments;
    	Totbest    /= Totalexperiments;

        if ((fp = fopen(Logfile, "a")) != NULL) {  /* log activity */
		IntLog(fp, "Number of Experiments", Totalexperiments);
		DblLog(fp, "Average Online", Totonline);
		DblLog(fp, "Average Offline", Totoffline);
		DblLog(fp, "Average Best", Totbest);
        	time(&clock);
		strcpy(Msg, ctime(&clock));
		StrLog(fp, "Finished", Msg);
        	fclose(fp);
	}
	else {
              	sprintf(Msg,"Input: can't open %s", Logfile);
               	Error(Msg);
        }

	FreStg();			/* free storage */

	sprintf(Cmd, CMD1, Sfx, Sfx);	/* create the report */
	system(Cmd);

	return(0);

} /* end main */




void
IntLog(fp, Msg, val)
FILE		*fp;
char		 Msg[];
int		 val;

{

	register int	i,
			k;

	k = LOGLEN - strlen(Msg);
	fprintf(fp, Msg);
	for (i = 0; i < k; i++)
		fprintf(fp, " ");
	fprintf(fp, ": ");
	fprintf(fp, "%6d\n", val);

} /* end IntLog */ 




void
DblLog(fp, Msg, val)
FILE		*fp;
char		 Msg[];
double		 val;

{

	register int	i,
			k;

	k = LOGLEN - strlen(Msg);
	fprintf(fp, Msg);
	for (i = 0; i < k; i++)
		fprintf(fp, " ");
	fprintf(fp, ": ");
	fprintf(fp, "% 8.6e\n", val);

} /* end DblLog */




void
StrLog(fp, Msg, val)
FILE		*fp;
char		 Msg[];
char		*val;

{

	register int	i,
			k;

	k = LOGLEN - strlen(Msg);
	fprintf(fp, Msg);
	for (i = 0; i < k; i++)
		fprintf(fp, " ");
	fprintf(fp, ": ");
	fprintf(fp, " %s\n", val);

} /* end DblLog */


/** end of file **/
