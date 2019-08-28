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
 *	$Id$
 *	$Log$
 *
 *	file:	setup.c 
 *
 *    author: 	Thomas Baeck
 *
 *   created:	07 apr '91
 *
 *   purpose: 	Realization of an finite state machine for reading and
 *		writing the ga-configuration information.
 *
 *  modified:
 *
 */

 
#include <stdio.h>
#include "global.h"

extern FUNCTION		f_tab[];
extern FILE 	       *fOpen(); 

#define USAGE		"GENEsYs 1.0\t\t\t(c) 1992 Thomas Baeck\n\n\
`setup' provides an interactive way for parameterization\n\
of a run of `ga'\n\
Options:\n\
\t-h      \t\t# This help information\n\n"

#define	OPTSTR		"h"

#define fopen(a,b)	fOpen((a),(b))

#define CMD		"/usr/bin/nice -19 ga -i %s &\n"
#define CMD1		"/usr/bin/nice -19 ga -i %s\n"
#define QFILDEF		"GA-queue"
#define	_DRUNQST	"Execute GA ?                     [%s] : "


main(	argc,
	argv)

int		argc;
char	      **argv;

{
	extern int 	WrtDta(),
			GetDta();

	extern char    *optarg;

	extern int	optind,
			opterr;

	int		Opt;

    	char 		Str[NSZ],
    			Cmd[NSZ],		/* command string */
    			InFil[NSZ],		/* name of InFil */
			QuFil[NSZ];		/* name of queueing file */

	char	       *strcpy();

    	FILE 	       *fp;

	while ((Opt = getopt(argc, argv, OPTSTR)) != -1) {

		switch (Opt) {

			case 'h':
				printf(USAGE);
				exit(0);
				break;

			default:
				printf(USAGE);
				exit(1);
				break;
		}
	}

	strcpy(QuFil, QFILDEF);

	GetDta(stdin);					/* get GA-data */
	sprintf(InFil, "in.%s", Sfx);

    	if ((fp = fopen(InFil, "w")) != NULL) { 	/* write InFil */
		WrtDta(fp);
		fclose(fp);
    	}
	else {
        	printf("Setup: can't open %s (aborted)\n", InFil);
        	exit(1);
	}

	WrtDta(stdout);					/* display data */
    	printf(_DRUNQST, "Yes");
	gets(Str);
  
    	if ((strlen(Str) == 0) || !(strcmp(Str,"yes")) || !(strcmp(Str,"y"))) {
	    	sprintf(Cmd, CMD, InFil);	
        	system(Cmd);
        	printf("GA command \"ga -i %s &\" executed\n", InFil);
    	}
    	else {
        	if ((fp = fopen(QuFil, "a")) != NULL) {
			fprintf(fp, CMD1, InFil);
            		fclose(fp);
		}
        	else {
            		printf("Setup: can't open %s (aborted)\n", QuFil);
        		exit(1);
        	}
        	printf("GA command \"ga -i %s &\" queued to file %s\n", 
			InFil, QuFil);
    	}

	return (0);

} /* end main */

/*** end of file ***/
