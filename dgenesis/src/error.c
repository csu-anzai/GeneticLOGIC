
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	error.c
 *
 *  purpose:	print message in error log file and abort.
 *
 *  modified:	7 feb 86
 *		27 apr 93	add IOError function (ECP)
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

void Error(s)
char *s;
{
	FILE *fp;
	long clock;
	long time();
	char *ctime();

	fp = fopen("log.error", "a");
	fprintf(fp, "\nERROR %s\n", s);
	time(&clock);
	fprintf(fp, "%s\n", ctime(&clock));
	fclose(fp);
	fprintf(stderr, "\nERROR %s\n", s);

	exit(1);
}


/* writes a message and a system error string to stderr */
void IOError(s)
char *s;
{
	fprintf(stderr, "\nERROR ");
        perror(s);
        exit(1);
}

/*** end of file ***/
