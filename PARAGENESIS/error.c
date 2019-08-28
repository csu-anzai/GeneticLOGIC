
/*
 *  file:	error.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	print message in error log file and abort.
 *
 *  modified:	7 feb 86
 */

#include <stdio.h>

Error(s)
char *s;
{
	FILE *fopen(), *fp;
	long clock;
	long time();
	char *ctime();

	fp = fopen("log.error", "a");
	fprintf(fp, "%s\n", s);
	time(&clock);
	fprintf(fp, "%s\n", ctime(&clock));
	fclose(fp);
	fprintf(stderr, "%s\n", s);
	
	exit(1);
}

/*** end of file ***/
