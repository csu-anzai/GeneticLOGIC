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
 *  file:	error.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	apr 84
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

	fp = fopen("errors", "a");
	fprintf(fp, "%s\n", s);
	time(&clock);
	fprintf(fp, "%s\n", ctime(&clock));
	fclose(fp);
	fprintf(stderr, "%s\n", s);
	
	exit(1);
}

/*** end of file ***/

