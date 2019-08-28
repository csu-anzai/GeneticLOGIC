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
 *
 *	$Id$
 *	$Log$
 *
 *
 *      file:    error.c
 *
 *    author:    John J. Grefenstette
 *
 *   created:    apr 84
 *
 *   purpose:    print message in log file and abort.
 *
 *  modified:    7 feb 86
 *
 *		 Thomas Baeck, 19 jul 90 
 *			respect fopen() errors.
 */
 
#include <stdio.h>
#include "extern.h"

void
Error(s)
char *s;

{
    	FILE 	       *fp;

    	long 		clock;
    	long 		time();

    	char 	       *ctime();

	if ((fp = fopen(Logfile, "a")) != NULL) {  /* log error */ 
    		fprintf(fp, "Error:       %s\n", s);
    		time(&clock);
    		fprintf(fp, "Terminating! %s\n", ctime(&clock));
    		fclose(fp);
	}
	else {
    		fprintf(stderr, "%s\n", s);
	}
    	exit(1);

} /* end Error() */
 
/*** end of file ***/
 
