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
 *	$Id$
 *	$Log$
 *
 *
 *	file:	fOpen.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:	respect fopen() errors and switch according to errno.
 *
 */

#include "extern.h"
#include <errno.h> 

#define _STIM	3600		/* maximum delay for sleep */


FILE	*
fOpen(Fil, mode)
char		*Fil,			/* filename */
		*mode;			/* mode	*/

{
    	FILE 		*fopen(),
	 		*fp;

	char		 Msg[NSZ];

	int 		 Dly = 1;

    	while ((fp = fopen(Fil, mode)) == NULL) {
		sprintf(Msg, "%s: fOpen(%s, %s) error (errno = %d)", 
			_GS, Fil, mode, errno);
		perror(Msg);
		sleep(Dly);
		if (Dly < _STIM)	/* increase / reset delay */
			Dly *= 2;
		else
			Dly  = 1;
    	}

	return(fp);

} /* end fOpen */



/*** end of file ***/
