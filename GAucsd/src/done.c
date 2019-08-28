
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1989                                       */
/*  Nicol N. Schraudolph                                     */
/*  Computer Science & Engineering, C-014                    */
/*  University of California, San Diego                      */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:   	done.c
 *
 *  author: 	Nicol N. Schraudolph
 *
 *  created:	August 1989
 *
 *  purpose:	provide for orderly termination on interrupt
 *
 */

#ifndef NOSIGNAL

#define   EXTERN
#include "global.h"

/* #define void int	*/	/* for confused compilers */

extern void Killer();
void (*Catcher)() = Killer;

void Killer(sig, code, scp, addr)
int sig, code;
char *scp, *addr;
{
	if (Sigcount == 2)
	{
		printf("third signal: immediate abort, might leave a mess\n");
		Error("killed by triple signal");
	}
	else if (Sigcount == 1)
	{
		printf("second signal: dump & quit after current generation\n");
		Sigcount = 2;
		signal(SIGINT,  Catcher);
		signal(SIGTERM, Catcher);
	}
	else
	{
		printf("first signal: conclusion after current run\n");
		Totalexperiments = Experiment;
		Sigcount = 1;
		signal(SIGINT,  Catcher);
		signal(SIGTERM, Catcher);
	}
}

#endif  /* !NOSIGNAL */

/* end of file */
