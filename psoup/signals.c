/* Signal-handling stuff for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 */

#ifdef	UNIX

#include "protos.h"
#include <signal.h>
#include <stdio.h>

/* Externs */
extern	short		monflag;

void
sigint_handler(sig, code, scp)
int sig;
int code;
struct sigcontext *scp;
{
	monflag = 1;
#ifdef SYSV
	signal(SIGINT, sigint_handler);
#endif
}

void
init_signals()
{
	signal(SIGINT, sigint_handler);
}

#endif
