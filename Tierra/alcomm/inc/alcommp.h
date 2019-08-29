/*
* alcommp.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __ALCOMMP_H
#define __ALCOMMP_H


#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>		/* u_char, ... typedefs */
#include <sys/param.h>		/* MAXHOSTNAMELEN, etc */
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "alcomm.h"
#include "alreques.h"
#include "debug.h"


#define	AL_ScratchStringSize	257

	/*   general scratch string buffer; guaranteed never to be used */
	/* by error functions or error macros, or by alerror.c calls.   */
extern char		_cScratch[ AL_ScratchStringSize ];


typedef struct _al_global_set	ALtGlobalSet;


struct _al_global_set {

  char			sHostname[ MAXHOSTNAMELEN ];
  u_long		ulHostAddr;
  int			iPID;

  int			iDefaultTCPPortalMode;
  int			iDefaultTCPPathwayMode;

  int			iNewCLinkNotify;
  int			iCLinkDeathNotify;

  ALtCLTable		CLT;

  ALtRQueue		RQ;

};


extern ALtGlobalSet		_AS;


#endif  /* ifndef __ALCOMMP_H; Add nothing past this point */
