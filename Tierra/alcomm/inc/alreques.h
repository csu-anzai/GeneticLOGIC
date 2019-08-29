/*
* alreques.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __ALREQUES_H
#define __ALREQUES_H


#define	ALrPRIVATE		0x00	/* major op; minor ops follow: */

#define	ALrNewCLink		0x01
#define	ALrDeadCLink		0x02

typedef struct _al_cldeathinfo	ALtCLDeathInfo, * pALtCLDeathInfo;

struct _al_cldeathinfo {

  pALtOpaque		pData;

};


#endif  /* ifndef __ALREQUES_H; Add nothing past this point */
