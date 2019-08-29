/*
* alcomm.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __ALCOMM_H
#define __ALCOMM_H

#include "portlayr.h"


#define	AL_False	0
#define	AL_True		(! AL_False)

#define	AL_NoWait	1
#define	AL_Wait		2

#define	AL_Default	0
#define	AL_Asynch	1
#define	AL_Synch	2

#define	AL_NoNotify	1
#define	AL_Notify	2
#define	AL_FullNotify	3

#define	AL_TCP		SOCK_STREAM
#define	AL_UDP		SOCK_DGRAM

#define	AL_CLTBLSIZ	32		/* table size for clink table */

#define	AL_MAX_CONNATTEMPTS	10

#define	ALsOK		0
#define	ALsLOSE		-1
#define	ALsNOMEM	-2
#define	ALsINVALID	-3
#define	ALsWOULDBLOCK	-20
#define	ALsHEADERREAD	-21
#define	ALsHEADERSHORT	-22
#define	ALsDATAREAD	-23
#define	ALsDEAD		-24
#define	ALsMONDO	-99	/* fatal system error (ie: internal bug) */


#define	AL_NoData	((pALtOpaque *)NULL)
#define	AL_NoHandle	((hALtCLink *)NULL)


typedef int			ALtStatus;

typedef int			ALtBool;
typedef void			ALtOpaque, * pALtOpaque;

typedef struct _al_header	ALtHeader, * pALtHeader;
typedef struct _al_request	ALtRequest, * pALtRequest;
typedef struct _al_rqueue	ALtRQueue, * pALtRQueue;

typedef struct _al_clink	ALtCLink, * pALtCLink, ** ppALtCLink;
typedef int			hALtCLink, * phALtCLink;
typedef struct _al_tcpportal	ALtTCPPortal, * pALtTCPPortal;
typedef struct _al_tcppathway	ALtTCPPathway, * pALtTCPPathway;
typedef struct _al_udpportal	ALtUDPPortal, * pALtUDPPortal;
typedef struct _al_udppathway	ALtUDPPathway, * pALtUDPPathway;
typedef struct _al_cltable	ALtCLTable, * pALtCLTable;


/***--------------------------------------------------------------------***/
/*** Structure definitions: requests, headers, request queue 		***/
/***--------------------------------------------------------------------***/

	/*   Historical note:  the term "request" is a misnomer; it   */
	/* refers to the physical packet of data which passes between */
	/* processes, whether that data is really a request or a      */
	/* response to a request, or neither.			      */


struct _al_header {	/* all word or halfword quantities in net byte order */

  u_char	op0;	/* [1] operation code */
  u_char	op1;	/* [1] sub-operation code */
  u_short	len;	/* [2] length of following data in 4-byte words */
  u_long	seq;	/* [4] sequence number; debugging etc. */

};


struct _al_request {

  ALtHeader		Hdr;		/* request header */
  pALtOpaque		pData;		/* data block, if present */
  hALtCLink		hCL;		/* handle of CLink request came from */
  pALtRequest		pNext;		/* next request, if in link */

};


struct _al_rqueue {

  int			iQLen;		/* # requests in queue */
  pALtRequest		pQueue;		/* head of queue */
  pALtRequest *		ppTail;		/* ptr to tail pointer */

};


/***--------------------------------------------------------------------***/
/*** Structure definitions: clinks, transport-specifics			***/
/***--------------------------------------------------------------------***/

#define	ALclIsPortal		0x01
#define	ALclIsTCP		0x02

#define	ALclUDPPathway		0x00
#define	ALclUDPPortal		0x01
#define	ALclTCPPathway		0x02
#define	ALclTCPPortal		0x03


struct _al_clink {

  int			iType;		/* clink type (above) */
  ALtBool		bAlive;		/* is link alive */
  int			iSoc;		/* socket fd */
  int			iPort;		/* port assigned to socket */
  int			iMode;		/* synch/asynch mode */
  pALtOpaque		pData;		/* application private data */

};


struct _al_tcpportal {

  ALtCLink		CL;

  int			iDefaultPathwayMode;

};


struct _al_tcppathway {

  ALtCLink		CL;

};


struct _al_udpportal {

  ALtCLink		CL;

};


struct _al_udppathway {

  ALtCLink		CL;

};


struct _al_cltable {

  fd_set		All_fdset;
  fd_set		Asynch_fdset;	/* fdset of asynch clinks in list */
  fd_set		Synch_fdset;	/* fdset of synch clinks in list */
  ppALtCLink		pCLs;		/* CLink pointer list */
  int			iCLCount;	/* number of active CLinks */

};


/***--------------------------------------------------------------------***/
/*** ALComm public external declarations				***/
/***--------------------------------------------------------------------***/

extern int	_iALSigSet;		/* sigs blocked during memory calls */
extern int	_iALSigSaveSet;


extern ALtStatus	ALCommInitialise P_(( int iNewCLNotify,
			  int iDeadCLNotify ));

extern ALtStatus	ALInitRQueue P_(( void ));	/* private */
extern ALtStatus	ALInitCLTable P_(( void ));	/* private */
extern void		ALIOHandler P_(( ));		/* private */

extern ALtStatus	ALEnqueueRequest P_(( pALtRequest pReq ));
extern ALtStatus	ALDequeueRequest P_(( pALtRequest * ppReq ));

extern ALtStatus	ALGetHostAddress P_(( ));
extern ALtStatus	ALGetHostnameFromAddress P_(( ));
extern ALtStatus	ALInterpretHostAddress P_(( ));
extern ALtStatus	ALTCPConnectToPort P_(( ));
extern ALtStatus	ALSetupSocket P_(( ));

extern ALtStatus	ALAddCLinkToTable P_(( pALtCLink pNL, int iCLType,
			  int iSoc, int iPort, int iMode, pALtOpaque pPData,
			  hALtCLink * pHandle ));
extern ALtStatus	ALDestroyCLink P_(( hALtCLink hCL ));
extern ALtStatus	ALChangeCLinkMode P_(( hALtCLink hCL, int iMode ));
extern ALtStatus	ALOpenTCPPortal P_(( int iPort, int iMode, int iPMode,
			  pALtOpaque pPData, hALtCLink * pHandle ));
extern ALtStatus	ALOpenTCPPathway P_(( u_long ulAddr, int iPort,
			  int iMode, pALtOpaque pPData, hALtCLink * pHandle ));
extern ALtStatus	ALServiceCLinks P_(( int iMode, int iHow ));
extern ALtStatus	ALServiceTCPPortal P_(( pALtTCPPortal pCL ));
extern ALtStatus	ALServiceTCPPathway P_(( pALtTCPPathway pCL ));
extern ALtStatus	ALTCPGetReqPacket P_(( pALtTCPPathway pCL, int iMode,
			  pALtRequest * ppReq ));
extern ALtStatus	ALTCPGenRequest P_(( hALtCLink hCL, u_char ucOp0,
			  u_char ucOp1, pALtOpaque pData, int iDataLen ));
extern ALtStatus	ALDumpRequest P_(( pALtRequest pReq ));
extern ALtStatus	ALQueueCLinkDeath P_(( pALtCLink pCL ));
extern ALtStatus	ALQueueAcceptedCLink P_(( pALtCLink pCL ));
extern ALtStatus	ALComposeRequest P_(( pALtRequest * ppReq,
			  u_char ucOp0, u_char ucOp1, pALtOpaque pData,
			  int iDataLen ));
extern ALtStatus	ALCLinkToHandle P_(( pCL, phCL ));
extern ALtStatus	ALGetCLPrivData P_(( hCL, ppPData ));
extern ALtStatus	ALSetCLPrivData P_(( hCL, pPData ));

extern void		ALGripe P_(( ));
extern void		ALGripeErr P_(( ));

/*   For obvious reasons, the functions below should really be made inline */
/* functions (where supported), or at least macros.  They are called so    */
/* often I'd hate to leave them real functions!  (this note is duplicated  */
/* in memory.c)								   */

extern char *		ALMalloc P_(( ));
extern int		ALFree P_(( ));
extern char *		ALRealloc P_(( ));
extern char *		ALCalloc P_(( ));

#define	TRYMALLOC( p, ptyp, bsiz )	{				\
  if (((p)=(ptyp)ALMalloc(sizeof(bsiz)))==NULL) 			\
    ALGripe("trymalloc","%s(%d): no memory",__FILE__,__LINE__);		\
}

#define	TRYMALLOCBLK( p, ptyp, bsiz, num )	{			\
  if (((p)=(ptyp)ALMalloc((num)*sizeof(bsiz)))==NULL)			\
    ALGripe("trymallocblk","%s(%d): no memory",__FILE__,__LINE__);	\
}

#define	TRYCALLOC( p, ptyp, bsiz, num )	{				\
  if (((p)=(ptyp)ALCalloc((num),sizeof(bsiz)))==NULL)			\
    ALGripe("trycalloc","%s(%d): no memory",__FILE__,__LINE__);		\
}

#define	TRYREALLOCBLK( op, np, ptyp, bsiz, num ) {			\
  if (((np)=(ptyp)ALRealloc((op),((num)*sizeof(bsiz))))==NULL)		\
    ALGripe("tryreallocblk","%s(%d): no memory",__FILE__,__LINE__);	\
}


#define	ALmProtectedFromSignals		int	_iALSigSaveSet
#define ALmBlockSignals()	{ _iALSigSaveSet=sigblock(_iALSigSet); }
#define	ALmUnblockSignals()	{ (void)sigsetmask(_iALSigSaveSet); }



#define ALmIsCLHandleValid(h)						\
  (((h)>=0)&&((h)<AL_CLTBLSIZ)&&					\
   (_AS.CLT.pCLs[(h)]!=NULL)&&(_AS.CLT.pCLs[(h)]->bAlive==AL_True))

#define	ALmHandleToCLink(h)	(_AS.CLT.pCLs[(h)])




#endif  /* ifndef __ALCOMM_H; Add nothing past this point */
