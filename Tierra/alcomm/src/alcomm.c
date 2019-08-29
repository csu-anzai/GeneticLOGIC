/*
* alcomm.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <fcntl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/uio.h>

#define	_DBGMainModule
#include "alcommp.h"
#undef _DBGMainModule



/*** ALComm library global definitions ***************************************/

int		_iALSigSet;

char		_cScratch[ AL_ScratchStringSize ];

ALtGlobalSet		_AS;


/*
* ALCommInitialise()
*
* params
*
* returns
*   ALsOK	normal initialisation
*   ALsLOSE	already initialised
*/

ALtStatus ALCommInitialise( iNewCLNotify, iDeadCLNotify )
  int		iNewCLNotify;
  int		iDeadCLNotify;
{
  static ALtBool	bInited = AL_False;
  ALtStatus		iStat = ALsLOSE;
  sigset_t		SSet;

  if ( ! bInited ) {

    (void) gethostname( _AS.sHostname, MAXHOSTNAMELEN );
    if ( ALGetHostAddress( _AS.sHostname, &_AS.ulHostAddr ) != ALsOK ) {
      ALGripe( "ALCInit", "can't resolve my own address... help!\n" );
      exit( 1 );
    }

    _AS.iPID = getpid();
    _AS.iDefaultTCPPortalMode = AL_Synch;
    _AS.iDefaultTCPPathwayMode = AL_Synch;
    _AS.iNewCLinkNotify = ( iNewCLNotify == AL_Default )
      			  ? AL_NoNotify : iNewCLNotify;
    _AS.iCLinkDeathNotify = ( iDeadCLNotify == AL_Default )
      			   ? AL_NoNotify : iDeadCLNotify;

    iStat = ALInitRQueue();
    if ( iStat == ALsOK )
      iStat = ALInitCLTable();

    (void) signal( SIGIO, ALIOHandler );
    (void) signal( SIGPIPE, SIG_IGN );

    	/* make sure SIGIO is unblocked, in case we use it */
    (void) sigprocmask( SIG_SETMASK, (sigset_t *)NULL, &SSet );
    (void) sigdelset( &SSet, SIGIO );
    (void) sigprocmask( SIG_SETMASK, &SSet, (sigset_t *)NULL );

    _iALSigSet = sigmask( SIGIO ) | sigmask( SIGPIPE );

  } /* endif ( ! bInited ) */

  return iStat;

} /* end ALCommInitialise() */


/*
* ALInitCLTable()
*
* params
*
* returns
*
*/

ALtStatus ALInitCLTable()
{

  FD_ZERO( &_AS.CLT.All_fdset );
  FD_ZERO( &_AS.CLT.Asynch_fdset );		/* clear fd sets */
  FD_ZERO( &_AS.CLT.Synch_fdset );

  TRYMALLOCBLK( _AS.CLT.pCLs, ppALtCLink, pALtCLink, AL_CLTBLSIZ );
  if ( _AS.CLT.pCLs == NULL )	return ALsNOMEM;

  bzero( _AS.CLT.pCLs, AL_CLTBLSIZ * sizeof( pALtCLink ) );
  _AS.CLT.iCLCount = 0;

  return ALsOK;

} /* end ALInitCLTable() */


/*
* ALIOHandler
*
* params
*
* returns
*
*/

void ALIOHandler( iSig, iCode, SCP, Addr )
  int			iSig;
  int			iCode;
  struct sigcontext *	SCP;
  char *		Addr;
{
  register int		iRet;
  static int		iLevel = 0;

DEBUG( DBGComm )
  ALGripe( "dbg", "[%d]: iohandler ( %d, %d, ... )\n", iLevel, iSig, iCode );
ENDDB()
  
  iLevel++;
  if (( iRet = ALServiceCLinks( AL_Asynch, AL_NoWait )) != ALsOK )
    ALGripe( "ALIOHandler", "ALServiceCLinks returned %d\n", iRet );

  iLevel--;
DEBUG( DBGComm )
  ALGripe( "dbg", "[%d]: out of iohandler.\n", iLevel );
ENDDB()

} /* end ALIOHandler() */


/*
* ALServiceCLinks()
*
* params
*
* returns
*
*/

ALtStatus ALServiceCLinks( iMode, iHow )
  int		iMode;
  int		iHow;
{
  register int		i;
  fd_set		fdsReadMask;
  register int		iRet;
  static struct timeval	tvWait = { 0, 0 };
  struct timeval *	pWait;

  if ( iMode == AL_Synch )
    bcopy( (char *)&_AS.CLT.Synch_fdset, (char *)&fdsReadMask,
	   sizeof( fd_set ) );
  else
    bcopy( (char *)&_AS.CLT.Asynch_fdset, (char *)&fdsReadMask,
	   sizeof( fd_set ) );

  pWait = ( iHow == AL_Wait ) ? (struct timeval *)NULL : &tvWait;

  iRet = select( FD_SETSIZE, &fdsReadMask, (fd_set *)NULL, (fd_set *)NULL,
		 pWait );

  if ( iRet < 0 ) {
    if ( errno != EINTR )
      ALGripeErr( "ALServiceCLinks" );
    return ALsOK;
  }

  if ( iRet == 0 )
    return ALsOK;

DEBUG( DBGComm )
  ALGripe( "dbg", "out of select; returned %d\n", iRet );
ENDDB()

  for ( i = 0; i < AL_CLTBLSIZ; i++ ) {

    if ( _AS.CLT.pCLs[ i ] != NULL &&
	 _AS.CLT.pCLs[ i ]->iMode == iMode &&
	 FD_ISSET( _AS.CLT.pCLs[ i ]->iSoc, &fdsReadMask ) ) {

      switch ( _AS.CLT.pCLs[ i ]->iType ) {

      case ALclTCPPortal:
DEBUG( DBGComm )
        ALGripe( "dbg", "servicing portal (type %d)\n",
		 _AS.CLT.pCLs[i]->iType );
ENDDB()
	(void) ALServiceTCPPortal( (pALtTCPPortal)_AS.CLT.pCLs[ i ] );
	break;

      case ALclTCPPathway:
DEBUG( DBGComm )
        ALGripe( "dbg", "servicing pathway (type %d)\n",
		 _AS.CLT.pCLs[i]->iType );
ENDDB()
	(void) ALServiceTCPPathway( (pALtTCPPathway)_AS.CLT.pCLs[ i ] );
	break;

      default:
	ALGripe( "ALServiceCLinks", "no code for clink type %d\n",
		 _AS.CLT.pCLs[ i ]->iType );
	break;

      } /* end switch */
    } /* endif */
  } /* endfor */

  return ALsOK;

} /* end ALServiceCLinks() */


/*
* ALOpenTCPPortal()
*
* params
*
* returns
*
*/

ALtStatus ALOpenTCPPortal( iPort, iMode, iPMode, pPData, pHandle )
  int		iPort;
  int		iMode;
  int		iPMode;
  pALtOpaque	pPData;
  hALtCLink *	pHandle;
{
  pALtTCPPortal		pNewCL;
  int			iS;
  register int		iRet;

  TRYMALLOC( pNewCL, pALtTCPPortal, ALtTCPPortal );
  if ( pNewCL == NULL )		return ALsNOMEM;

  	/* try setting up socket */

  if ( ALSetupSocket( &iS, AL_TCP, &iPort ) != ALsOK ) {
    /* DAN ALGripe( "ALOpenTCPPortal", "ALSetupSocket failed\n" ); */
    return ALsLOSE;
  } else {
DEBUG( DBGComm )
    ALGripe( "ALOpenTCPPortal", "TCP port assigned: %d\n", iPort );
ENDDB()
  }

  pNewCL->iDefaultPathwayMode = iPMode;

  (void) listen( iS, 5 );

  if ( fcntl( iS, F_SETOWN, _AS.iPID ) < 0 )
    ALGripeErr( "TCPSoc setown" );
  if ( fcntl( iS, F_SETOWN, -_AS.iPID ) < 0 )
    ALGripeErr( "TCPSoc setown group" );

DEBUG( DBGComm )
  ALGripe( "dbg", "making type %d CLink\n", ALclTCPPortal );
ENDDB()
  if ( iMode == AL_Default )
    iMode = _AS.iDefaultTCPPortalMode;
  return ALAddCLinkToTable( (pALtCLink)pNewCL, ALclTCPPortal, iS, iPort,
			    iMode, pPData, pHandle );

} /* end ALOpenTCPPortal() */


/*
* ALOpenTCPPathway()
*
* params
*
* returns
*
*/

ALtStatus ALOpenTCPPathway( ulAddr, iPort, iMode, pPData, pHandle )
  u_long	ulAddr;
  int		iPort;
  int		iMode;
  pALtOpaque	pPData;
  hALtCLink *	pHandle;
{
  pALtTCPPathway	pNewCL;
  int			iS;
  register int		iRet;

  TRYMALLOC( pNewCL, pALtTCPPathway, ALtTCPPathway );
  if ( pNewCL == NULL )		return ALsNOMEM;

  if (( iRet = ALTCPConnectToPort( &iS, ulAddr, iPort )) != ALsOK )
    return iRet;

  if ( iMode == AL_Default )
    iMode = _AS.iDefaultTCPPathwayMode;
  return ALAddCLinkToTable( (pALtCLink)pNewCL, ALclTCPPathway, iS, 0, iMode,
			    pPData, pHandle );

} /* end ALOpenTCPPathway() */


/*
* ALAddCLinkToTable()
*
* params
*
* returns
*
*/

ALtStatus ALAddCLinkToTable( pNL, iCLType, iSoc, iPort, iMode, pPData, pHandle)
  pALtCLink		pNL;
  int			iCLType;
  int			iSoc;
  int			iPort;
  int			iMode;
  pALtOpaque		pPData;
  hALtCLink *		pHandle;
{
  register int		iH;
  register int		iFlags;

  if ( _AS.CLT.iCLCount == AL_CLTBLSIZ )
    return ALsLOSE;

  for ( iH = 0; iH < AL_CLTBLSIZ; iH++ )
    if ( _AS.CLT.pCLs[ iH ] == NULL )
      break;

  if ( iH == AL_CLTBLSIZ ) {				/* sanity check */
    ALGripe( "ALAddCLinkToTable", "iCLCount inconsistent with table\n" );
    return ALsMONDO;
  }

	/* set up clink fields common to all types */
  pNL->iType = iCLType;
  pNL->bAlive = AL_True;
  pNL->iSoc = iSoc;
  pNL->iPort = iPort;
  pNL->iMode = iMode;
  pNL->pData = pPData;

  _AS.CLT.pCLs[ iH ] = pNL;	/* put pointer into table */
  _AS.CLT.iCLCount++;

  if (( iFlags = fcntl( iSoc, F_GETFL, 0 )) < 0 )
    ALGripeErr( "ALAddCLink...,fcntl getfl" );

  FD_SET( iSoc, &_AS.CLT.All_fdset );
  if ( iMode == AL_Asynch ) {
    FD_SET( iSoc, &_AS.CLT.Asynch_fdset );
    iFlags |= FASYNC;
  } else {
    FD_SET( iSoc, &_AS.CLT.Synch_fdset );
    iFlags &= ~FASYNC;
  }
  if ( fcntl( iSoc, F_SETFL, iFlags ) < 0 )
    ALGripeErr( "ALAddCLink...,fcntl setfl" );

  if ( pHandle != NULL )
    *pHandle = iH;

  return ALsOK;

} /* end ALAddCLinkToTable() */


/*
* ALServiceTCPPortal()
*
* params
*
* returns
*
*/

ALtStatus ALServiceTCPPortal( pCL )
  pALtTCPPortal		pCL;
{
  pALtTCPPathway	pNewCL;
  register int		iS;
  register int		iSocFlags;
  register int		iMode;
  register int		iRet;

DEBUG( DBGComm )
  ALGripe( "dbg", "servicing portal\n" );
ENDDB()

  TRYMALLOC( pNewCL, pALtTCPPathway, ALtTCPPathway );
  if ( pNewCL == NULL )		return ALsNOMEM;

  if (( iSocFlags = fcntl( pCL->CL.iSoc, F_GETFL, 0 )) < 0 )
    ALGripeErr( "ALServiceTCPPortal,1fcntl getfl" );
  iSocFlags |= FNDELAY;
  if ( fcntl( pCL->CL.iSoc, F_SETFL, iSocFlags ) < 0 )
    ALGripeErr( "ALServiceTCPPortal,1fcntl setfl" );

  while ( 1 ) {
    if (( iS = accept( pCL->CL.iSoc, (struct sockaddr *)NULL, (int *)NULL )
	) < 0 ) {
      if ( errno != EWOULDBLOCK )
        ALGripeErr( "ALServiceTCPPortal" );
      iRet = ALsOK;
      break;
    } else {
DEBUG( DBGAll )
      ALGripe( "dbg", "hello, we just got socket %d from an accept()\n", iS );
ENDDB()
      iMode = ( pCL->iDefaultPathwayMode == AL_Default )
	      ? _AS.iDefaultTCPPathwayMode
	      : pCL->iDefaultPathwayMode;
      iRet = ALAddCLinkToTable( (pALtCLink)pNewCL, ALclTCPPathway, iS, 0,
			        iMode, AL_NoData, AL_NoHandle );
      if ( _AS.iNewCLinkNotify == AL_Notify )
	(void) ALQueueAcceptedCLink( pNewCL );
    }
  }

  if (( iSocFlags = fcntl( pCL->CL.iSoc, F_GETFL, 0 )) < 0 )
    ALGripeErr( "ALServiceTCPPortal,1fcntl getfl" );
  iSocFlags &= ~FNDELAY;
  if ( fcntl( pCL->CL.iSoc, F_SETFL, iSocFlags ) < 0 )
    ALGripeErr( "ALServiceTCPPortal,1fcntl setfl" );

  return iRet;

} /* end ALServiceTCPPortal() */


/*
* ALQueueAcceptedCLink()
*
* params
*
* returns
*
*/

ALtStatus ALQueueAcceptedCLink( pCL )
  pALtCLink	pCL;
{
  pALtRequest		pReq;

  (void) ALComposeRequest( &pReq, ALrPRIVATE, ALrNewCLink, AL_NoData, 0 );
  (void) ALCLinkToHandle( pCL, &pReq->hCL );
  (void) ALEnqueueRequest( pReq );

  return ALsOK;

} /* end ALQueueAcceptedCLink() */


/*
* ALGetCLPrivData()
*
* params
*
* returns
*
*/

ALtStatus ALGetCLPrivData( hCL, ppPData )
  hALtCLink	hCL;
  pALtOpaque *	ppPData;
{
  register pALtCLink	pCL;

  if ( ! ALmIsCLHandleValid( hCL ) || ppPData == NULL )
    return ALsINVALID;

  pCL = ALmHandleToCLink( hCL );

  *ppPData = pCL->pData;

  return ALsOK;

} /* end ALGetCLPrivData() */


/*
* ALSetCLPrivData()
*
* params
*
* returns
*
*/

ALtStatus ALSetCLPrivData( hCL, pPData )
  hALtCLink	hCL;
  pALtOpaque	pPData;
{
  pALtCLink		pCL;

  if ( ! ALmIsCLHandleValid( hCL ) )
    return ALsINVALID;

  pCL = ALmHandleToCLink( hCL );

  pCL->pData = pPData;

  return ALsOK;

} /* end ALSetCLPrivData() */


/*
* ALServiceTCPPathway()
*
* params
*
* returns
*
*/

ALtStatus ALServiceTCPPathway( pCL )
  pALtTCPPathway	pCL;
{
  register int		iRet;
  pALtRequest		pReq;

DEBUG( DBGAll )
  ALGripe( "  dbg", "now servicing pathway...\n" );
ENDDB()

  while (( iRet = ALTCPGetReqPacket( pCL, AL_NoWait, &pReq )
	 ) != ALsWOULDBLOCK ) {
DEBUG( DBGAll )
    ALGripe( "  dbg", "ALTCPGetReqPacket() returned %d\n", iRet );
ENDDB()
    if ( iRet != ALsOK ) {
      if ( iRet == ALsDEAD )
	return ALsOK;
      else {
DEBUG( DBGComm )
        ALGripe( "DBG", "ALTCPGetReqPacket returned %d\n", iRet );
ENDDB() 
      /* most often should be EINVAL, ECONNRESET, or EINTR */
        return ALsLOSE;
      }
    } else {

DEBUG( DBGAll )
      ALGripe( "  dbg", "enqueuing a request\n" );
ENDDB()
      if (( iRet = ALEnqueueRequest( pReq )) != ALsOK )
	return ALsLOSE;
    }
  }

  return ALsOK;

} /* end ALServiceTCPPathway() */


/*
* ALQueueCLinkDeath()
*
* params
*
* returns
*
*/

ALtStatus ALQueueCLinkDeath( pCL )
  pALtCLink	pCL;
{
  pALtRequest			pReq;
  register pALtCLDeathInfo	pDI;

  TRYMALLOC( pDI, pALtCLDeathInfo, ALtCLDeathInfo );
  if ( pDI == NULL )		return ALsNOMEM;
  pDI->pData = pCL->pData;
  
  (void) ALComposeRequest( &pReq, ALrPRIVATE, ALrDeadCLink, (pALtOpaque)pDI,
			   sizeof( ALtCLDeathInfo ) );
  (void) ALCLinkToHandle( pCL, &pReq->hCL );
  (void) ALEnqueueRequest( pReq );

  return ALsOK;

} /* end ALQueueCLinkDeath() */


/*
* ALCLinkToHandle()
*
* params
*
* returns
*
*/

ALtStatus ALCLinkToHandle( pCL, phCL )
  pALtCLink	pCL;
  hALtCLink *	phCL;
{
  register int		i;

  if ( phCL == NULL )
    return ALsINVALID;

  for ( i = 0; i < AL_CLTBLSIZ; i++ )
    if ( _AS.CLT.pCLs[ i ] == pCL )
      break;

  if ( i == AL_CLTBLSIZ )
    return ALsLOSE;

  *phCL = i;

  return ALsOK;

} /* end ALCLinkToHandle() */


/*
* ALComposeRequest()
*
* params
*
* returns
*
*/

ALtStatus ALComposeRequest( ppReq, ucOp0, ucOp1, pData, iDataLen )
  pALtRequest *		ppReq;
  u_char		ucOp0;
  u_char		ucOp1;
  pALtOpaque		pData;
  int			iDataLen;
{
  register pALtRequest	pReq;

  TRYMALLOC( pReq, pALtRequest, ALtRequest );
  if ( pReq == NULL )		return ALsNOMEM;

  *ppReq = pReq;
  pReq->Hdr.op0 = ucOp0;
  pReq->Hdr.op1 = ucOp1;
  if ( pData == NULL )
    pReq->Hdr.len = 0;
  else
    pReq->Hdr.len = (iDataLen + 3) >> 2;	/* HARDCODED CONSTANTS */
  
  pReq->pData = pData;
  pReq->hCL = -1;
  pReq->pNext = (pALtRequest)NULL;

  return ALsOK;

} /* end ALComposeRequest() */


/*
* ALChangeCLinkMode()
*
* params
*
* returns
*
*/

ALtStatus ALChangeCLinkMode( hCL, iMode )
  hALtCLink	hCL;
  int		iMode;
{
  ALmProtectedFromSignals;
  register int		iFlags;
  register pALtCLink	pCL;
  register int		iRet = ALsOK;

  ALmBlockSignals();

  if ( ! ALmIsCLHandleValid( hCL ) )
    iRet = ALsLOSE;
  else {
    pCL = ALmHandleToCLink( hCL );

    if (( iFlags = fcntl( pCL->iSoc, F_GETFL, 0 )) < 0 ) {
      ALGripeErr( "ALChangeCLinkMode fcntl getfl" );
      iRet = ALsLOSE;
    } else {

      if ( pCL->iMode == iMode )
	return ALsOK;

      switch ( iMode ) {
      case AL_Asynch:
	iFlags |= FASYNC;
	FD_SET( pCL->iSoc, &_AS.CLT.Asynch_fdset );
	FD_CLR( pCL->iSoc, &_AS.CLT.Synch_fdset );
	break;
      case AL_Synch:
	iFlags &= ~FASYNC;
	FD_SET( pCL->iSoc, &_AS.CLT.Synch_fdset );
	FD_CLR( pCL->iSoc, &_AS.CLT.Asynch_fdset );
	break;
      }

      pCL->iMode = iMode;
      if ( fcntl( pCL->iSoc, F_SETFL, iFlags ) < 0 ) {
	ALGripeErr( "ALChangeCLinkMode fcntl setfl" );
	iRet = ALsLOSE;
      }
    }
  }

  ALmUnblockSignals();

  return iRet;

} /* end ALChangeCLinkMode() */


/*
* ALTCPGetReqPacket()
*
* params
*
* returns
*
*/

ALtStatus ALTCPGetReqPacket( pCL, iMode, ppReq )
  pALtTCPPathway	pCL;
  int			iMode;
  pALtRequest *		ppReq;
{
  register pALtRequest	pReq;
  register int		iSocFlags;
  char *		pData;
  register int		iLen;
  register int		iDataLen;
  register int		iTotal;
  register int		iRet;
  int			hCL;

  if ( pCL == NULL || ppReq == NULL )
    return ALsINVALID;

  if ( pCL->CL.bAlive == AL_False )
    return ALsDEAD;

  TRYMALLOC( pReq, pALtRequest, ALtRequest );
  if ( pReq == NULL )		return ALsNOMEM;

  (void) ALCLinkToHandle( pCL, &hCL );
  pReq->hCL = hCL;

DEBUG( DBGAll )
  ALGripe( "  dbg", "note socket in getreqpacket is %d\n", pCL->CL.iSoc );
ENDDB()

  if ( iMode == AL_NoWait ) {
    if (( iSocFlags = fcntl( pCL->CL.iSoc, F_GETFL, 0 )) < 0 )
      ALGripeErr( "ALTCPGetReqPacket,1fcntl getfl" );
    iSocFlags |= FNDELAY;
    if ( fcntl( pCL->CL.iSoc, F_SETFL, iSocFlags ) < 0 )
      ALGripeErr( "ALTCPGetReqPacket,1fcntl setfl" );
DEBUG( DBGAll )
    ALGripe( "  dbg", "in GetReqPacket, setting NoWait stuff\n" );
ENDDB()
  }

  iLen = read( pCL->CL.iSoc, (char *)&pReq->Hdr, sizeof( ALtHeader ) );

DEBUG( DBGAll )
  ALGripe( "  dbg", "(sanity check; read() returned %d)\n", iLen );
ENDDB()

  if ( iMode == AL_NoWait ) {
    if (( iSocFlags = fcntl( pCL->CL.iSoc, F_GETFL, 0 )) < 0 )
      ALGripeErr( "ALTCPGetReqPacket,2fcntl getfl" );
    iSocFlags &= ~FNDELAY;
    if ( fcntl( pCL->CL.iSoc, F_SETFL, iSocFlags ) < 0 )
      ALGripeErr( "ALTCPGetReqPacket,2fcntl setfl" );
  }

  if ( iLen < 0 ) {

    if ( errno == ECONNRESET )
      iLen = 0;
    else {

      (void) ALFree( (char *)pReq );
DEBUG( DBGComm )
      ALGripe( "DBG", "iLen on read returned error %d\n", errno );
ENDDB()
      return ( errno == EWOULDBLOCK ) ? ALsWOULDBLOCK : ALsHEADERREAD;

    }
  }

  if ( iLen == 0 ) {	/* zero read on header or reset connection */
    			/* indicates death of clink */

    (void) ALFree( (char *)pReq );
    pCL->CL.bAlive = AL_False;

DEBUG( DBGAll )
    ALGripe( "info", "dead link in ALTCPGetReqPacket\n" );
ENDDB()

    if ( _AS.iCLinkDeathNotify != AL_FullNotify ) {
      (void) ALFree( (char *)pCL->CL.pData );
      pCL->CL.pData = (pALtOpaque)NULL;
    }
    if ( _AS.iCLinkDeathNotify != AL_NoNotify )
      (void) ALQueueCLinkDeath( pCL );
    if (( iRet = ALDestroyCLink( hCL )) != ALsOK )
      ALGripe( "ALTCPGetReqPacket", "ALDestroyCLink returned %d\n", iRet );

    return ALsDEAD;
  }

  if ( iLen < sizeof( ALtHeader ) )
    return ALsHEADERSHORT;

DEBUG( DBGAll )
  ALGripe( "  dbg", "wow, we have a header with data following!\n" );
  ALGripe( "  dbg", "header says there's %d words following.\n",
	   ntohs( pReq->Hdr.len ) );
ENDDB()

  pReq->Hdr.len = ntohs( pReq->Hdr.len );

  if ( pReq->Hdr.len > 0 ) {	/* there is data following */

    iDataLen = pReq->Hdr.len << 2;		/* HARDCODED CONSTANT */
    
    TRYMALLOCBLK( pReq->pData, pALtOpaque, long, pReq->Hdr.len );
    if ( pReq->pData == NULL ) {
      (void) ALFree( pReq );
      return ALsNOMEM;
    }

    iTotal = 0;
    pData = (char *)pReq->pData;
    do {

      if (( iLen = read( pCL->CL.iSoc, &pData[ iTotal ], iDataLen - iTotal )
	  ) < 0 ) {
	(void) ALFree( pData );
	pReq->pData = (pALtOpaque)NULL;
	return ALsDATAREAD;
      }
      
      if ( iLen == 0 )
	break;
      else
	iTotal += iLen;

    } while ( iTotal < iDataLen );

  } else
    pReq->pData = (pALtOpaque)NULL;

  *ppReq = pReq;

  return ALsOK;

} /* end ALTCPGetReqPacket() */


/*
* ALTCPGenRequest()
*
* params
*
* returns
*
*/

ALtStatus ALTCPGenRequest( hCL, ucOp0, ucOp1, pData, iDataLen )
  hALtCLink	hCL;
  u_char	ucOp0;
  u_char	ucOp1;
  pALtOpaque	pData;
  int		iDataLen;
{
  ALmProtectedFromSignals;
  register pALtTCPPathway	pCL;
  register int			iRet = ALsOK;
  ALtHeader			tmpHdr;
  struct iovec			iovData[2];
  int				iVec;
  static u_long			ulWritePad = 0L;		

  if ( ! ALmIsCLHandleValid( hCL ) )
    return ALsLOSE;

  ALmBlockSignals();

  pCL = (pALtTCPPathway)ALmHandleToCLink( hCL );

  tmpHdr.op0 = ucOp0;
  tmpHdr.op1 = ucOp1;
  tmpHdr.seq = 0;	/* seq unused with TCP */
  if ( pData == NULL )
    tmpHdr.len = 0;
  else
    tmpHdr.len = htons( (iDataLen+3) >> 2 );	/* HARDCODED CONSTANT */

  if ( write( pCL->CL.iSoc, (char *)&tmpHdr, sizeof( ALtHeader )) < 0 ) {
    if ( errno != EPIPE ) {
      close( pCL->CL.iSoc );
      pCL->CL.bAlive = AL_False;
      ALGripeErr( "ALTCPGenRequest" );
      iRet = ALsLOSE;
    }
  }

  if ( iRet == ALsOK && iDataLen > 0 ) {

    iovData[0].iov_base = (caddr_t)pData;
    iovData[0].iov_len = iDataLen;

    	/*   If we've been passed a block of data to send and the size of */
	/* that block isn't a word multiple, then we pad the end of the   */
	/* transmitted data with NULLs.	 The writev() allows this while   */
	/* insuring that a peer read() will atomically read both data and */
	/* padding (if any).						  */

    if ( iDataLen % 4 == 0 )
      iVec = 1;
    else {
      iVec = 2;
      iovData[1].iov_base = (caddr_t)&ulWritePad;
      iovData[1].iov_len = 4 - (iDataLen % 4);
    }

    if ( writev( pCL->CL.iSoc, iovData, iVec ) < 0 ) {
      if ( errno != EPIPE ) {
	close( pCL->CL.iSoc );
	pCL->CL.bAlive = AL_False;
	ALGripeErr( "ALTCPGenRequestAndReply" );
	iRet = ALsLOSE;
      }
    }
  } /* endif (data to send) */

  ALmUnblockSignals();

  return iRet;

} /* end ALTCPGenRequest() */


/*
* ALDumpRequest()
*
* params
*
* returns
*
*/

ALtStatus ALDumpRequest( pReq )
  pALtRequest	pReq;
{
  register int		len, i, j, lines, el;

  printf( "R:" );
  if ( pReq == NULL ) {
    puts( "<null>" );
    return ALsOK;
  }
  printf( "[%02x:%02x]", pReq->Hdr.op0, pReq->Hdr.op1 );
  if ( pReq->Hdr.len == 0 ) {
    printf( "\n" );
    return ALsOK;
  }
  len = pReq->Hdr.len << 2;
  lines = (len-1) >> 4;
  el = 0;
  for ( j = 0; j <= lines; j++ ) {
    if ( j == 0 )
      printf( "( " );
    else
      printf( "         ( " );
    for ( i = 0; i < 16; i++ ) {
      if ( (el+i) < len )
	printf( "%02x ", (u_char)((char *)pReq->pData)[ el + i ] );
      else
	printf( "-- " );
    }
    printf( ")[" );
    for ( i = 0; i < 16; i++ ) {
      if ( (el+i) < len ) {
	if ( isprint( ((char *)pReq->pData)[ el+i ] ) )
	  putchar( ((char *)pReq->pData)[ el+i ] );
	else
	  putchar( '.' );
      } else
	putchar( '-' );
    }
    printf( "]\n" );
    el += 16;
  }

  return ALsOK;

} /* end ALDumpRequest() */


/*
* ALDestroyRequest()
*
* params
*
* returns
*
*/

ALtStatus ALDestroyRequest( pReq )
  pALtRequest	pReq;
{

  if ( pReq == NULL )
    return ALsINVALID;

  (void) ALFree( (char *)pReq->pData );
  (void) ALFree( (char *)pReq );

  return ALsOK;

} /* end ALDestroyRequest() */


/*
* ALDestroyCLink()
*
* params
*
* returns
*
*/

ALtStatus ALDestroyCLink( hCL )
  hALtCLink	hCL;
{
  ALmProtectedFromSignals;
  register pALtCLink	pCL;
  register int		iSocFlags;

	/* can't use ALmIsCLHandleValid, 'cause it checks for bAlive */
  if ( hCL < 0 || hCL > AL_CLTBLSIZ || _AS.CLT.pCLs[ hCL ] == NULL )
    return ALsLOSE;

  ALmBlockSignals();

  pCL = ALmHandleToCLink( hCL );

  FD_CLR( pCL->iSoc, &_AS.CLT.All_fdset );
  switch ( pCL->iMode ) {
  case AL_Asynch:
    FD_CLR( pCL->iSoc, &_AS.CLT.Asynch_fdset );
    iSocFlags = fcntl( pCL->iSoc, F_GETFL, 0 ) & ~FASYNC;
    (void) fcntl( pCL->iSoc, F_SETFL, iSocFlags );
    break;
  case AL_Synch:
    FD_CLR( pCL->iSoc, &_AS.CLT.Synch_fdset );
    break;
  }

  (void) shutdown( pCL->iSoc, 2 );
  close( pCL->iSoc );

  ALFree( (char *)pCL );
  _AS.CLT.iCLCount--;
  _AS.CLT.pCLs[ hCL ] = (pALtCLink)NULL;

  ALmUnblockSignals();

  return ALsOK;

} /* end ALDestroyCLink() */


ALtStatus ALGetLinkFD( hCL, pFD )
  hALtCLink	hCL;
  int *		pFD;
{
  ALmProtectedFromSignals;
  pALtCLink		pCL;

  if ( hCL < 0 || hCL > AL_CLTBLSIZ || _AS.CLT.pCLs[ hCL ] == NULL )
    return ALsLOSE;

  ALmBlockSignals();

  pCL = ALmHandleToCLink( hCL );

  *pFD = pCL->iSoc;

  ALmUnblockSignals();

  return ALsOK;

} /* end ALGetLinkFD() */


/* end alcomm.c */
