/*
* mlayer.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
* Many thanks to the Chick Corea Elektric Band, Front 242, and Steve Vai
* for producing such incredibly wonderful music.  Go check 'em out if you
* haven't already.  I *love* Vai's "Erotic Nightmares", and also "I Would
* Love To" (both from the _Passion and Warfare_ album).
*
*/

#include "mlayerp.h"


/*** MLayer library global definitions **************************************/

MtGlobalSet		_MS;
int *			_MSDFTblCnt;


void		MHandleMessage P_(( ));
void		MHandleQuery P_(( ));
void		MHandleDataflow P_(( ));
void		MDataflowControl P_(( ));
void		MDiddleDataflow P_(( ));



MtStatus MInitialise( iDefHiWM, iDefLoWM, pDefMRoutines, iMRCount,
		      pDefQRoutines, iQRCount, pDefDRoutines, iDRCount,
		      pDefDIRoutines, iDIRCount )
  int			iDefHiWM;
  int			iDefLoWM;
  pMtDefaultRoutines	pDefMRoutines;
  int			iMRCount;
  pMtDefaultRoutines	pDefQRoutines;
  int			iQRCount;
  pMtDefaultRoutines	pDefDRoutines;
  int			iDRCount;
  pMtDefaultRoutines	pDefDIRoutines;
  int			iDIRCount;
{
  int			iRet;
  int			i;

DEBUG( DBGAll )
  ALGripe( "info", "initialising ALlayer\n" );
ENDDB()
  if (( iRet = ALCommInitialise( AL_Notify, AL_FullNotify )) < 0 )
/*DAN    ALGripe( "ALCommInitialise", "error %d\n", iRet ), exit( -1 ); */
{    ALGripe( "ALCommInitialise", "error %d\n", iRet ); return MsLOSE;}

  _MS.bReplyFlag = M_False;

  TRYCALLOC( _MS.LIT.pLIs, ppMtLinkInfo, pMtLinkInfo, M_LITBLSIZ );
  if ( _MS.LIT.pLIs == NULL )		return MsNOMEM;
  _MS.LIT.iLICount = 0;
  _MS.LIT.iCurLink = -1;

  TRYCALLOC( _MS.MRT, pMfnRoutine, MfnRoutine, M_MRTBLSIZ );
  if ( _MS.MRT == NULL )	return MsNOMEM;
  TRYCALLOC( _MS.QRT, pMfnRoutine, MfnRoutine, M_QRTBLSIZ );
  if ( _MS.QRT == NULL )	return MsNOMEM;
  TRYCALLOC( _MS.DRT, pMfnRoutine, MfnRoutine, M_DRTBLSIZ );
  if ( _MS.DRT == NULL )	return MsNOMEM;
  TRYCALLOC( _MS.DIRT, pMfnRoutine, MfnRoutine, M_OUTDRTBLSIZ );
  if ( _MS.DIRT == NULL )	return MsNOMEM;
      /* ^^^^^^^^---> divorced phone-sex persona */

  TRYCALLOC( _MS.DFTable, phMtLinkInfo *, phMtLinkInfo, M_OUTDRTBLSIZ );
  if ( _MS.DFTable == NULL )		return MsNOMEM;
  TRYCALLOC( _MS.DFTEntryCount, int *, int, M_OUTDRTBLSIZ );
  if ( _MS.DFTEntryCount == NULL )	return MsNOMEM;
  _MSDFTblCnt = _MS.DFTEntryCount;	/* for MIsDFEnabled macro; mlayer.h */

	/* install default message routines */

  for ( i = 0; i < iMRCount; i++ ) {
    if ( pDefMRoutines[i].ucWhich > M_MRTBLSIZ )
      ALGripe( "MInitialise", "Message routine #%d has code (%d) larger than max (%d).\n", i, pDefMRoutines[i].ucWhich, M_MRTBLSIZ );
    else
      _MS.MRT[ pDefMRoutines[i].ucWhich ] = pDefMRoutines[i].fnRoutine;
  }

	/* install default query routines */

  for ( i = 0; i < iQRCount; i++ ) {
    if ( pDefQRoutines[i].ucWhich > M_QRTBLSIZ )
      ALGripe( "MInitialise", "Query routine #%d has code (%d) larger than max (%d).\n", i, pDefQRoutines[i].ucWhich, M_QRTBLSIZ );
    else
      _MS.QRT[ pDefQRoutines[i].ucWhich ] = pDefQRoutines[i].fnRoutine;
  }

	/* install default dataflow routines */

  for ( i = 0; i < iDRCount; i++ ) {
    if ( pDefDRoutines[i].ucWhich > M_DRTBLSIZ )
      ALGripe( "MInitialise", "Dataflow routine #%d has code (%d) larger than max (%d).\n", i, pDefDRoutines[i].ucWhich, M_DRTBLSIZ );
    else
      _MS.DRT[ pDefDRoutines[i].ucWhich ] = pDefDRoutines[i].fnRoutine;
  }

	/* install default dataflow initialisation routines */
 
  for ( i = 0; i < iDIRCount; i++ ) {
    if ( pDefDIRoutines[i].ucWhich > M_OUTDRTBLSIZ )
      ALGripe( "MInitialise", "Data init routine #%d has code (%d) larger than max (%d).\n", i, pDefDIRoutines[i].ucWhich, M_OUTDRTBLSIZ );
    else
      _MS.DIRT[ pDefDIRoutines[i].ucWhich ] = pDefDIRoutines[i].fnRoutine;
  }
      
  if ( iDefHiWM < 1 ) {		/* make flowcontrol disabled */
    _MS.iDefaultHiWM = -1;
    _MS.iDefaultLoWM = -1;
  } else {

    _MS.iDefaultHiWM = iDefHiWM;

    if ( iDefLoWM < 0 )
      _MS.iDefaultLoWM = 0;
    else if ( iDefLoWM >= iDefHiWM )
      _MS.iDefaultLoWM = iDefHiWM - 1;
    else
      _MS.iDefaultLoWM = iDefLoWM;
  }

  return MsOK;

} /* end MInitialise() */


MtStatus MOpenPublicPort( iPort )
  int		iPort;
{
  int			iRet;
  hALtCLink		hPortal;

  if ( iPort >= 0 ) {
DEBUG( DBGAll )
  ALGripe( "info", "opening TCP portal\n" );
ENDDB()
    if ((iRet = ALOpenTCPPortal( iPort, AL_Asynch, AL_Asynch,
				 (pALtOpaque)NULL, &hPortal )) < 0)
/* DAN      ALGripe( "ALOpenTCPPortal", "error %d\n", iRet );  */
    return MsLOSE;
  } else {
DEBUG( DBGAll )
    ALGripe( "info", "no TCP portal created\n" );
ENDDB()
  }

  return MsOK;

} /* end MOpenPublicPort() */


MtStatus MConnectTo( sHost, iPort, phLink )
  char *	sHost;
  int		iPort;
  hMtLinkInfo *	phLink;
{
  pMtLinkInfo		pNewLink;
  char *		sHostname;
  u_long		ulAddr;
  int			iRet;
  hALtCLink		hCL;

DEBUG( DBGAll )
  ALGripe( "info", "resolving host\n" );
ENDDB()
  if (( iRet = ALInterpretHostAddress( sHost, &sHostname, &ulAddr )
      ) != ALsOK ) {
    ALGripe( "AL call", "ALInterpretHostAddress returned %d\n", iRet );
    return ALsLOSE;
  }

DEBUG( DBGAll )
  ALGripe( "info", "trying connect to %s\n", sHostname );
ENDDB()
  
  if (( iRet = ALOpenTCPPathway( ulAddr, iPort, AL_Default, AL_NoData, &hCL )
      ) != ALsOK )
    {ALGripe( "AL open call", "open tcp pathway returned %d\n", iRet );
/* DAN    exit( -1 ); */
    return MsLOSE; }

  if (( iRet = MSetupNewLinkInfo( hCL, &pNewLink, phLink )) != MsOK )
/* DAN     ALGripe( "MSetupNewLinkInfo", "returned %d\n", iRet ), exit( -1 );*/
    {ALGripe( "MSetupNewLinkInfo", "returned %d\n", iRet ); return MsLOSE;}

  ALSetCLPrivData( hCL, (pALtOpaque)pNewLink );

  return MsOK;

} /* end MConnectTo() */


MtStatus MSetupNewLinkInfo( hCL, ppNewLI, phNewLI )
  hALtCLink	hCL;
  ppMtLinkInfo	ppNewLI;
  phMtLinkInfo	phNewLI;
{
  pMtLinkInfo		pLI;
  int			iRet;

  TRYMALLOC( pLI, pMtLinkInfo, MtLinkInfo );
  if ( pLI == NULL )	return MsNOMEM;

  if ( ppNewLI != NULL )
    *ppNewLI = pLI;

  pLI->hLink = hCL;
  pLI->iHiWM = _MS.iDefaultHiWM;
  pLI->iLoWM = _MS.iDefaultLoWM;
  pLI->iAckWait = M_False;
  pLI->iLevel = 0;
  pLI->iOutWin = -1;		/* outgoing flow control disabled */
  pLI->RB.bValid = M_False;	/* no request reply buffered */

  if (( iRet = MAddLinkToTable( pLI, phNewLI )) != MsOK )
    {ALGripe( "MAddLinkToTable", "returned %d\n", iRet );
/* DAN      exit( -1 ); */
      return MsLOSE;}

  return MsOK;

} /* end MSetupNewLinkInfo() */


MtStatus MAddLinkToTable( pNL, pHandle )
  pMtLinkInfo	pNL;
  phMtLinkInfo	pHandle;
{
  register int		iH;

  if ( _MS.LIT.iLICount == M_LITBLSIZ )
    return MsLOSE;

  for ( iH = 0; iH < M_LITBLSIZ; iH++ )
    if ( _MS.LIT.pLIs[ iH ] == NULL )
      break;

  if ( iH == M_LITBLSIZ ) {				/* sanity check */
    ALGripe( "MAddLinkToTable", "iLICount inconsistent with table\n" );
    return MsLOSE;
  }

  _MS.LIT.pLIs[ iH ] = pNL;	/* put pointer into table */

  if ( pHandle != NULL )
    *pHandle = iH;

  _MS.LIT.iLICount++;

DEBUG( DBGAll )
  ALGripe( "info", "added LI %d to table\n", iH );
ENDDB()

  return MsOK;

} /* end MAddLinkToTable() */


MtStatus MServiceRequests( iHow )
  int		iHow;
{
  int			iRet;
  int			iALHow;
  pALtRequest		pReq;
  pMtLinkInfo		pLI;

  iALHow = ( iHow == M_Wait ) ? AL_Wait : AL_NoWait;

  if (( iRet = ALServiceCLinks( AL_Synch, iALHow )) != ALsOK )
/* DAN    ALGripe( "ALServiceCLinks", "error %d\n", iRet ), exit( -1 ); */
    {ALGripe( "ALServiceCLinks", "error %d\n", iRet ); return MsLOSE;}

  pReq = (pALtRequest)NULL;
  iRet = ALDequeueRequest( &pReq );

  while ( pReq != NULL ) {
DEBUG( DBGAll )
    (void) ALDumpRequest( pReq );
ENDDB()

    if ( pReq->Hdr.op0 == ALrPRIVATE )

      MProcessALPrivateRequest( pReq );

    else {	/* request is not a low-level private request */

      if (( iRet = ALGetCLPrivData( pReq->hCL, (pALtOpaque)&pLI )
	  ) == ALsOK ) {

	/* check flowcontrol information */

	if ( pLI->iHiWM >= 0 && pLI->iAckWait == M_False ) {

	  pLI->iLevel--;

DEBUG( DBGAll )
          ALGripe( "info", "retrieved a new packet; now LI level = (%d)\n",
		   pLI->iLevel );
ENDDB()
	  if ( pLI->iLevel <= pLI->iLoWM ) {	/* is low watermk reached? */
	    MtSendWindow	SW;
	    hMtLinkInfo	hLI;

	    if ( pLI->iLevel < pLI->iLoWM )		/* sanity check */
	      ALGripe( "MServiceRequests", "internal bug; req level dropped below low watermark\n" );

	    (void) MLinkInfoToHandle( pLI, &hLI );
	    SW.iUpdate = htonl( pLI->iHiWM - pLI->iLoWM );
DEBUG( DBGAll )
  	    ALGripe( "info", "level is to low watermark; updating peer.\n" );
ENDDB()
	    if (( iRet = MGenRequest( hLI, MrPRIVATE, MrtUpdateSendWin,
				      (pMtOpaque)&SW, sizeof( SW ) )) != MsOK )
	      ALGripe( "MServiceRequests", "MGenRequest isn't happy (%d)\n",
		       iRet );
	    pLI->iLevel = pLI->iHiWM;

	  } /* end if (low watermark reached) */

	} /* end if (flowcontrol active) */

      } /* end if (clink is valid) */

      switch ( pReq->Hdr.op0 ) {

      case MrPRIVATE:
	MProcessMPrivateRequest( pReq, pLI );
	break;

      default:
	MProcessRequest( pReq, pLI );
	break;

      } /* end case */

    } /* end else (process non-al-private request) */

    (void) ALDestroyRequest( pReq );
    pReq = (pALtRequest)NULL;
    iRet = ALDequeueRequest( &pReq );
  }

  return MsOK;

} /* end MServiceRequests() */


MtStatus MGenRequest( hLI, op0, op1, pData, iDataLen )
  hMtLinkInfo	hLI;
  u_char	op0;
  u_char	op1;
  pMtOpaque	pData;
  int		iDataLen;
{
  int			iRet;
  pMtLinkInfo		pLI;

  if ( ! MmIsLIHandleValid( hLI ) )
    return MsLOSE;

  pLI = MmHandleToLinkInfo( hLI );

  	/* if outgoing flow control is disabled, iOutWin will be -1 */

  while ( pLI->iOutWin == 0 ) {	/* if window is zero (and thus outgoing */
    				/*   f/c is enabled), wait for it to open */

DEBUG( DBGAll )
    ALGripe( "info", "outgoing send window is closed for LI (%d). waiting...\n", hLI );
ENDDB()

    if (( iRet = MServiceRequests( M_Wait )) != MsOK ) {
      ALGripe( "MGenRequest", "MServiceRequests failed while waiting for send window\n" );
      return MsLOSE;
    }

	/* at this point we must verify that LI handle references a valid */
	/*   LI structure, ie: that the link didn't die on us during the */
	/*   MServiceRequests wait. */

  if (( pLI = MmHandleToLinkInfo( hLI )) == NULL )
    return MsDEADLINK;		/* yep, link died on us.  inform caller */

DEBUG( DBGAll )
    if ( pLI->iOutWin == -1 )
      ALGripe( "info", "hello, outgoing f/c has just been disabled\n" );
    else if ( pLI->iOutWin > 0 )
      ALGripe( "info", "okay, outgoing send window has jumped to (%d)\n",
	       pLI->iOutWin );
ENDDB()

  } /* end while (f/c enabled & window closed) */

  if ( pLI->iOutWin > 0 )	/* if f/c is not disabled, */
    pLI->iOutWin--;		/*   record the transmission of this packet */

DEBUG( DBGAll )
  ALGripe( "info", "...outgoing packet sent; now outwin == %d\n",
	   pLI->iOutWin );
ENDDB()
  if (( iRet = ALTCPGenRequest( pLI->hLink, op0, op1, (pALtOpaque)pData,
			        iDataLen )) != ALsOK ) {

    if ( iRet == ALsDEAD )
      return MsDEADLINK;	/* inform caller */
    else if ( iRet != ALsLOSE )
/* DAN      ALGripe( "ALTCPGenRequest", "error %d\n", iRet ), exit( -1 );*/
      {ALGripe( "ALTCPGenRequest", "error %d\n", iRet ); return MsLOSE;}

  }

  return MsOK;

} /* end MGenRequest */


MtStatus MGenRequestWithReply( hLI, op0, op1, pData, iDataLen, ppReplyData,
			       piReplyDataLen )
  hMtLinkInfo	hLI;
  u_char	op0;
  u_char	op1;
  pMtOpaque	pData;
  int		iDataLen;
  pMtOpaque *	ppReplyData;
  int *		piReplyDataLen;
{
  int			iRet;
  pMtLinkInfo		pLI;
  pALtRequest		pReq;
  
  if (( iRet = MGenRequest( hLI, op0, op1, pData, iDataLen ) ) != MsOK )
    return iRet;

  pLI = MmHandleToLinkInfo( hLI );

  pLI->RB.bValid = M_False;	/* mark this LI's reply buffer invalid */

DEBUG( DBGAll )
  ALGripe( "info", "just made a query; waiting for reply...\n" );
ENDDB()

  while ( pLI->RB.bValid == M_False ) {

    if (( iRet = MServiceRequests( M_Wait )) != MsOK ) {
      ALGripe( "MGenRequestWithReply", "MServiceRequests failed (%d)\n",
	       iRet );
      return MsLOSE;
    }
DEBUG( DBGAll )
    ALGripe( "info", "(inside reply waiting loop)\n" );
ENDDB()

  	/* at this point we must verify that LI handle references a valid */
	/*   LI structure, ie: that the link didn't die on us during the */
	/*   MServiceRequests wait.  Same check is done during a closed */
	/*   output flow window inside of MGenRequest(). */

    if (( pLI = MmHandleToLinkInfo( hLI )) == NULL ) {
      *ppReplyData = (pMtOpaque)NULL;	/* help idiot-proof this function */
      *piReplyDataLen = 0;
      return MsDEADLINK;	/* yep, link died on us.  inform caller */
    }

  } /* end while (reply not gotten) */

  *ppReplyData = pLI->RB.pData;
  *piReplyDataLen = pLI->RB.iDataLen;

DEBUG( DBGAll )
  ALGripe( "info", "got the desired reply.\n" );
ENDDB()

  return MsOK;

} /* end MGenRequestWithReply() */


void MProcessALPrivateRequest( pReq )
  pALtRequest	pReq;
{
  pMtLinkInfo		pLI;
  MtStatus		iRet;

  switch ( pReq->Hdr.op1 ) {

  case ALrDeadCLink:
    MCleanupDeadLink( (pALtCLDeathInfo)pReq->pData );
    break;

  case ALrNewCLink:
    if (( iRet = MSetupNewLinkInfo( pReq->hCL, &pLI, (phMtLinkInfo)NULL )
	) != MsOK )
      ALGripe( "MProcALPrivReq.", "MSetupNewLinkInfo failed (%d)\n", iRet );
    ALSetCLPrivData( pReq->hCL, (pALtOpaque)pLI );
    break;

  default:
    ALGripe( "MProcessALPrivateRequest", "unknown AL request %d\n",
	     pReq->Hdr.op1 );
    break;

  }

} /* end MProcessALPrivateRequest() */


void MProcessMPrivateRequest( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{
  pMtSendWindow		pSW;
  hMtLinkInfo		hLI;
  MtStatus		iRet;

  switch ( pReq->Hdr.op1 ) {

  case MrtSetSendWin:
    pSW = (pMtSendWindow)pReq->pData;
    pSW->iUpdate = ntohl( pSW->iUpdate );
    pLI->iOutWin = pSW->iUpdate;
    (void) MLinkInfoToHandle( pLI, &hLI );
    if ( pSW->iUpdate != -1 ) {
      if (( iRet = MGenRequest( hLI, MrPRIVATE, MrtAckSetSendWin, M_NoData, 0 )
	  ) != MsOK )
	ALGripe( "MProcMPrivReq.", "MGenRequest isn't happy (%d)\n", iRet );
    }
DEBUG( DBGAll )
    ALGripe( "info", "remote set our send window to (%d)\n", pSW->iUpdate );
    if ( pSW->iUpdate > -1 )
      ALGripe( "info", "...sent an acknowledgement\n" );
ENDDB()
    break;

  case MrtAckSetSendWin:
    if ( pLI->iAckWait != M_True )	/* sanity check */
      ALGripe( "MProcMPrivReq.", "iAckWait isn't set to true; hmm...\n" );
    pLI->iAckWait = M_False;	/* reset the wait state */
DEBUG( DBGAll )
    ALGripe( "info", "got ack of send-window setting\n" );
ENDDB()
    break;

  case MrtUpdateSendWin:
    pSW = (pMtSendWindow)pReq->pData;
    pSW->iUpdate = ntohl( pSW->iUpdate );
    pLI->iOutWin += pSW->iUpdate;
DEBUG( DBGAll )
    ALGripe( "info", "send win just updated by (%d) to total of (%d)\n",
	     pSW->iUpdate, pLI->iOutWin );
ENDDB()
    break;
    
  default:
    ALGripe( "MProcessMPrivateRequest", "unknown M request %d\n",
	     pReq->Hdr.op1 );
    break;

  }

} /* end MProcessMPrivateRequest() */


void MProcessRequest( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{

  switch ( pReq->Hdr.op0 ) {

  case MrMessage:
    MHandleMessage( pReq, pLI );
    break;

  case MrQuery:
    MHandleQuery( pReq, pLI );
    break;

  case MrQueryReply:
    pLI->RB.ucOp = pReq->Hdr.op1;
    pLI->RB.pData = pReq->pData;
    pLI->RB.iDataLen = pReq->Hdr.len << 2;
    pLI->RB.bValid = M_True;
    break;

  case MrQueryError:
    pLI->RB.ucOp = pReq->Hdr.op1;
    pLI->RB.pData = (pMtOpaque)NULL;
    pLI->RB.iDataLen = 0;
    pLI->RB.bValid = M_True;
    break;

  case MrModifyDataflow:
    MDataflowControl( pReq, pLI );
    break;

  case MrDataflowEvent:
    MHandleDataflow( pReq, pLI );
    break;

  default:
    ALGripe( "MProcessRequest", "unknown M request, op0 = %d\n",
	     pReq->Hdr.op0 );
    break;

  }

} /* end MProcessRequest() */


void MHandleMessage( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{
  register u_char	ucM;
  hMtLinkInfo		hLI;

  ucM = pReq->Hdr.op1;
DEBUG( DBGAll )
  ALGripe( "info", "handling message (%d)\n", ucM );
ENDDB()

  (void) MLinkInfoToHandle( pLI, &hLI );

  if ( ucM > M_MRTBLSIZ )
    ALGripe( "MHandleMessage", "message (%d) outside max (%d)\n", ucM,
	     M_MRTBLSIZ );
  else if ( _MS.MRT[ ucM ] == NULL )
    ALGripe( "MHandleMessage", "no message routine registered for (%d)\n",
	     ucM );
  else
    ( _MS.MRT[ ucM ] )( ucM, pReq->pData, hLI );

} /* end MHandleMessage() */


void MHandleQuery( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{
  register u_char	ucQ;
  hMtLinkInfo		hLI;

  ucQ = pReq->Hdr.op1;
DEBUG( DBGAll )
  ALGripe( "info", "handling query (%d)\n", ucQ );
ENDDB()

  (void) MLinkInfoToHandle( pLI, &hLI );

  if ( ucQ > M_QRTBLSIZ )
    ALGripe( "MHandleQuery", "query (%d) outside max (%d)\n", ucQ,
	     M_QRTBLSIZ );
  else if ( _MS.QRT[ ucQ ] == NULL ) {
    ALGripe( "MHandleQuery", "no query routine registered for (%d)\n", ucQ );
    (void) ALTCPGenRequest( pLI->hLink, MrQueryError, ucQ, (pMtOpaque)NULL,
			    0 );
  } else {

    _MS.bReplyFlag = M_True;	/* set reply verification flag */

    	/* call registered query handler routine */

    ( _MS.QRT[ ucQ ] )( ucQ, pReq->pData, hLI );

    if ( _MS.bReplyFlag == M_True ) {
      _MS.bReplyFlag = M_False;
      (void) ALTCPGenRequest( pLI->hLink, MrQueryError, ucQ, (pMtOpaque)NULL,
			     0 );
    }

  }

} /* end MHandleQuery() */


MtStatus MGenReply( hLI, ucOutOp, pData, iDataLen )
  hMtLinkInfo	hLI;
  u_char	ucOutOp;
  pMtOpaque	pData;
  int		iDataLen;
{
  int			iRet;

  if ( ! MmIsLIHandleValid( hLI ) )
    return MsLOSE;

  iRet = MGenRequest( hLI, MrQueryReply, ucOutOp, pData, iDataLen );

  _MS.bReplyFlag = M_False;

  return iRet;

} /* end MGenReply() */


void MHandleDataflow( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{
  register u_char	ucDF;
  hMtLinkInfo		hLI;

  (void) MLinkInfoToHandle( pLI, &hLI );
  ucDF = pReq->Hdr.op1;
  if ( ucDF > M_DRTBLSIZ )
    ALGripe( "MHandleDataflow", "dataflow type (%d) outside max (%d)\n", ucDF,
	     M_DRTBLSIZ );
  else if ( _MS.DRT[ ucDF ] == NULL )
    ALGripe( "MHandleDataflow", "no handler installed for type (%d)\n", ucDF );
  else
    ( _MS.DRT[ ucDF ] )( ucDF, pReq->pData, hLI );

} /* end MHandleDataflow() */


void MDataflowControl( pReq, pLI )
  pALtRequest	pReq;
  pMtLinkInfo	pLI;
{
  register u_char	ucDF;
  register int		iOperation;
  int			hLI;

DEBUG( DBGAll )
  ALGripe( "info", "changing dataflow...\n" );
ENDDB()

  (void) MLinkInfoToHandle( pLI, &hLI );

  if ( pReq->pData == NULL ) {
    ALGripe( "MDataflowControl", "no dataflow operation specified\n" );
    return;
  }

  iOperation = ntohl( *( (int *)pReq->pData ) );

  ucDF = pReq->Hdr.op1;

  MDiddleDataflow( ucDF, hLI, iOperation );

} /* end MDataflowControl() */


void MDiddleDataflow( ucDF, hLI, iHow )
  u_char	ucDF;
  int		iHow;
{
  int			iTgt;
  int			iEnd;
  phMtLinkInfo		pLIEntry;
  register int		i;

  if ( ucDF > M_OUTDRTBLSIZ ) {
    ALGripe( "MDiddleDataflow", "specified dataflow type (%d) is outside max (%d)\n", ucDF, M_OUTDRTBLSIZ );
    return;
  }

  if ( iHow == M_Enable ) {	/* enable a dataflow... */

	/* if slot is null, allocate initial entries */
    
    if ( ( pLIEntry = _MS.DFTable[ ucDF ] ) == NULL ) {
DEBUG( DBGAll )
      ALGripe( "info", "no slot in table for (%d); creating...\n", ucDF );
ENDDB()

      TRYMALLOCBLK( pLIEntry, phMtLinkInfo, hMtLinkInfo,
		    M_DFBLKSIZ );
      if ( pLIEntry == NULL ) {
	ALGripe( "MDiddleDataflow", "no memory for new LI table block\n" );
	return;
      }
      _MS.DFTEntryCount[ ucDF ] = M_DFBLKSIZ;
      for ( i = 0; i < M_DFBLKSIZ; i++ )
	pLIEntry[ i ] = -1;
      _MS.DFTable[ ucDF ] = pLIEntry;
      iTgt = 0;

    } else {	/* slot not null; locate empty spot & check to see if LI is */
		/*   possibly already in the table */      

      for ( iTgt = 0; iTgt < _MS.DFTEntryCount[ ucDF ]; iTgt++ )
        if ( pLIEntry[ iTgt ] == hLI )	/* handle already req'd df */
	  return;
	else if ( pLIEntry[ iTgt ] == -1 )	/* found vacant spot */
	  break;

      if ( iTgt == _MS.DFTEntryCount[ ucDF ] ) { /* need 2 make slot bigger */

	TRYREALLOCBLK( pLIEntry, pLIEntry, phMtLinkInfo, hMtLinkInfo,
		       iTgt + M_DFBLKSIZ );
	for ( i = iTgt; i < iTgt + M_DFBLKSIZ; i++ )
	  pLIEntry[ i ] = -1;
	_MS.DFTEntryCount += M_DFBLKSIZ;
	_MS.DFTable[ ucDF ] = pLIEntry;	/* in case it's changed */

      }	/* end make slot bigger */

    } /* end locating blank entry */

    pLIEntry[ iTgt ] = hLI;	/* put LI in table */

	/* NOTICE: I'm not going to put in code to deal with the MtDFSets */
	/*   right now.  They may be superfluous with the addition of the */
	/*   DFTable.  If that's the case, then they'll disappear. */

    	/* see if there's a df init procedure registered; call it if so */

    if ( _MS.DIRT[ ucDF ] != NULL )
      (_MS.DIRT[ ucDF ])( ucDF, hLI );

  } else if ( iHow == M_Disable ) {		/* disable a dataflow... */

	/* if slot is null, ignore disable request */

    if ( ( pLIEntry = _MS.DFTable[ ucDF ] ) == NULL )
      return;

    	/* look for handle in slot; remove if there, ignore if not */

    for ( iTgt = 0; iTgt < _MS.DFTEntryCount[ ucDF ]; iTgt++ )
      if ( pLIEntry[ iTgt ] == hLI )
	break;

    for ( iEnd = _MS.DFTEntryCount[ ucDF ] - 1; iEnd > iTgt; iEnd-- )
      if ( pLIEntry[ iEnd ] != -1 )
	break;

    if ( iTgt != iEnd )
      pLIEntry[ iTgt ] = pLIEntry[ iEnd ];

    if ( iEnd > 0 )
      pLIEntry[ iEnd ] = -1;
    else {
      (void) ALFree( (char *)pLIEntry );
      _MS.DFTable[ ucDF ] = (phMtLinkInfo)NULL;
      _MS.DFTEntryCount[ ucDF ] = 0;
    }

  } else
    ALGripe( "MDiddleDataflow", "unknown operation(%d)\n", iHow );

} /* end MDiddleDataflow() */


void MCleanupDeadLink( pDI )
  pALtCLDeathInfo	pDI;
{
  pMtLinkInfo		pLI;
  hMtLinkInfo		hLI;
  register int		i;

  pLI = (pMtLinkInfo)pDI->pData;

  (void) MLinkInfoToHandle( pLI, &hLI );

  for ( i = 0; i < M_OUTDRTBLSIZ; i++ )
    MDiddleDataflow( i, hLI, M_Disable );

DEBUG( DBGAll )
    ALGripe( "info", "wiped LI %d\n", hLI );
ENDDB()

  ALFree( (char *)pLI );
  pLI = (pMtLinkInfo)NULL;
  _MS.LIT.iLICount--;
  _MS.LIT.pLIs[ hLI ] = (pMtLinkInfo)NULL;

} /* end MCleanupDeadLink() */


MtStatus MLinkInfoToHandle( pLI, phLI )
  pMtLinkInfo	pLI;
  phMtLinkInfo	phLI;
{
  register int		i;

  if ( phLI == NULL )
    return MsLOSE;

  for ( i = 0; i < _MS.LIT.iLICount; i++ )
    if ( _MS.LIT.pLIs[ i ] == pLI )
      break;

  if ( i == M_LITBLSIZ )
    return MsLOSE;

  *phLI = i;

  return MsOK;

} /* end MLinkInfoToHandle() */


int MShowDFTable( )
{
  int		i;

  printf( "DFTable[0]: " );
  if ( _MS.DFTable[0] == NULL )
    printf( "<null>\n" );
  else {
    printf( "[%d] [%d] [%d] [%d]\n", _MS.DFTable[0][0], _MS.DFTable[0][1],
	    _MS.DFTable[0][2], _MS.DFTable[0][3] );
  }

} /* end MShowDFTable() */


MtStatus MSetFlowcontrol( hLI, iHiWM, iLoWM )
  hMtLinkInfo	hLI;
  int		iHiWM;
  int		iLoWM;
{
  pMtLinkInfo		pLI;
  MtSendWindow		SW;
  MtStatus		iRet;

  if ( ! MmIsLIHandleValid( hLI ) )
    return MsLOSE;

  pLI = MmHandleToLinkInfo( hLI );

  if ( iHiWM < 0 ) {
    iHiWM = -1;		/* turn off flowcontrol; no need to wait for ack */
    iLoWM = -1;
  } else {

    if ( iHiWM == 0 )		/* whatever LoWM is, it has to be less than */
      iHiWM = 1;		/*   iHiWM, so iHiWM can't be zero. */

    if ( iLoWM < 0 )
      iLoWM = 0;
    else if ( iLoWM >= iHiWM )
      iLoWM = iHiWM - 1;

    pLI->iAckWait = M_True;	/* we're going to be waiting for an ack */

  }

  pLI->iHiWM = iHiWM;
  pLI->iLoWM = iLoWM;
  pLI->iLevel = iHiWM;
	/* artificially inflate the update by one to account for peer's */
  	/*   transmission of an ACK, which is counted on peer's end but */
  	/*   not included in the definition of our iLevel peer estimate */
  SW.iUpdate = ( iHiWM == -1 ) ? -1 : iHiWM + 1;

DEBUG( DBGAll )
  ALGripe( "info", "requesting flow control setting; window (%d)\n",
	   iHiWM );
ENDDB()

  if (( iRet = MGenRequest( hLI, MrPRIVATE, MrtSetSendWin, (pMtOpaque)&SW,
			    sizeof( SW ) )) != MsOK )
    ALGripe( "MSetFlowcontrol", "MGenRequest isn't happy: (%d)\n", iRet );

  return iRet;

} /* end MSetFlowcontrol() */


MtStatus MDistributeDataflow( ucDF, pData, iDataLen )
  u_char	ucDF;
  pMtOpaque	pData;
  int		iDataLen;
{
  register int		i;
  MtStatus		iRet;
  phMtLinkInfo		pLIs;

  if ( ucDF >= M_OUTDRTBLSIZ ) {
    ALGripe( "MDistDF.", "df type (%d) outside max (%d)\n", ucDF,
	     M_OUTDRTBLSIZ );
    return MsLOSE;
  }

  pLIs = _MS.DFTable[ ucDF ];
  for ( i = 0; i < _MS.DFTEntryCount[ ucDF ] && pLIs[ i ] != -1; i++ ) {

    if (( iRet = MGenRequest( pLIs[ i ], MrDataflowEvent, ucDF, pData,
			      iDataLen )) != MsOK )
      ALGripe( "MDistDF.", "note MGenRequest failure (%d) on LI (%d)\n",
	       iRet, pLIs[i] );

  }

  return MsOK;

} /* end MDistributeDataflow() */


MtStatus MGetLinkFD( hLI, pFD )
  hMtLinkInfo	hLI;
  int *		pFD;
{
  pMtLinkInfo	pLI;

  if ( ! MmIsLIHandleValid( hLI ) )
    return MsLOSE;

  pLI = MmHandleToLinkInfo( hLI );

  (void) ALGetLinkFD( pLI->hLink, pFD );

  return MsOK;

}




/* end mlayer.c */
