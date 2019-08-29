/*
* rqueue.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <fcntl.h>
#include <sys/time.h>

#include "alcommp.h"


/*
* ALInitRQueue()
*
* params
*
* returns
*
*/

ALtStatus ALInitRQueue( )
{

  _AS.RQ.iQLen = 0;			/* queue length zero */
  _AS.RQ.pQueue = (pALtRequest)NULL;	/* empty queue */
  _AS.RQ.ppTail = &_AS.RQ.pQueue;	/* ptr to insertion point */

  return ALsOK;

} /* end ALInitRQueue() */


/*
* ALEnqueueRequest()
*
* params
*
* returns
*
*/

ALtStatus ALEnqueueRequest( pReq )
  pALtRequest	pReq;
{
  ALmProtectedFromSignals;
  register int		iRet;

  ALmBlockSignals();

  if ( pReq == NULL )
    iRet = ALsINVALID;
  else {
    *_AS.RQ.ppTail = pReq;
    pReq->pNext = (pALtRequest)NULL;
    _AS.RQ.ppTail = &pReq->pNext;
    _AS.RQ.iQLen++;
    iRet = ALsOK;
  }

  ALmUnblockSignals();

  return iRet;

} /* end ALEnqueueRequest() */


/*
* ALDequeueRequest()
*
* params
*
* returns
*
*/

ALtStatus ALDequeueRequest( ppReq )
  pALtRequest *		ppReq;
{
  ALmProtectedFromSignals;
  register int		iRet;

  ALmBlockSignals();

  if ( ppReq == NULL )
    iRet = ALsINVALID;
  else if ( _AS.RQ.iQLen == 0 )
    iRet = ALsLOSE;	/* no requests in queue */
  else {
    *ppReq = _AS.RQ.pQueue;
    if (( _AS.RQ.pQueue = _AS.RQ.pQueue->pNext ) == NULL )
      _AS.RQ.ppTail = &_AS.RQ.pQueue;
    _AS.RQ.iQLen--;
    iRet = ALsOK;
  }

  ALmUnblockSignals();

  return iRet;

} /* end ALDequeueRequest() */





/* end rqueue.c */
