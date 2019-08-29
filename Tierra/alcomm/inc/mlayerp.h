/*
* mlayerp.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __MLAYERP_H
#define __MLAYERP_H


#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/param.h>

#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "alcomm.h"
#include "alreques.h"
#include "debug.h"
#include "mlayer.h"
#include "mrequest.h"


#define	M_DFSetSize		128


typedef struct _m_dfset		MtDFSet;

struct _m_dfset {
  u_long	bits[ 4 ];	/* HARDCODED CONSTANT; yields 128 bits */
};

#define	DFSetZERO( _dfset )						\
  bzero((char *)(_dfset),sizeof(*(_dfset)))
#define	DFSetSET( _dfset, _dfbit )					\
  ((_dfset)->bits[(_dfbit)>>5] |= (1<<((_dfbit)%32)))
#define	DFSetCLEAR( _dfset, _dfbit )					\
  ((_dfset)->bits[(_dfbit)>>5] &= ~(1<<((_dfbit)%32)))
#define	DFSetSETMERGE( _dfset, _andset )				\
  { (_dfset)->bits[0] |= (_andset)->bits[0];				\
    (_dfset)->bits[1] |= (_andset)->bits[1];				\
    (_dfset)->bits[2] |= (_andset)->bits[2];				\
    (_dfset)->bits[3] |= (_andset)->bits[3]; }
#define	DFSetISSET( _dfset, _dfbit )					\
  ((_dfset)->bits[(_dfbit)>>5] & (1<<((_dfbit)%32)))


typedef struct _m_global_set	MtGlobalSet;

typedef struct _m_reply_buf	MtReplyBuf;


struct _m_reply_buf {

  int		bValid;
  u_char	ucOp;
  pMtOpaque	pData;
  int		iDataLen;

};


struct _m_link_info {

  hALtCLink	hLink;		/* handle of this clink */

  MtDFSet	DFS;

  int		iHiWM;		/* max # reqs allowed on q. from this link */
  int		iLoWM;		/* point at which win update is sent */
  int		iAckWait;	/* true while waiting for SetSendWin ack */
  int		iLevel;		/* total possible outstanding requests */

  int		iOutWin;	/* # reqs which may be sent out */

  MtReplyBuf	RB;

};


struct _m_litable {

  MtDFSet		GlobalDFS;
  ppMtLinkInfo		pLIs;		/* link info pointer list */
  int			iLICount;	/* number of links */
  int			iCurLink;	/* index of current link */

};


struct _m_global_set {

  int			iDefaultHiWM;		/* def. hi wm for new links */
  int			iDefaultLoWM;		/* def. lo wm for new links */

  MtLITable		LIT;

  pMfnRoutine		MRT;

  pMfnRoutine		QRT;

  pMfnRoutine		DRT;

  pMfnRoutine		DIRT;		/* how unfortunate :-P */

  phMtLinkInfo *	DFTable;
  int *			DFTEntryCount;

  int			bReplyFlag;

};



#endif  /* ifndef __MLAYERP_H; Add nothing past this point */
