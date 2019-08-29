/*
* mlayer.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __MLAYER_H
#define __MLAYER_H

#include "portlayr.h"
#include "mrequest.h"

#ifndef u_char
#include <sys/types.h>
#endif


#define	M_LITBLSIZ		16

#define	M_MRTBLSIZ		128
#define	M_QRTBLSIZ		128
#define	M_DRTBLSIZ		128
#define	M_OUTDRTBLSIZ		128
#define	M_DFBLKSIZ		4

#define	M_False			0
#define	M_True			(! M_False)

#define	M_Wait			0
#define	M_NoWait		1

#define	M_Disable		0
#define	M_Enable		1

#define	MrUSER			8	/* first 8  major opcodes reserved */

#define	MsOK			0
#define	MsLOSE			-1
#define	MsNOMEM			-2
#define	MsDEADLINK		-3
#define	MsMONDO			-99

#define	M_NoFns			(pMtDefaultRoutines)NULL
#define	M_NoData		(pMtOpaque)NULL

typedef void			(*MfnRoutine) P_(( ));
typedef MfnRoutine *		pMfnRoutine;

typedef int			MtStatus;

typedef void			MtOpaque, * pMtOpaque;
typedef int			hMtLinkInfo, * phMtLinkInfo;

typedef struct _m_link_info	MtLinkInfo, * pMtLinkInfo, ** ppMtLinkInfo;
typedef struct _m_litable	MtLITable, * pMtLITable;

typedef struct _m_def_routines	MtDefaultRoutines, * pMtDefaultRoutines;

struct _m_def_routines {

  u_char		ucWhich;
  MfnRoutine		fnRoutine;

};


extern int *		_MSDFTblCnt;	/* user: don't worry about this */


extern MtStatus		MInitialise P_(( ));

extern MtStatus		MConnectTo P_(( char *sHost, int iPort, int * hLink));

void			MProcessALPrivateRequest P_(( ));
void			MProcessMPrivateRequest P_(( ));
void			MProcessRequest P_(( ));
void			MCleanupDeadLink P_(( ));

#define	MIsDFEnabled(df)						\
  (((df)<M_OUTDRTBLSIZ)&&(_MSDFTblCnt[(df)]>0))


#define	MmIsLIHandleValid(h)						\
  (((h)>=0)&&((h)<M_LITBLSIZ)&&(_MS.LIT.pLIs[(h)]!=NULL))

#define	MmHandleToLinkInfo(h)		(_MS.LIT.pLIs[(h)])


#define	MHtoNS(d)	(d)
#define	MHtoNL(d)	(d)
#define	MNtoHS(d)	(d)
#define	MNtoHL(d)	(d)


#endif  /* ifndef __MLAYER_H; Add nothing past this point */
