/*
* mrequest.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __MREQUEST_H
#define __MREQUEST_H

#define	MrPRIVATE		0x01
#define	MrERROR			0x02
#define	MrMessage		0x03
#define	MrQuery			0x04
#define	MrQueryReply		0x05
#define	MrQueryError		0x06
#define	MrModifyDataflow	0x07
#define	MrDataflowEvent		0x08

/*** MrPrivate: **************************************************************/

#define	MrtSetSendWin		0x00
#define	MrtAckSetSendWin	0x01
#define	MrtUpdateSendWin	0x02

/*** MrtSetSendWin,    ***/
/*** MrtUpdateSendWin: ***/

typedef struct _m_sendwindow {

  int		iUpdate;

} MtSendWindow, * pMtSendWindow;

/*** MrERROR: ****************************************************************/

/* no MrERRORs defined yet; may not be used... */

#endif  /* ifndef __MREQUEST_H; Add nothing past this point */
