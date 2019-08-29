/*
* debug.h
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef __DEBUG_H
#define __DEBUG_H


#ifdef DBUG

#ifdef	_DBGMainModule
u_long		_debug_state = 0L;
#else
extern u_long	_debug_state;
#endif

#define	DEBUG(db)	if (_debug_state & (db)) {
#define	ENDDB()		}

#else /* ...not DBUG */

#define	DEBUG(db)	if (0) {
#define	ENDDB()		}

#endif /* DBUG */


#define	DBGComm		0x0001
#define DBGAll		0xffff



#endif  /* ifndef __DEBUG_H; Add nothing past this point */
