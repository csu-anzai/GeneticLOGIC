/* debug.h - debugging macros usable in C or C++.
 *
 * $Id: debug.h,v 1.3 91/03/20 10:40:46 leech Exp Locker: leech $
 *
 * Copyright (C) 1990, Jonathan P. Leech
 *
 * This software may be freely copied, modified, and redistributed,
 * provided that this copyright notice is preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is". Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 *
 * $Log:	debug.h,v $
 * Revision 1.3  91/03/20  10:40:46  leech
 * New debug bit for Names. Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:18  leech
 * First public release.
 *
 */

#ifndef DEBUG_H
#define DEBUG_H

#include "boolean.h"

/* Debugging macros for lsys program; includeable by either C or C++ source */
extern int ParseDebug;

#define PD_EXPRESSION  0x1
#define PD_LEXER       0x2
#define PD_MAIN        0x4
#define PD_MODULE      0x8
#define PD_PARSER     0x10
#define PD_PRODUCTION 0x20
#define PD_SYMBOL     0x40
#define PD_INTERPRET  0x80
#define PD_MEMLEAK   0x100
#define PD_NAME      0x200

#ifdef	PDEBUG_ENABLED
#define PDEBUG(level,code) if (ParseDebug & (level)) { code ; }
#else
#define PDEBUG(level,code)
#endif /*PDEBUG_ENABLED*/

/* Ensure that memory logging is turned off before static destructors
 *  are called and other cleanup is done.
 */
#define exit(sts) do_exit(sts)

#ifdef __cplusplus
/* This is a hack to get around a bogus #define of NULL in <stddef.h> */
#define NULL 0
#include <stddef.h>

extern "C" void do_exit(int status);

extern void memlog_enable(boolean on);
extern void memalloc(const char *label, void *p);
extern void memfree(const char *label, void *p);
void  *operator new(size_t sz, char *label);
void   delete_sz(void *p, size_t sz, char *label = NULL);
#endif

#ifndef __GNUG__
#define DELETE_OPERATOR(type,name) \
    void operator delete(void *p, size_t s) { \
	delete_sz(p,s,name); }
#else
#define DELETE_OPERATOR(type,name) \
    void operator delete(void *p) { \
	delete_sz(p,sizeof(type),name); }
#endif

#endif /*DEBUG_H*/
