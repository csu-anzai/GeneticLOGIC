/* global_headers.h - headers needed by almost all lsys modules.
 *
 * $Id: global_headers.h,v 1.3 91/03/20 10:41:06 leech Exp Locker: leech $
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
 * $Log:	global_headers.h,v $
 * Revision 1.3  91/03/20  10:41:06  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:19  leech
 * First public release.
 *
 */

#ifndef GLOBAL_H
#define GLOBAL_H

#include "streamio.h"
#include <string.h>
#include <math.h>

#include "boolean.h"
#include "debug.h"
#include "List.h"

// Should include <rand48.h>, but this isn't on the Ultrix distribution
extern "C" {
    double drand48();
    long   lrand48();
    void   srand48(long);
};

// In lex.l
extern "C" char *strdup(const char *s);

#endif /*GLOBAL_H*/
