/* interpret.h - external definition of L-system interpreter.
 *
 * $Id: interpret.h,v 1.2 90/10/12 18:48:20 leech Exp Locker: leech $
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
 * $Log:	interpret.h,v $
 * Revision 1.2  90/10/12  18:48:20  leech
 * First public release.
 * 
 */

#ifndef INTERPRET_H
#define INTERPRET_H

#include "DBGenerator.h"

// In interpret.c
extern void
    interpret(List(Module) &ml,
	      DBGenerator  &g,
	      boolean	    plant,
	      float	    turn = 90,
	      float	    width_scale = 1);
#endif /*INTERPRET_H*/
