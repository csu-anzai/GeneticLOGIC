/* parser.h - global variables produced by L-system parser.
 *
 * $Id: parser.h,v 1.2 90/10/12 18:48:25 leech Exp Locker: leech $
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
 * $Log:	parser.h,v $
 * Revision 1.2  90/10/12  18:48:25  leech
 * First public release.
 * 
 */

#ifndef PARSER_H
#define PARSER_H

// Headers needed by just about everybody
#include "global_headers.h"

#include "Symtab.h"
#include "Value.h"
#include "Expression.h"
#include "Module.h"
#include "Production.h"

extern Symtab(Value)	IgnoreTable,	// Symbols ignored in context
			SymbolTable;	// Variables and bound formal parameters
extern List(Production) Rules;		// Production list
extern List(Module)    *Start;		// Starting module list

#endif /*PARSER_H*/
