/* Symtab.c - methods for symbol tables of specific object types.
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
 * $Log:	Symtab.c,v $
 * Revision 1.2  90/10/12  18:48:12  leech
 * First public release.
 * 
 */
static char RCSid[] = "$Id: Symtab.c,v 1.2 90/10/12 18:48:12 leech Exp Locker: leech $";

#include "Symtab.h"
#include "Value.h"

SymtabImplement(Value,"","",'\n');
SymtabImplement(Anyptr,"","",'\n');
SymtabImplement(int,"","",'\n');
