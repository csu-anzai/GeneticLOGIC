/* Module.h - class definition for L-system modules.
 *
 * $Id: Module.h,v 1.3 91/03/20 10:39:39 leech Exp Locker: leech $
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
 * $Log:	Module.h,v $
 * Revision 1.3  91/03/20  10:39:39  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:07  leech
 * First public release.
 *
 */

#ifndef MODULE_H
#define MODULE_H

#include "Expression.h"

// A Module is the basic object of an L-system to which productions
//  are applied, and which is interpreted in terms of turtle movements
//  and graphical objects. Since there are so many of them allocated,
//  the class is tightly packed.
class Module {
    short	      tag;		// Module name
    char	      ignoreflag;	// Should module be ignored in context?
    char	      emptyflag;	// Should tag & param be deleted?
    List(Expression) *param;		// Expressions bound to module
public:
    Module(const Name &name, List(Expression) *elist, boolean ignore = false);
    Module(Module &m);

    void *operator new(size_t s) { return ::operator new(s,"Module"); }
    DELETE_OPERATOR(Module,"Module");
   ~Module();

    // Call empty() before deallocating a module if it
    //	has been used as the argument to a copy constructor.
    void empty();

    Name name() { return Name(tag); }

    boolean bind(Module &values, Symtab(Value) &st);
    boolean conforms(Module &m);
    boolean ignore() { return ignoreflag ? true : false; }
    Module *instantiate(Symtab(Value) &st);
    boolean getfloat(float &f, unsigned int n = 0);

    friend ostream &operator<<(ostream &o, Module &m);
};

ListDeclare(Module);

// These Names are used in context matching to ascend/descend tree levels
extern Name
    LBRACKET,
    RBRACKET;

#endif /*MODULE_H*/

