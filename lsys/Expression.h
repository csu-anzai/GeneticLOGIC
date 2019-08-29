/* Expression.h - class definition for arithmetic expressions.
 *
 * $Id: Expression.h,v 1.3 91/03/20 10:39:26 leech Exp Locker: leech $
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
 * $Log:	Expression.h,v $
 * Revision 1.3  91/03/20  10:39:26  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:03  leech
 * First public release.
 *
 */

#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "Value.h"
#include "Name.h"

// Forward declaration for expressions of the form f(a,b,c) which
//  themselves contain lists of expressions.
class List(Expression);

class Expression {
    int   op;
    union {
	struct {
	    int		      id;   // Name id
	    List(Expression) *args; // Function arguments
	} name;
	char  v[sizeof(Value)];     // Ensure union is big enough for a Value
	Expression *args[2];	    // Child expressions
    } val;

    Name	      varname()  { return Name(val.name.id); }
    Value	      value()	 { return *(Value *)&val.v; }
    Expression	     *lchild()	 { return val.args[0]; }
    Expression	     *rchild()	 { return val.args[1]; }
    Name	      funcname() { return Name(val.name.id); }
    List(Expression) *funcargs() { return val.name.args; }
public:
    Expression(int o, Expression *lop, Expression *rop);
    Expression(const Name &name, List(Expression) *args = NULL);
    Expression(Value *v);
    Expression(Value &v);

    void *operator new(size_t s) { return ::operator new(s,"Expression"); }
    DELETE_OPERATOR(Expression,"Expression");
   ~Expression();

    // Access methods
    int type() { return op; }
    Name name();

    // Evaluation methods
    Value evaluate(Symtab(Value) &st);
    Value leval(Symtab(Value) &st) { return lchild()->evaluate(st); }
    Value reval(Symtab(Value) &st) { return rchild()->evaluate(st); }

    friend ostream &operator<<(ostream &o, Expression &e);
};

ListDeclare(Expression);

boolean bind(List(Expression) *formals, List(Expression) *values, Symtab(Value) &st);
boolean conforms(List(Expression) *formals, List(Expression) *values);
List(Expression) *instantiate(List(Expression) *before, Symtab(Value) &st);
boolean getfloat(Symtab(Value) &st, List(Expression) &list, float &f, unsigned int n = 0);
boolean getvalue(Symtab(Value) &st, List(Expression) &list, Value &f, unsigned int n = 0);

#endif /*EXPRESSION_H*/

