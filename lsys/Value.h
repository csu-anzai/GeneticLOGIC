/* Value.h - class definition for mixed int/float arithmetic in Expressions
 *  with runtime typing.
 *
 * $Id: Value.h,v 1.3 91/03/20 10:40:38 leech Exp Locker: leech $
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
 * $Log:	Value.h,v $
 * Revision 1.3  91/03/20  10:40:38  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:15  leech
 * First public release.
 *
 */

#ifndef VALUE_H
#define VALUE_H

#include "boolean.h"
#include "Symtab.h"

enum ValueType { INT, FLOAT, UNDEFINED };

class Value {
    ValueType type;
    union {
	int   ival;
	float fval;
    } val;

    enum Optype { II, IF, FI, FF, UNDEF };
    Optype binary_optype(Value &v);

public:
    Value();
    Value(int);
    Value(float);
    Value(double);
    Value(Value &v);

    void *operator new(size_t s) { return ::operator new(s,"Value"); }
    DELETE_OPERATOR(Value,"Value");

    Value operator-();
    Value operator~();
    Value operator!();
    Value abs();
    Value operator& (Value &v);
    Value operator| (Value &v);
    Value operator&&(Value &v);
    Value operator||(Value &v);
    Value operator==(Value &v);
    Value operator!=(Value &v);
    Value operator< (Value &v);
    Value operator<=(Value &v);
    Value operator>=(Value &v);
    Value operator> (Value &v);
    Value operator+ (Value &v);
    Value operator- (Value &v);
    Value operator* (Value &v);
    Value operator/ (Value &v);
    Value operator% (Value &v);
    Value operator^ (Value &v);

    boolean value(int &i);
    boolean value(float &f);

    friend ostream &operator<<(ostream &o, const Value &v);
};

SymtabDeclare(Value);
#endif /*VALUE_H*/
