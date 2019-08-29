/* Value.c - methods for mixed int/float arithmetic in Expressions
 *  with runtime typing.
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
 * $Log:	Value.c,v $
 * Revision 1.3  91/03/20  10:36:06  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:14  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: Value.c,v 1.3 91/03/20 10:36:06 leech Exp Locker: leech $";

#include "global_headers.h"
#include "Value.h"

// If not initialized to a value, has an undefined value
Value::Value() {
    type = UNDEFINED;
}

Value::Value(int i) {
    type = INT;
    val.ival = i;
}

Value::Value(float f) {
    type = FLOAT;
    val.fval = f;
}

Value::Value(double d) {
    type = FLOAT;
    val.fval = d;
}

Value::Value(Value &v) {
    type = v.type;
    val  = v.val;
}

Optype Value::binary_optype(Value &v) {
    if (type == INT) {
	if (v.type == INT)
	    return II;
	else if (v.type == FLOAT)
	    return IF;
    } else if (type == FLOAT) {
	if (v.type == INT)
	    return FI;
	else if (v.type == FLOAT)
	    return FF;
    }
    return UNDEF;
}

// Unary negation
Value Value::operator-() {
    switch (type) {
	case INT:
	    return Value(-val.ival);
	case FLOAT:
	    return Value(-val.fval);
	case UNDEFINED:
	default:
	    return *this;
    }
}

// Unary bitwise complement
Value Value::operator~() {
    switch (type) {
	case INT:
	    return Value(~val.ival);
	case FLOAT:
	    cerr << "Value::operator~(): cannot complement non-integer value" << endl;
	case UNDEFINED:
	default:
	    return Value();
    }
}

// Unary logical complement
Value Value::operator!() {
    switch (type) {
	case INT:
	    return Value(!val.ival);
	case FLOAT:
	    cerr << "Value::operator!(): cannot complement non-integer value" << endl;
	case UNDEFINED:
	default:
	    return Value();
    }
}

// Absolute value
Value Value::abs() {
    switch (type) {
	case INT:
	    return Value(val.ival > 0 ? val.ival : -val.ival);
	case FLOAT:
	    return Value(fabs(val.fval));
	case UNDEFINED:
	default:
	    return Value();
    }
}

// Binary bitwise AND
Value Value::operator& (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival & v.val.ival);
	case IF:
	case FI:
	case FF:
	    cerr << "Value::operator&(): cannot bitwise AND non-integer values" << endl;
	case UNDEF:
	    return Value();
    }
}

// Binary bitwise OR
Value Value::operator| (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival | v.val.ival);
	case IF:
	case FI:
	case FF:
	    cerr << "Value::operator|(): cannot bitwise OR non-integer values" << endl;
	case UNDEF:
	    return Value();
    }
}

// Binary logical AND
Value Value::operator&&(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival && v.val.ival);
	case IF:
	case FI:
	case FF:
	    cerr << "Value::operator&&(): cannot logical AND non-integer values" << endl;
	case UNDEF:
	    return Value();
    }
}

// Binary logical OR
Value Value::operator||(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival || v.val.ival);
	case IF:
	case FI:
	case FF:
	    cerr << "Value::operator||(): cannot logical OR non-integer values" << endl;
	case UNDEF:
	    return Value();
    }
}

// Equality
Value Value::operator==(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival == v.val.ival);
	case IF:
	    return Value(val.ival == v.val.fval);
	case FI:
	    return Value(val.fval == v.val.ival);
	case FF:
	    return Value(val.fval == v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Inequality
Value Value::operator!=(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival != v.val.ival);
	case IF:
	    return Value(val.ival != v.val.fval);
	case FI:
	    return Value(val.fval != v.val.ival);
	case FF:
	    return Value(val.fval != v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Less-than
Value Value::operator< (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival <  v.val.ival);
	case IF:
	    return Value(val.ival <  v.val.fval);
	case FI:
	    return Value(val.fval <  v.val.ival);
	case FF:
	    return Value(val.fval <  v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Less-than or equal-to
Value Value::operator<=(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival <= v.val.ival);
	case IF:
	    return Value(val.ival <= v.val.fval);
	case FI:
	    return Value(val.fval <= v.val.ival);
	case FF:
	    return Value(val.fval <= v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Greater-than or equal-to
Value Value::operator>=(Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival >= v.val.ival);
	case IF:
	    return Value(val.ival >= v.val.fval);
	case FI:
	    return Value(val.fval >= v.val.ival);
	case FF:
	    return Value(val.fval >= v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Greater-than
Value Value::operator> (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival >  v.val.ival);
	case IF:
	    return Value(val.ival >  v.val.fval);
	case FI:
	    return Value(val.fval >  v.val.ival);
	case FF:
	    return Value(val.fval >  v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Binary addition
Value Value::operator+ (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival +  v.val.ival);
	case IF:
	    return Value(val.ival +  v.val.fval);
	case FI:
	    return Value(val.fval +  v.val.ival);
	case FF:
	    return Value(val.fval +  v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Subtraction
Value Value::operator- (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival -  v.val.ival);
	case IF:
	    return Value(val.ival -  v.val.fval);
	case FI:
	    return Value(val.fval -  v.val.ival);
	case FF:
	    return Value(val.fval -  v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Multiplication
Value Value::operator* (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(val.ival *  v.val.ival);
	case IF:
	    return Value(val.ival *  v.val.fval);
	case FI:
	    return Value(val.fval *  v.val.ival);
	case FF:
	    return Value(val.fval *  v.val.fval);
	case UNDEF:
	    return Value();
    }
}

// Division (checks for /0 and returns an undefined number)
// Note that int / int -> float, unlike C rules
Value Value::operator/ (Value &v) {
    float divisor, dividend;

    if (type == UNDEFINED || v.type == UNDEFINED)
	return Value();

    divisor = (v.type == INT) ? (float)v.val.ival : v.val.fval;

    if (divisor == 0)
	return Value();

    dividend = (type == INT) ? (float)val.ival : val.fval;
    return Value(dividend / divisor);
}

// Modulo (checks for /0 and returns an undefined number)
Value Value::operator% (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    if (v.val.ival == 0)
		return Value();
	    else
		return Value(val.ival % v.val.ival);
	    break;
	case IF:
	case FI:
	case FF:
	    cerr << "Value::operator%(): cannot compute modulo of non-integer values" << endl;
	case UNDEF:
	    return Value();
    }
}

// Power
Value Value::operator^ (Value &v) {
    switch (binary_optype(v)) {
	case II:
	    return Value(pow(val.ival,v.val.ival));
	case IF:
	    return Value(pow(double(val.ival),v.val.fval));
	case FI:
	    return Value(pow(val.fval,v.val.ival));
	case FF:
	    return Value(pow(val.fval,v.val.fval));
	case UNDEF:
	    return Value();
    }
}

// Returns numeric value in i, true if a valid integer
boolean Value::value(int &i) {
    if (type == INT) {
	i = val.ival;
	return true;
    } else
	return false;
}

// Returns numeric value in f, true if a valid integer or real number
//  (integers are converted to floats).
boolean Value::value(float &f) {
    if (type == INT) {
	f = val.ival;
	return true;
    } else if (type == FLOAT) {
	f = val.fval;
	return true;
    } else
	return false;
}

ostream &operator<<(ostream &o, const Value &v) {
    if (&v == NULL)
	return o;

    switch (v.type) {
	case INT:
	    o << v.val.ival;
	    break;
	case FLOAT:
#ifndef __GNUG__
	    o.setf(ios::showpoint);
	    o << v.val.fval;
	    o.unsetf(ios::showpoint);
#else
	    o << v.val.fval;
#endif /*__GNUG__*/
	    break;
	case UNDEFINED:
	default:
	    o << "(undefined value)";
	    break;
    }
    return o;
}

