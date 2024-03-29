/* Expression.c - methods to evaluate arithmetic expressions.
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
 * $Log:	Expression.c,v $
 * Revision 1.3  91/03/20  10:35:16  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:03  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: Expression.c,v 1.3 91/03/20 10:35:16 leech Exp Locker: leech $";

#include "global_headers.h"
#include <time.h>
#include "vector.h"
#include "token.h"
#include "Symtab.h"
#include "Value.h"
#include "Expression.h"

#define EXPRFUNC(name) Value name(Symtab(Value) &st, List(Expression) &args)
typedef EXPRFUNC((*Exprfunc));

EXPRFUNC(expr_sin) {
    float x;

    if (getfloat(st, args, x))
	return Value(sin(dtor(x)));
    else
	return Value();
}

EXPRFUNC(expr_cos) {
    float x;

    if (getfloat(st, args, x))
	return Value(cos(dtor(x)));
    else
	return Value();
}

EXPRFUNC(expr_tan) {
    float x;

    if (getfloat(st, args, x))
	return Value(tan(dtor(x)));
    else
	return Value();
}

EXPRFUNC(expr_asin) {
    float x;

    if (getfloat(st, args, x))
	return Value(rtod(asin(x)));
    else
	return Value();
}

EXPRFUNC(expr_acos) {
    float x;

    if (getfloat(st, args, x))
	return Value(rtod(acos(x)));
    else
	return Value();
}

EXPRFUNC(expr_atan) {
    float x;

    if (getfloat(st, args, x))
	return Value(rtod(atan(x)));
    else
	return Value();
}

EXPRFUNC(expr_atan2) {
    float x, y;

    if (getfloat(st, args, y) && getfloat(st, args,x,1))
	return Value(rtod(atan2(y,x)));
    else
	return Value();
}

// Returns type of argument
EXPRFUNC(expr_abs) {
    Value v;

    if (getvalue(st, args, v))
	return v.abs();
    else
	return Value();
}

// Always returns int
EXPRFUNC(expr_ceil) {
    float x;

    if (getfloat(st, args, x))
	return Value((int)ceil(x));
    else
	return Value();
}

// Always returns int
EXPRFUNC(expr_floor) {
    float x;

    if (getfloat(st, args, x))
	return Value((int)floor(x));
    else
	return Value();
}

EXPRFUNC(expr_exp) {
    float x;

    if (getfloat(st, args, x))
	return Value(exp(x));
    else
	return Value();
}

EXPRFUNC(expr_log) {
    float x;

    if (getfloat(st, args, x))
	return Value(log(x));
    else
	return Value();
}

EXPRFUNC(expr_log10) {
    float x;

    if (getfloat(st, args, x))
	return Value(log10(x));
    else
	return Value();
}

// Return a uniformly distributed random number;
//  rand()  returns [0,1)
//  rand(n) returns [0,n)   (floating point)
EXPRFUNC(expr_rand) {
    float x;

    if (getfloat(st, args, x))
	return Value(x * drand48());
    else
	return Value(drand48());
}

// Reseed the random number generator and return the seed;
//  srand() sets the seed to the current time, while
//  srand(value) sets it to the specified value.
EXPRFUNC(expr_srand) {
    float x;
    long  seed;

    // Should have getint()
    if (getfloat(st, args, x))
	seed = long(x);
    else
	seed = time(NULL);

    srand48(seed);
    return Value((int)seed);
}

// The function table for function terms in expressions
static Symtab(Anyptr) *Functab = NULL;

static void setup_functions() {
    if (Functab)
	return;
    Functab = new Symtab(Anyptr);

    Functab->enter("sin",   Anyptr(expr_sin));
    Functab->enter("cos",   Anyptr(expr_cos));
    Functab->enter("tan",   Anyptr(expr_tan));
    Functab->enter("asin",  Anyptr(expr_asin));
    Functab->enter("acos",  Anyptr(expr_acos));
    Functab->enter("atan",  Anyptr(expr_atan));
    Functab->enter("atan2", Anyptr(expr_atan2));
    Functab->enter("abs",   Anyptr(expr_abs));
    Functab->enter("ceil",  Anyptr(expr_ceil));
    Functab->enter("floor", Anyptr(expr_floor));
    Functab->enter("exp",   Anyptr(expr_exp));
    Functab->enter("log",   Anyptr(expr_log));
    Functab->enter("log10", Anyptr(expr_log10));
    Functab->enter("rand",  Anyptr(expr_rand));
    Functab->enter("srand", Anyptr(expr_srand));
}

char *opname(int op) {
    static char s[] = " ";

    switch (op) {
	case UMINUS:	return "-";
	case '!':	return "!";
	case '~':	return "~";
	case AND:	return "&&";
	case OR:	return "||";
	case EQ:	return "==";
	case NE:	return "!=";
	case LE:	return "<=";
	case GE:	return ">=";
	default:	s[0] = op; return s;
    }
}

ostream &operator<<(ostream &o, Expression &e) {
    if (&e == NULL)
	return o;

    switch (e.op) {
	case NAME:
	    o << e.varname();
	    break;
	case FUNCTION:
	    o << e.funcname() << *e.funcargs();
	    break;
	case VALUE:
	    o << e.value();
	    break;
	case UMINUS:
	case '!':
	case '~':
	    o << opname(e.op) << *e.lchild();
	    break;
	default:
	    o << '(' << *e.lchild() << opname(e.op) << *e.rchild() << ')';
	    break;
    }
    return o;
}

Expression::Expression(int o, Expression *lop, Expression *rop)
{
    //PDEBUG(PD_MEMLEAK, memalloc("Expression::Expression(op,lop,rop)", this));
    PDEBUG(PD_EXPRESSION,
	   cerr << "Creating expression w/op " << o << "='" << (char)o << "'"
		<< " &lhs = " << (void *)lop << " &rhs = " << (void *)rop << endl);

    op = o;
    val.args[0] = lop;
    val.args[1] = rop;
}

// Create a function call node, or a named variable node if there are
//  no arguments.
Expression::Expression(const Name &name, List(Expression) *funcargs)
{
    //PDEBUG(PD_MEMLEAK, memalloc("Expression::Expression(name,funcargs)", this));
    if (funcargs == NULL) {
	PDEBUG(PD_EXPRESSION,
	       cerr << "Creating expression w/op " << NAME << " name " << name << endl);

	op = NAME;
	val.name.id = name;
	val.name.args = NULL;
    } else {
	PDEBUG(PD_EXPRESSION,
	       cerr << "Creating expression w/op " << FUNCTION << " function "
		    << name << *funcargs << endl);

	op = FUNCTION;
	val.name.id = name;
	val.name.args = funcargs;
    }
}

// Create a value node
// The passed pointer is not used.
Expression::Expression(Value *v)
{
    //PDEBUG(PD_MEMLEAK, memalloc("Expression::Expression(Value *)", this));
    PDEBUG(PD_EXPRESSION,
	   cerr << "Creating expression w/op " << VALUE << " value " << *v << endl);

    op = VALUE;
    *(Value *)&val.v = *v;
}

// Create a value node
Expression::Expression(Value &v)
{
    //PDEBUG(PD_MEMLEAK, memalloc("Expression::Expression(Value &)", this));
    PDEBUG(PD_EXPRESSION,
	   cerr << "Creating expression w/op " << VALUE << " value " << v << endl);

    op = VALUE;
    *(Value *)&val.v = v;
}

Expression::~Expression() {
    //PDEBUG(PD_MEMLEAK, memfree("Expression::~Expression()", this));
    switch (op) {
	case NAME:
	    PDEBUG(PD_EXPRESSION,
		   cerr << "Deleting expression::name (not dynamic)" << endl);
	    break;
	case FUNCTION:
	    PDEBUG(PD_EXPRESSION,
		   cerr << "Deleting expression::function "
			<< varname() << "args @ " << funcargs()
			<< endl);
	    delete funcargs();
	    break;
	case VALUE:
	    PDEBUG(PD_EXPRESSION,
		   cerr << "Deleting expression::value (not dynamic)" << endl);
	    break;
	default:
	    PDEBUG(PD_EXPRESSION,
		   cerr << "Deleting expression::op = " << op << " kids @ "
			<< (void *)lchild() << " @ " << (void *)rchild()
			<< endl);
	    delete lchild();
	    delete rchild();
	    break;
    }
}

Name Expression::name() {
    static Name bogus("[Not a Name]");

    return (op == NAME) ? varname() : bogus;
}

Value Expression::evaluate(Symtab(Value) &st) {
    Value v;
    Anyptr   p;

    switch (op) {

	case VALUE:
	    return value();

	case FUNCTION:
	    if (Functab == NULL)
		setup_functions();

	    if (Functab->lookup(funcname(), p) == true) {
		Exprfunc f;
		f = (Exprfunc)p;
		return (*f)(st, *funcargs());
	    } else {
		cerr << "Unimplemented function '" << funcname() << "'" << endl;
		return Value();
	    }
	    break;

	case NAME:
	    if (st.lookup(varname(), v))
		return v;
	    else {
		cerr << "Fatal error in Expression::evaluate: no bound variable '" << varname() << "'" << endl;
		return Value();
	    }
	    break;

	// Unary operators

	// Arithmetic
	case UMINUS: return -leval(st);

	// Bitwise complement
	case '~':    return ~leval(st);

	// Logical complement
	case '!':    return !leval(st);

	// Binary operators

	// Bitwise logical and, or
	case '&':    return leval(st) &  reval(st);
	case '|':    return leval(st) |  reval(st);

	// Logical and, or
	case AND:    return leval(st) && reval(st);
	case OR:     return leval(st) || reval(st);

	// Logical comparisons
	case EQ:     return leval(st) == reval(st);
	case NE:     return leval(st) != reval(st);
	case '<':    return leval(st) <  reval(st);
	case LE:     return leval(st) <= reval(st);
	case GE:     return leval(st) >= reval(st);
	case '>':    return leval(st) >  reval(st);

	// Arithmetic
	case '+':    return leval(st) +  reval(st);
	case '-':    return leval(st) -  reval(st);
	case '*':    return leval(st) *  reval(st);
	case '/':    return leval(st) /  reval(st);
	// Modulo
	case '%':    return leval(st) %  reval(st);
	// Power, *not* XOR as in C
	case '^':    return leval(st) ^  reval(st);

	// Shouldn't get here
	default:
	    cerr << "Fatal error in Expression::evaluate: unrecognized operator '"
		 << (char)op << "' = " << (int)op << endl;
	    return Value();
    }
}

// Bind symbolic names of the list to values in list 'values'
//  using symbol table st for evaluation and binding. The two
//  lists should conform() for this method to succeed.
// Returns true on success, false otherwise.
boolean bind(List(Expression) *formals,
	     List(Expression) *values,
	     Symtab(Value) &st) {

    // No binding need be done if one list is NULL
    if (formals == NULL || values == NULL)
	return true;

    if (conforms(formals, values) == false) {
	cerr << "bind: formal and value lists are not the same length" << endl;
	return false;
    }

    ListIterator(Expression) left(*formals), right(*values);
    Expression *lp, *rp;

    for (lp = left.first(), rp = right.first();
	 lp != NULL && rp != NULL;
	 lp = left.next(), rp = right.next()) {

	if (lp->type() != NAME) {
	    cerr << "bind: left expression " << *lp << " is not a formal" << endl;
	    return false;
	}
	if (rp->type() != VALUE) {
	    cerr << "bind: right expression " << *rp << " is not a value" << endl;
	    return false;
	}
	Value v = rp->evaluate(st);
	PDEBUG(PD_EXPRESSION, cerr << "Binding " << lp->name() << " = " << v << endl);
	st.enter(lp->name(), v);
    }

    return true;
}

// Check if list 'el' is conformant with the list, e.g.,
//  that they have the same number of expressions.
boolean conforms(List(Expression) *formals, List(Expression) *values) {
    int fsize = formals ? formals->size() : 0;
    int vsize = values ? values->size() : 0;
    return (fsize == vsize) ? true : false;
}

// Instantiate the list; that is, return a copy with all of the
//  expressions evaluated in the context of the symbol table.
// Return a NULL expression list if a NULL list is passed
List(Expression) *instantiate(List(Expression) *before, Symtab(Value) &st) {
    if (before == NULL)
	return NULL;

    List(Expression) *new_el = new List(Expression);
    ListIterator(Expression) ei(before);
    Expression *e;

    for (e = ei.first(); e; e = ei.next()) {
	Expression *new_e = new Expression(e->evaluate(st));
	new_el->append(new_e);
    }

    return new_el;
}

ListImplement(Expression,'(',')',',');

// Return the n'th (0 base) value in the list, if available.
// Evaluates the expression against specified symbol table; if the
//  the expression must be a bound value, supply an empty symbol table.
// Returns true on success, false if the list does not have enough
//  parameters or the parameter is not a number.
// This should be wrapped around a method of class List(Expression),
//  but that's a pain with the template method used.
boolean getvalue(
    Symtab(Value)    &st,
    List(Expression) &list,
    Value	     &v,
    unsigned int     n)
{
    // Ensure there are enough parameters
    if (n >= list.size())
	return false;

    ListIterator(Expression) ei(list);
    Expression	 *e;

    // Fetch the n'th expression in the list
    for (e = ei.first(); n > 0; n--)
	e = ei.next();

    // Should be an assertion
    if (e == NULL)
	return false;

    // Evaluate the expression. Since all expressions should have reduced
    //	to bound values by now, an empty symbol table is used.
    v = e->evaluate(st);

    return true;
}

boolean getfloat(
    Symtab(Value)    &st,
    List(Expression) &list,
    float	     &f,
    unsigned int     n)
{
    Value v;

    if (getvalue(st,list,v,n))
	return v.value(f);
    else
	return false;
}
