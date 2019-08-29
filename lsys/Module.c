/* Module.c - methods for handling L-system modules.
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
 * $Log:	Module.c,v $
 * Revision 1.3  91/03/20  10:35:38  leech
 * Bug fix in module printing.
 * 
 * Revision 1.2  90/10/12  18:48:06  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: Module.c,v 1.3 91/03/20 10:35:38 leech Exp Locker: leech $";

#include "global_headers.h"
#include "Symtab.h"
#include "Value.h"
#include "Expression.h"
#include "Module.h"

// These Names are used in context matching to ascend/descend tree levels
Name
    LBRACKET("["),
    RBRACKET("]");

Module::Module(
    const Name &n,
    List(Expression) *elist,
    boolean ignore) : tag(n) {

    //PDEBUG(PD_MEMLEAK, memalloc("Module::Module(name,elist,ignore)", this));
    param = elist;
    ignoreflag = (ignore == true);
    emptyflag = 0;

    PDEBUG(PD_MODULE,
	   cerr << "Creating module " << *this << " @ " << (void *)this << endl);
}

Module::Module(Module &m) : tag(m.tag) {
    //PDEBUG(PD_MEMLEAK, memalloc("Module::Module(Module &)", this));
    param = m.param;
    ignoreflag = m.ignoreflag;
    emptyflag = 0;

    PDEBUG(PD_MODULE, cerr << "Copying module " << *this << endl);
}

Module::~Module() {
    //PDEBUG(PD_MEMLEAK, memfree("Module::~Module()", this));
    PDEBUG(PD_MODULE,
	   cerr << "Deleting module @ " << (void *)this << endl);

    if (emptyflag == 0)
	delete param;
}

// Don't delete parameter list when destructor called, even if it's
//  dynamically allocated.
void Module::empty() {
    emptyflag = 1;
}

// Bind symbolic names of the module to values in module 'values'
//  using symbol table st for evaluation and binding. The two
//  modules should conform() for this method to succeed.
boolean Module::bind(Module &values, Symtab(Value) &st) {
    PDEBUG(PD_MODULE, cerr << "Module::bind: formals = " << *this
			   << " values = " << values << endl);

    if (::bind(param, values.param, st) == false) {
	cerr << "failure binding module " << values << " to " << *this << endl;
	return false;
    } else
	return true;
}

// Check if module 'm' is conformant with the module, e.g.,
//  that they have the same name and their expression lists are
//  conformant.
boolean Module::conforms(Module &m) {
    if (tag != m.tag)
	return false;

    return ::conforms(param, m.param);
}

// Instantiate the module; that is, return a copy with all of the
//  module's expressions evaluated in the context of the symbol table.
Module *Module::instantiate(Symtab(Value) &st) {
    List(Expression) *el = ::instantiate(param, st);
    Module *new_m = new Module(tag, el, ignoreflag ? true : false);

    PDEBUG(PD_MODULE,
	   cerr << "Module::instantiate: " << *this << " @ " << (void *)this
		<< " -> " << *new_m << " @ " << (void *) new_m << endl
	  );
    PDEBUG(PD_MODULE, cerr << "        old elist: " << *el << endl);

    return new_m;
}

// Return the n'th (0 base) parameter of module m in f, if available.
// Returns true on success, false if m does not have enough parameters
//  or the parameter is not a number.
boolean Module::getfloat(float &f, unsigned int n) {
    if (param) {
	// An empty symbol table used to ensure the argument is a bound value.
	static Symtab(Value) st;

	return ::getfloat(st, *param, f, n);
    } else
	return false;
}

ostream &operator<<(ostream &o, Module &m) {
    if (&m == NULL)
	return o;

    o << Name(m.tag);
    if (m.param != NULL && m.param->size() > 0)
	o << *m.param;
    return o;
}

ListImplement(Module,"","",' ');
