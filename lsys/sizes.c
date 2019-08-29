static char RCSid[] = "$Id: sizes.c,v 1.1 91/03/20 21:13:46 leech Exp Locker: leech $";

#include <osfcn.h>
#ifndef __GNUG__
#include <libc.h>
#else
#include <std.h>
#endif /*__GNUG__*/
#include <new.h>
#include <time.h>
#include <sys/types.h>
#include <sys/times.h>

#include "global_headers.h"
#include "vector.h"
#include "parser.h"
#include "interpret.h"

Symtab(Value)	 IgnoreTable,	    // modules to ignore in context matching
		 SymbolTable;	    // bound variables for expressions
List(Production) Rules;		    // productions
List(Module)	*Start;		    // initial string

int ParseDebug = 0;

void out_of_memory() {
    cerr << "FATAL: no free memory!" << endl;
    exit(1);
}

#include <malloc.h>

main(int argc, char *argv[])
{
#ifdef M_MXFAST
    cout << "mallopt() is available to this program\n";
    if (argc > 1) {
	cout << "Setting mallopt(M_MXFAST, 32)\n";
	mallopt(M_MXFAST, 32);
    }
#endif

    set_new_handler(out_of_memory);

    // Find sizes of each type of data structure
#define CHECK(type,typename) {\
    type *p1, *p2;							    \
    cout << setw(24) << typename << " : sizeof = " << sizeof(type) << endl; \
    p1 = (type *)malloc(sizeof(type));					    \
    p2 = (type *)malloc(sizeof(type));					    \
    cout << setw(24) << " " << " : stride = " << (char *)p2 - (char *)p1 << endl;	    \
}

    CHECK(Name		      ,"Name");
    CHECK(Value		      ,"Value");
    CHECK(Expression	      ,"Expression");
    CHECK(Module	      ,"Module");
    CHECK(Link		      ,"Link");
    CHECK(List(Module)	      ,"List(Module)");
    CHECK(ListIterator(Module),"ListIterator(Module)");
    CHECK(Symbol(Value)       ,"Symbol(Value)");
    CHECK(Symtab(Value)       ,"Symtab(Value)");
    CHECK(Predecessor	      ,"Predecessor");
    CHECK(Successor	      ,"Successor");
    CHECK(Production	      ,"Production");

    ParseDebug = PD_MEMLEAK;

    memlog_enable(true);


    cout << "\n\nNow let's see what happens with overloaded operator new/delete\n";

    class MemHog {
	int	x[2];

    public:
	void *operator new(size_t s) { return ::operator new(s,"MemHog"); }
	DELETE_OPERATOR(MemHog,"MemHog");
    };

    cout << "Allocating MemHog\n";
    MemHog *mp = new MemHog;
    cout << "Deleting MemHog @" << (void *)mp << '\n';
    delete mp;


    cout << "\n\nAllocating then deleting Name, Value, Expression, Elist, Module, Mlist\n";

#define alloc_msg(name,ptr) \
    cout << "Allocating " << name << "\t @" << (void *)ptr << '\n';

    Name *n = new Name(0);
    alloc_msg("Name", n);

    Value *v = new Value();
    alloc_msg("Value", v);

    Expression *e = new Expression(v);
    alloc_msg("Expression", e);

    List(Expression) *elist = new List(Expression);
    alloc_msg("List(Expression)", elist);

    Module *m = new Module(*n, elist);
    alloc_msg("Module", m);

    List(Module) *mlist = new List(Module);
    alloc_msg("List(Module)", mlist);

#define delete_msg(name,ptr) \
    cout << "Deleting  " << name << "\t @" << (void *)ptr << '\n'; \
    delete ptr;

    delete_msg("Name", n);
    delete_msg("Value", v);
    delete_msg("Expression", e);
    delete_msg("Module", m);
    delete_msg("List(Module)", mlist);
    delete_msg("List(Expression)", elist);

    cout << "Done allocating and deleting\n";

    memlog_enable(false);
}
