// Test member operator delete(void *).
// g++ 1.39.0 beta passes NULL as the parameter if there's a destructor.
// This results in enormous memory lossage in lsys.

#ifdef __GNUG__
#include <std.h>
#else
#include <libc.h>
#include <stdlib.h>
#endif
#include <stream.h>

void delete_sz(void *p, size_t sz, char *label) {
    if (p == NULL)
	cout << "operator delete() was (incorrectly) passed NULL\n";
    else
	cout << "operator delete() appears to be working OK\n";
    free((char *)p);
}

struct Module {
   ~Module() { }
    void operator delete(void *p) {
	delete_sz(p,sizeof(Module),"Module");
    }
};

main() {
    Module *m = new Module;
    delete m;
}
