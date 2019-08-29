#ifndef _STREAMIO_H
#define _STREAMIO_H

#include <stream.h>

#ifdef __GNUG__

// ostream << void *
inline ostream &operator<< (ostream &o, void *p) {
    return o << "0x" << hex(long(p));
}

// Manipulators 'endl' and 'flush'
inline ostream &operator<<(ostream &o, ostream &(*manip)(ostream &)) {
    return (*manip)(o);
}

#if 0
// Manipulators 'setw' and 'setprecision'
inline ostream &operator<<(ostream &o, ostream &(*manip)(ostream &,int)) {
    return (*manip)(o);
}
#endif

inline ostream &endl(ostream &o) { o << '\n'; return o.flush(); }
inline ostream &flush(ostream &o) { return o.flush(); }

// Manipulators not supported by old stream library
#define setw(width) ""
#define setprecision(precision) ""
#define setiosflags(flags) ""

// ios class for ios:: flags
struct ios {
    enum { beg = 0 };		// 'whence' in lseek()
    enum { fixed, showpoint };	// formatting flags
};

typedef long streamoff;

// ofstream w/file name constructor and seekp() method
class ofstream : public ostream {
public:
    ofstream(char *file) : (new Filebuf(file, io_writeonly, a_create)) { }
    long seekp(streamoff offset, int whence) {
	// This is type-unsafe, but we know the ofstream was initialized with
	//  a Filebuf.
	Filebuf *fb = (Filebuf *)bp;
	File *fp = fb->Fp;
	fp->seek(offset, whence);
	return fp->tell();
    }
};

#endif /*__GNUG__*/

#endif /*_STREAMIO_H*/
