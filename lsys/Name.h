/* Name.h - class definition for space-efficient hashed names.
 *
 * $Id: Name.h,v 1.3 91/03/20 10:41:18 leech Exp Locker: leech $
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
 * $Log:	Name.h,v $
 * Revision 1.3  91/03/20  10:41:18  leech
 * Support for G++ bug (no automatically generated Name::Name(Name &)
 * constructor).
 * 
 * Revision 1.2  90/10/12  18:48:08  leech
 * First public release.
 *
 */

#ifndef NAME_H
#define NAME_H

#include "Symtab.h"

class Symtab(int);

class Name {
    static Symtab(int) *map;
    static char **reverse_map;
    static int	  next_index;
    static int	  reverse_map_size;

    int    index;
public:
    Name(char *);
    Name(int id) {
	if (id < 0 || id >= reverse_map_size)
	    index = 0;
	else
	    index = id;
    }
#ifdef __GNUG__
    Name(Name &n) {
	index = n.index;
    }
#endif
    operator int() const { return index; }
    operator const char *() const { return reverse_map[index]; }
};

inline int operator==(const Name &a, const Name &b) {
    return int(a) == int(b);
}

inline int operator!=(const Name &a, const Name &b) {
    return int(a) != int(b);
}

ostream &operator<<(ostream &o, const Name &n);

#endif /*NAME_H*/
