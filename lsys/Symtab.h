/* Symtab.h - class definition for symbol tables of specific object types.
 *
 * $Id: Symtab.h,v 1.3 91/03/20 10:39:52 leech Exp Locker: leech $
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
 * $Log:	Symtab.h,v $
 * Revision 1.3  91/03/20  10:39:52  leech
 * Changed type of Anyptr from void * to void (*)() since function pointers
 * may be larger than data pointers.
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:13  leech
 * First public release.
 *
 */

#ifndef SYMTAB_H
#define SYMTAB_H

#include "streamio.h"
#include <string.h>
#include "boolean.h"
#include "debug.h"
#include "List.h"

#define Symbol(type) name2(type,Symbol)
#define Symtab(type) name2(type,Symtab)

#include "Name.h"
extern "C" char *strdup(const char *s);

#define SymtabDeclare(type) \
class Symbol(type) {						\
    char *tag;							\
    type  value;						\
public:								\
    Symbol(type)(const char *name, const type &v) {		\
	tag = strdup(name);					\
	value = v;						\
    }								\
    void *operator new(size_t s) { return ::operator new(s,"Symbol(type)"); }  \
    DELETE_OPERATOR(Symbol(type),"Symbol(type)");		\
   ~Symbol(type)() { delete tag; }				\
    friend ostream &operator<<(ostream &o, Symbol(type) &s);	\
    friend class Symtab(type);					\
};								\
								\
ListDeclare(Symbol(type));					\
								\
class Symtab(type) {						\
    List(Symbol(type)) head;					\
    Symbol(type) *search(const char *name);			\
public:								\
   /* Symtab(type)(); */					\
   /*void *operator new(size_t s) { return ::operator new(s,"Symtab(type)"); } */ \
   /*void operator delete(void *p, size_t s) { delete_sz(p,s,"Symtab(type)"); }*/ \
   /* ~Symtab(type)(); */					\
    boolean enter(const char *name, const type &v);		\
    boolean lookup(const char *name, type &v);			\
    friend ostream &operator<<(ostream &o, Symtab(type) &s);	\
};

#define SymtabImplement(type,start,end,separator) \
ostream &operator<<(ostream &o, Symbol(type) &s) {		\
    o << '(' << s.tag << " = ";					\
    return o << s.value << ')';					\
}								\
								\
/* search for a symbol by name; returns a pointer to it		\
 *  or NULL if not in the table.				\
 */								\
Symbol(type) *Symtab(type)::search(const char *name) {		\
    ListIterator(Symbol(type)) li(&head);			\
    Symbol(type) *s;						\
								\
    for (s = li.first(); s && strcmp(s->tag, name); s = li.next()) \
	;							\
								\
    return s;							\
}								\
								\
/* enter a symbol; returns true if already present in the table,\
 *  false otherwise.						\
 */								\
boolean Symtab(type)::enter(const char *name, const type &v) {	\
    Symbol(type) *s = search(name);				\
								\
    if (s) {							\
	s->value = v;						\
	return true;						\
    } else {							\
	s = new Symbol(type)(name, v);				\
	head.append(s);						\
	return false;						\
    }								\
}								\
/* look up a symbol; returns symbol value in v. Returns true if \
 * in the table, false if not (v is unmodified in this case).	\
 */								\
boolean Symtab(type)::lookup(const char *name, type &v) {	\
    Symbol(type) *s = search(name);				\
								\
    if (s) {							\
	v = s->value;						\
	return true;						\
    } else							\
	return false;						\
}								\
ostream &operator<<(ostream &o, Symtab(type) &s) {		\
    return o << s.head << '\n';					\
}								\
ListImplement(Symbol(type),start,end,separator)

/* Symbol tables of simple types (function pointer and int) */
typedef void (*Anyptr)();

SymtabDeclare(Anyptr);
SymtabDeclare(int);

#endif /*SYMTAB_H*/
