/* List.h - class definition for lists of specific object types.
 *
 * $Id: List.h,v 1.3 91/03/20 10:39:30 leech Exp Locker: leech $
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
 * $Log:	List.h,v $
 * Revision 1.3  91/03/20  10:39:30  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:06  leech
 * First public release.
 *
 */

#ifndef LIST_H
#define LIST_H

#include "streamio.h"
#include <generic.h>
#include "debug.h"

struct Link {
    Link *prev, *next;
    void *obj;

    Link(void *p) {
	prev = next = NULL; obj = p;
    }

    // The destructor cannot delete obj, since it's a pointer to
    //	an object of unknown type. This task falls on the
    //	destructor for derived Link_ classes. So the default
    //	destructor suffices.
    // Ideally, this would be a private class with friend classes
    //	being all classes derived from List_ and List_Iterator.
    // There is no way to specify this, unfortunately.
    void *operator new(size_t s) { return ::operator new(s,"Link"); }
    DELETE_OPERATOR(Link,"Link");
};

class List_ {
protected:
    Link *head, *tail;
    Link *ptr;
    int   count;

    void  clear();
public:
    List_();

    void *operator new(size_t s) { return ::operator new(s,"List_"); }
    DELETE_OPERATOR(List_,"List_");
    // Delete all items on the list
   ~List_();

    int   size() { return count; }

    // Append a dynamically allocated object
    void  append(void *);

    // Append a dynamically allocated list; deletes the argument list
    //	but not its contents.
    void  append(List_ *);

    friend ostream &operator<<(ostream &o, List_ &l);
    friend class List_Iterator;
};

class List_Iterator {
    List_ *list;	/* List being iterated */
    Link  *ptr;		/* Pointer to current data item */

public:
    List_Iterator(List_ *l) {
	list = l;
	ptr = NULL;
    }

    void *first()    { return (ptr = list->head) ? ptr->obj : NULL; }
    void *next()     { if (ptr) ptr = ptr->next; return ptr ? ptr->obj : NULL; }
    void *previous() { if (ptr) ptr = ptr->prev; return ptr ? ptr->obj : NULL; }
    void *last()     { return (ptr = list->tail) ? ptr->obj : NULL; }
};

#define List(type) name2(type,List)
#define ListIterator(type) name2(type,ListIterator)

#define ListDeclare(type) \
class List(type) : public List_ {					\
public:									\
    List(type)() : List_() { }						\
    void *operator new(size_t s) { return ::operator new(s,"List(type)"); } \
    DELETE_OPERATOR(List(type),"List(type)");				\
   ~List(type)();							\
    void append(type *t) { List_::append(t); }				\
    void append(List(type) *l) { List_::append((List_ *)l); }		\
    friend ostream &operator<<(ostream &o, List(type) &l);		\
};									\
									\
class ListIterator(type) : public List_Iterator {			\
public:									\
    ListIterator(type)(List(type) *l) : List_Iterator(l) { }		\
    ListIterator(type)(List(type) &l) : List_Iterator(&l) { }		\
    void *operator new(size_t s) { return ::operator new(s,"Iter(type)"); } \
    DELETE_OPERATOR(ListIterator(type),"Iter(type)");			\
    type *first() { return (type *)List_Iterator::first(); }		\
    type *next() { return (type *)List_Iterator::next(); }		\
    type *previous() { return (type *)List_Iterator::previous(); }	\
    type *last() { return (type *)List_Iterator::last(); }		\
};

/*
 * start is starting string to bracket a list (usually "" or '[')
 * end is ending string			      (usually "" or ']')
 * separator is placed between list elements  (usually ' ')
 */
#define ListImplement(type,start,end,separator) \
/* Delete all items on the list, then delete the list */		\
List(type)::~List(type)() {						\
    Link *p;								\
									\
    PDEBUG(PD_MEMLEAK, memfree("List(type)::~List(type)", this));      \
    while (head != NULL) {						\
	/* Delete the object pointed to by the link */			\
	delete (type *)head->obj;					\
	p = head;							\
	head = head->next;						\
	/* Delete the Link */						\
	delete p;							\
    }									\
}									\
									\
extern ostream &operator<<(ostream &o, type &l);			\
ostream &operator<<(ostream &o, List(type) &l) {			\
    if (&l == NULL)							\
	return o;							\
									\
    ListIterator(type) li(&l);						\
    type *p, *tail = li.last();						\
									\
    o << start;								\
    for (p = li.first(); p; p = li.next()) {				\
	o << *p;							\
	if (p != tail)							\
	    o << separator;						\
    }									\
    o << end;								\
									\
    return o;								\
}
#endif /*LIST_H*/
