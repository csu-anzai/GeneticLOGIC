/* List.c - methods for lists of specific object types.
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
 * $Log:	List.c,v $
 * Revision 1.2  90/10/12  18:48:05  leech
 * First public release.
 * 
 */
static char RCSid[] = "$Id: List.c,v 1.2 90/10/12 18:48:05 leech Exp Locker: leech $";

#include "List.h"

List_::List_() {
    head = tail = NULL;
    ptr = NULL;
    count = 0;
}

List_::~List_() {
}

// Clear all items from a list
void List_::clear() {
    head = tail = NULL;
    ptr = NULL;
    count = 0;
}

void List_::append(void *lp) {
    Link *link = new Link(lp);

    // Thread the new item onto the end of the list
    link->prev = tail;

    // if tail is non-NULL, append new item to the end
    if (tail) {
	tail->next = link;
	tail = link;
    } else
	head = tail = link;
    count++;
}

void List_::append(List_ *lp) {
    // Check for append to self
    if (this == lp) {
	cerr << "List::append(List *): cannot append a List to itself!" << endl;
	return;
    }

    // If appending an empty list, just return
    if (lp->tail == NULL)
	return;

    // If append to an empty list, just copy the appended list
    if (tail == NULL)
	*this = *lp;
    else {
	// Thread the new list
	tail->next = lp->head;
	lp->head->prev = tail;
	tail = lp->tail;
	count += lp->count;
    }

    // Remove entries from the appended list (could delete lp here)
    lp->clear();
}

ostream &operator<<(ostream &o, List_ &l) {
    if (&l == NULL)
	return o;

    List_Iterator li(&l);
    o << '(';
    for (void *lp = li.first(); lp != NULL; lp = li.next()) {
	o << (void *)lp << ' ';
    }
    o << ")\n";

    return o;
}
