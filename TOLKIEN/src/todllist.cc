//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
//      This class is generated from GNU's generic DLList class.
//      The copyright notice is included below.
//
// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include <limits.h>
#include "tvalues.h"
#include "todllist.h"
#include "errhndl.h"

static  PCTypeInfo tobjlistBases[] = { &Collection::infoObj, 0 };
const TypeInfo  TObjDLList::infoObj("TObjDLList", tobjlistBases);

TObjDLList::TObjDLList(RCTObjDLList a, OwnerType dt) : curSz(a.curSz)
{
    ownsElements(dt);
    if (a.h != 0)
     {
        if (ownsElements()) {
           TObjDLListNode* p = a.h;
           TObjDLListNode* t = new TObjDLListNode(p->hd);
	   t->hd = p->hd->deepCopy();
           h = t;
           p = p->fd;
	   while (p != a.h)
           {
             TObjDLListNode* n = new TObjDLListNode(p->hd);
	     n->hd = p->hd->deepCopy();
             t->fd = n;
             n->bk = t;
             t = n;
             p = p->fd;
           }
	   t->fd = h;
	   h->bk = t;
        }
        else {
           TObjDLListNode* p = a.h;
           TObjDLListNode* t = new TObjDLListNode(p->hd);
           h = t;
           p = p->fd;
	   while (p != a.h)
           {
             TObjDLListNode* n = new TObjDLListNode(p->hd);
             t->fd = n;
	     n->bk = t;
             t = n;
             p = p->fd;
           }
           t->fd = h;
           h->bk = t;
        }
    }
    else
        h = 0;
}

sizeType TObjDLList::sizeOfList() const
{
  sizeType l = 0;
  TObjDLListNode* t = h;
  if (t != 0) do { ++l; t = t->fd; } while (t != h);
  return l;
}

void TObjDLList::deepenShallowCopy()
{
  if ( h == 0 || ownsElements() )
    return;

  TObjDLListNode* p = h->fd;

  while (p != 0)
  {
    TObjDLListNode* nxt = p->fd;
    p->hd = p->hd->deepCopy();
    p = nxt;
  }
  ownsElements(owner);
}

void TObjDLList::clear()
{
  if (h == 0)
    return;

  TObjDLListNode* p = h->fd;
  h->fd = 0;
  h = 0;

  if (delObj()) {
      while (p != 0)
      {
        TObjDLListNode* nxt = p->fd;
	delete p->hd;
	delete(p);
	p = nxt;
      }
  }
  else {
      while (p != 0)
      {
        TObjDLListNode* nxt = p->fd;
        delete(p);
        p = nxt;
      }
  }

  curSz = 0;
}


Pix TObjDLList::prepend(PTObject item)
{
  TObjDLListNode* t = new TObjDLListNode(item);

  curSz++;	// one more element
  if (h == 0)
    t->fd = t->bk = h = t;
  else
  {
    t->fd = h;
    t->bk = h->bk;
    h->bk->fd = t;
    h->bk = t;
    h = t;
  }
  return Pix(t);
}

Pix TObjDLList::append(PTObject item)
{
  TObjDLListNode* t = new TObjDLListNode(item);

  curSz++;	// one more element
  if (h == 0)
    t->fd = t->bk = h = t;
  else
  {
    t->bk = h->bk;
    t->bk->fd = t;
    t->fd = h;
    h->bk = t;
  }
  return Pix(t);
}

Pix TObjDLList::ins_after(Pix p, PTObject item)
{
  if (p == 0) return prepend(item);

  TObjDLListNode* u = (TObjDLListNode*) p;
  TObjDLListNode* t = new TObjDLListNode(item, u, u->fd);

  curSz++;	// one more element
  if (ownsElements())
      t->hd = item->deepCopy();
  u->fd->bk = t;
  u->fd = t;
  return Pix(t);
}

Pix TObjDLList::ins_before(Pix p, PTObject item)
{
  if (p == 0) error("null Pix");
  TObjDLListNode* u = (TObjDLListNode*) p;
  TObjDLListNode* t = new TObjDLListNode(item, u->bk, u);

  curSz++;	// one more element
  if (ownsElements())
      t->hd = item->deepCopy();
  u->bk->fd = t;
  u->bk = t;
  if (u == h) h = t;
  return Pix(t);
}

void TObjDLList::join(TObjDLList& b)
{
  TObjDLListNode* t = b.h;
  sizeType listSize = b.size();

  b.h = 0;
  if (h == 0)
    h = t;
  else if (t != 0)
  {
    TObjDLListNode* l = t->bk;
    h->bk->fd = t;
    t->bk = h->bk;
    h->bk = l;
    l->fd = h;
  }

  curSz += listSize;
}

int TObjDLList::owns(Pix p)
{
  TObjDLListNode* t = h;
  if (t != 0 && p != 0)
  {
    do
    {
      if (Pix(t) == p) return 1;
      t = t->fd;
    } while (t != h);
  }
  return 0;
}

void TObjDLList::del(Pix& p, int dir)
{
  if (p == 0) error("null Pix");
  TObjDLListNode* t = (TObjDLListNode*) p;
  if (t->fd == t)
  {
    h = 0;
    p = 0;
  }
  else
  {
    if (dir < 0)
    {
      if (t == h)
	p = 0;
      else
	p = Pix(t->bk);
    }
    else
    {
      if (t == h->bk)
	p = 0;
      else
	p = Pix(t->fd);
    }
    t->bk->fd = t->fd;
    t->fd->bk = t->bk;
    if (t == h) h = t->fd;
  }
  if (delObj())
      delete t->hd;
  delete t;

  curSz--;	// one element less
}

void TObjDLList::del_after(Pix& p)
{
  if (p == 0)
  {
    del_front();
    return;
  }

  TObjDLListNode* b = (TObjDLListNode*) p;
  TObjDLListNode* t = b->fd;

  if (b == t)
  {
    h = 0;
    p = 0;
  }
  else
  {
    t->bk->fd = t->fd;
    t->fd->bk = t->bk;
    if (t == h) h = t->fd;
  }
  if (delObj())
      delete t->hd;
  delete t;

  curSz--;	// one element less
}

PTObject TObjDLList::remove_front()
{
  if (h == 0)
    error("remove_front of empty list");
  TObjDLListNode* t = h;
  PTObject res = t->hd;
  if (h->fd == h)
    h = 0;
  else
  {
    h->fd->bk = h->bk;
    h->bk->fd = h->fd;
    h = h->fd;
  }
  delete t;
  curSz--;	// one element less
  return res;
}


void TObjDLList::del_front()
{
  if (h == 0)
    error("del_front of empty list");
  TObjDLListNode* t = h;
  if (h->fd == h)
    h = 0;
  else
  {
    h->fd->bk = h->bk;
    h->bk->fd = h->fd;
    h = h->fd;
  }
  if (delObj())
      delete t->hd;
  delete t;
  curSz--;	// one element less
}

PTObject TObjDLList::remove_rear()
{
  if (h == 0)
    error("remove_rear of empty list");
  TObjDLListNode* t = h->bk;
  PTObject res = t->hd;
  if (h->fd == h)
    h = 0;
  else
  {
    t->fd->bk = t->bk;
    t->bk->fd = t->fd;
  }
  delete t;
  curSz--;	// one element less

  return res;
}


void TObjDLList::del_rear()
{
  if (h == 0)
    error("del_rear of empty list");
  TObjDLListNode* t = h->bk;
  if (h->fd == h)
    h = 0;
  else
  {
    t->fd->bk = t->bk;
    t->bk->fd = t->fd;
  }
  if (delObj())
      delete t->hd;
  delete t;
  curSz--;	// one element less
}


int TObjDLList::OK()
{
  int v = 1;
  if (h != 0)
  {
    TObjDLListNode* t = h;
    long count = MAXLONG;      // Lots of chances to find h!
    do
    {
      count--;
      v &= t->bk->fd == t;
      v &= t->fd->bk == t;
      t = t->fd;
    } while (v && count > 0 && t != h);
    v &= count > 0;
  }
  if (!v) error("invariant failure");
  return v;
}

BOOL TObjDLList::isEqual( RCTObject obj ) const
{
        Pix p;
        BOOL found;

        if (this->isSame(obj))
            return TRUE;

        if ( obj.isA() != isA() )
            return FALSE;
        else {
                if ( ((RCollection) obj).size() != size() )
                    return FALSE;

                CollectionIterator iter((RCollection) obj);
                while ( iter ) {
                     for (p = first(), found = FALSE; p != NULL; next(p))
                          if (operator()(p)->isEqual(* iter())) {
                              found = TRUE;
                              break;
                          }
                     if (! found )
                         break;
                     ++iter;
                }
        }

        return found;
}

int      TObjDLList::compare(RCTObject obj) const
{
        if (this->isSame(obj))
            return 0;

        if ( obj.isA() == isA() ) {
                RCTObjDLList src = (RCTObjDLList) obj;
                Pix p = first(), q = src.first();
                int val;

                while (p && q) {
                    if ( (val = operator()(p)->compare(*src(q))) != 0 )
                        return val;
                    next(p);
                    src.next(q);
                }

                if ( ! (p || q) ) // both lists have equal length
                    return 0;
                else
                    if ( p && !q ) // this list is longer
                        return 1;
        }
        return -1;
}

PTObject TObjDLList::includes(RCTObject obj) const
{
	for (Pix p = first(); p != NULL; next(p))
             if (operator()(p)->isEqual(obj))
                 return operator()(p);
        return NOOBJECT;
}

unsigned TObjDLList::occurrencesOf(RCTObject obj) const
{
	unsigned res = 0;
	for (Pix p = first(); p != NULL; next(p))
	     if (operator()(p)->isEqual(obj)) res++;
        return res;
}

PTObject TObjDLList::remove(RCTObject obj)
{
        PTObject pObj = NOOBJECT;
	for (Pix p = first(); p != NULL; next(p))
	     if (operator()(p)->isEqual(obj)) {
		 pObj = operator()(p);
		 del(p);
		 break;
	     }
	return pObj;
}

PTObject TObjDLList::at(sizeType pos) const
{
	register sizeType count = pos;

	if (pos >= size())
	    return rear(); // return last element if pos out of bound

	for (Pix p = first(); count > 0; next(p), count--);

	return	operator()(p);
}

PTObject   TObjDLList::elem(sizeType pos) const
{
	register sizeType count = pos;

	if (pos >= size())
	    return rear(); // return last element if pos out of bound

	for (Pix p = first(); count > 0; next(p), count--);

	return	operator()(p);
}

void     TObjDLList::setAt(sizeType pos, PTObject pNewObj)
{
	register sizeType count = pos;

	if (pos >= size())
	    return; // index out of bound

	for (Pix p = first(); count > 0; next(p), count--);

	RPTObject       pRefObj = operator()(p);
	if (delObj())
	    delete pRefObj;

	pRefObj = pNewObj;
}

PTObject TObjDLList::insertAt(sizeType pos, PTObject pNewObj)
{
	register sizeType count = pos;
        Pix r;

        if (pos >= size())
	    // append element if pos out of bound
            r = append( pNewObj);
        else {
            for (Pix p = first(); count > 0; next(p), count--);
            r = ins_before(p, pNewObj);
        }
        if (r)
            return pNewObj;
        else
            return NOOBJECT;
}

PTObject TObjDLList::removeAt(sizeType pos)
{
        ASSERT(pos < size());

	register sizeType count = pos;
        PTObject pObj;

	for (Pix p = first(); count > 0; next(p), count--);
        pObj = operator()(p);
        del(p);
        return pObj;
}

void     TObjDLList::resize(sizeType newSize)
{
  if (newSize >= curSz)
      return;

  if (h == 0)
    return;

  TObjDLListNode *p = h->fd, *nxt;

  curSz = newSize;
  while (p != 0 && (newSize-- > 0) )
      {
	nxt = p->fd;
	p = nxt;
      }


  // delete all the remaining nodes

  if (delObj()) {
      while (p != 0)
      {
	nxt = p->fd;
	delete p->hd;
	delete(p);
	p = nxt;
      }
  }
  else {
      while (p != 0)
      {
	nxt = p->fd;
	delete(p);
	p = nxt;
      }
  }
}

