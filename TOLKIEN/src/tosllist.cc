//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
//      This class is generated from GNU's generic SLList class.
//      The copyright notice is included below.
//
// This may look like C code, but it is really -*- C++ -*-
// WARNING: This file is obsolete.  Use ../SLList.cc, if you can.
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
#include <values.h>
#include <iostream.h>
#include <builtin.h>
#include "tosllist.h"

static  PCTypeInfo tosllistBases[] = { &Collection::infoObj, 0 };
const TypeInfo  TObjSLList::infoObj("TObjSLList", tosllistBases);

TObjSLList::TObjSLList(RCTObjSLList a, OwnerType dt) : curSz(a.curSz)
{
  ownsElements(dt);
  if (a.last == 0)
    last = 0;
  else
  {
    if (ownsElements()) {
          TObjSLListNode* p = a.last->tl;
          TObjSLListNode* h = new TObjSLListNode(p->hd);
          h->hd = p->hd->deepCopy();
          last = h;
          for (;;)
          {
            if (p == a.last)
            {
              last->tl = h;
              return;
            }
            p = p->tl;
            TObjSLListNode* n = new TObjSLListNode(p->hd);
	    n->hd = p->hd->deepCopy();
            last->tl = n;
            last = n;
          }
    }
    else {
         TObjSLListNode* p = a.last->tl;
         TObjSLListNode* h = new TObjSLListNode(p->hd);
         last = h;
         for (;;)
         {
           if (p == a.last)
	   {
	     last->tl = h;
	     return;
	   }
	   p = p->tl;
	   TObjSLListNode* n = new TObjSLListNode(p->hd);
	   last->tl = n;
	   last = n;
	 }
    }
  }
}

void TObjSLList::clear()
{
  if (last == 0)
    return;

  TObjSLListNode* p = last->tl;
  last->tl = 0;
  last = 0;

  if (delObj())
      while (p != 0)
      {
        TObjSLListNode* nxt = p->tl;
        delete p->hd;
	delete(p);
        p = nxt;
      }
  else
      while (p != 0)
      {
        TObjSLListNode* nxt = p->tl;
	delete(p);
        p = nxt;
      }
  curSz = 0;
}


Pix TObjSLList::prepend(PTObject  item)
{
  TObjSLListNode* t = new TObjSLListNode(item);
  curSz++;	// one more element
  if (last == 0)
    t->tl = last = t;
  else
  {
    t->tl = last->tl;
    last->tl = t;
  }
  return Pix(t);
}


Pix TObjSLList::prepend(TObjSLListNode* t)
{
  if (t == 0) return 0;
  curSz++;	// one more element
  if (last == 0)
    t->tl = last = t;
  else
  {
    t->tl = last->tl;
    last->tl = t;
  }
  return Pix(t);
}


Pix TObjSLList::append(PTObject  item)
{
  TObjSLListNode* t = new TObjSLListNode(item);
  curSz++;	// one more element
  if (last == 0)
    t->tl = last = t;
  else
  {
    t->tl = last->tl;
    last->tl = t;
    last = t;
  }
  return Pix(t);
}

Pix TObjSLList::append(TObjSLListNode* t)
{
  if (t == 0) return 0;
  curSz++;	// one more element
  if (last == 0)
    t->tl = last = t;
  else
  {
    t->tl = last->tl;
    last->tl = t;
    last = t;
  }
  return Pix(t);
}

void TObjSLList::join(TObjSLList& b)
{
  TObjSLListNode* t = b.last;
  sizeType listSize = b.size();

  b.last = 0;
  if (last == 0)
    last = t;
  else if (t != 0)
  {
    TObjSLListNode* f = last->tl;
    last->tl = t->tl;
    t->tl = f;
    last = t;
  }
  curSz += listSize;
}

Pix TObjSLList::ins_after(Pix p, PTObject  item)
{
  TObjSLListNode* u = (TObjSLListNode*)p;
  TObjSLListNode* t = new TObjSLListNode(item);
  curSz++;	// one more element
  if (last == 0)
    t->tl = last = t;
  else if (u == 0) // ins_after 0 means prepend
  {
    t->tl = last->tl;
    last->tl = t;
  }
  else
  {
    t->tl = u->tl;
    u->tl = t;
    if (u == last) 
      last = t;
  }
  return Pix(t);
}


void TObjSLList::del_after(Pix p)
{
  TObjSLListNode* u = (TObjSLListNode*)p;
  if (last == 0 || u == last) error("cannot del_after last");
  if (u == 0) u = last; // del_after 0 means delete first
  TObjSLListNode* t = u->tl;
  if (u == t)
    last = 0;
  else
  {
    u->tl = t->tl;
    if (last == t)
      last = u;
  }
  if (delObj())
      delete t->hd;
  delete t;

  curSz--;	// one element less
}

int TObjSLList::owns(Pix p) const
{
  TObjSLListNode* t = last;
  if (t != 0 && p != 0)
  {
    do
    {
      if (Pix(t) == p) return 1;
      t = t->tl;
    } while (t != last);
  }
  return 0;
}

PTObject TObjSLList::remove_front()
{
  if (last == 0) error("remove_front of empty list");
  TObjSLListNode* t = last->tl;
  PTObject res = t->hd;
  if (t == last)
    last = 0;
  else
    last->tl = t->tl;
  delete t;
  curSz--;	// one element less
  return res;
}

int TObjSLList::remove_front(RPTObject x)
{
  if (last == 0)
    return 0;
  else
  {
    TObjSLListNode* t = last->tl;
    x = t->hd;
    if (t == last)
      last = 0;
    else
      last->tl = t->tl;
    delete t;
    curSz--;
    return 1;
  }
}


void TObjSLList::del_front()
{
  if (last == 0) error("del_front of empty list");
  TObjSLListNode* t = last->tl;
  if (t == last)
    last = 0;
  else
    last->tl = t->tl;
  if (delObj())
      delete t->hd;
  delete t;
  curSz--;	// one element less
}

int TObjSLList::OK()
{
  int v = 1;
  if (last != 0)
  {
    TObjSLListNode* t = last;
    long count = MAXLONG;      // Lots of chances to find last!
    do
    {
      count--;
      t = t->tl;
    } while (count > 0 && t != last);
    v &= count > 0;
  }
  if (!v) error("invariant failure");
  return v;
}

BOOL TObjSLList::isEqual( RCTObject obj ) const
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

int      TObjSLList::compare(RCTObject obj) const
{
        if (this->isSame(obj))
            return 0;

        if ( obj.isA() == isA() ) {
                RCTObjSLList src = (RCTObjSLList) obj;
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

PTObject TObjSLList::includes(RCTObject obj) const
{
	for (Pix p = first(); p != NULL; next(p))
	     if (operator()(p)->isEqual(obj))
		 return operator()(p);
        return NOOBJECT;
}

unsigned TObjSLList::occurrencesOf(RCTObject obj) const
{
	unsigned res = 0;
	for (Pix p = first(); p != NULL; next(p))
	     if (operator()(p)->isEqual(obj)) res++;
	return res;
}

PTObject TObjSLList::remove(RCTObject obj)
{
        PTObject pObj = NOOBJECT;
	Pix prev, cur;
	cur = first();
	if (operator()(cur)->isEqual(obj))
	    remove_front(pObj);
	else {
	    prev = cur;
	    for (next(cur); cur != NULL; next(cur))
	     if (operator()(cur)->isEqual(obj)) {
		 pObj = remove_after(prev);
		 break;
	     }
	     else
		 prev = cur;
	}
	return pObj;
}

PTObject TObjSLList::at(sizeType pos) const
{
	register sizeType count = pos;

	if (pos >= size())
	    return rear(); // return last element if pos out of bound

	for (Pix p = first(); count > 0; next(p), count--);

	return	operator()(p);
}

PTObject   TObjSLList::elem(sizeType pos) const
{
	register sizeType count = pos;

	if (pos >= size())
	    return rear(); // return last element if pos out of bound

	for (Pix p = first(); count > 0; next(p), count--);

	return	operator()(p);
}

void     TObjSLList::setAt(sizeType pos, PTObject pNewObj)
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

PTObject TObjSLList::insertAt(sizeType pos, PTObject pNewObj)
{
        register sizeType count = pos - 1;
        Pix r;

	if (pos >= size())
	    // append element if pos out of bound
            r = append( pNewObj);
        else
            if (pos == 0)
                r = prepend(pNewObj);
        else {
            for (Pix p = first(); count > 0; next(p), count--);
            r = ins_after(p, pNewObj);
        }
        if (r)
	    return pNewObj;
	else
            return NOOBJECT;
}

PTObject TObjSLList::removeAt(sizeType pos)
{
	ASSERT(pos < size());

	register sizeType count = pos;
        PTObject pObj = NOOBJECT;
	Pix cur, prev;

	if (pos == 0)
	    remove_front(pObj);
	else {
	    cur = prev = first();
	    for (count--,next(cur);
		 count > 0;
		 next(cur), count--);
		 prev = cur;
	    pObj = remove_after(prev);
	}
	return pObj;
}

void     TObjSLList::resize(sizeType newSize)
{
  if (newSize >= curSz)
      return;
  else
      if (last == 0)
	  return;

  TObjSLListNode *nxt = last->tl;
  sizeType numToDel = curSz - newSize;

  curSz = newSize;
  if (delObj())
      while (nxt != 0 && (numToDel-- > 0) ) {
	    delete last->hd;
	    delete last;
	    last = nxt;
	    nxt = nxt->tl;
      }
  else
      while (nxt != 0 && (numToDel-- > 0) ) {
	    delete last;
	    last = nxt;
	    nxt = nxt->tl;
      }
}

PTObject TObjSLList::remove_after(Pix p)
{
  PTObject pObj = NOOBJECT;

  TObjSLListNode* u = (TObjSLListNode*)p;
  if (last == 0 || u == last) error("cannot remove_after last");
  if (u == 0) u = last; // del_after 0 means delete first
  TObjSLListNode* t = u->tl;
  if (u == t)
    last = 0;
  else
  {
    u->tl = t->tl;
    if (last == t)
      last = u;
  }

  pObj = t->hd;

  delete t;

  curSz--;	// one element less

  return pObj;
}

sizeType TObjSLList::sizeOfList() const
{
  sizeType l = 0;
  TObjSLListNode* t = last;
  if (t != 0) do { ++l; t = t->tl; } while (t != last);
  return l;
}

void TObjSLList::deepenShallowCopy()
{
  if ( last == 0 || ownsElements() )
    return;

  TObjSLListNode* p = last->tl;

  while (p != 0)
  {
    TObjSLListNode* nxt = p->tl;
    p->hd = p->hd->deepCopy();
    p = nxt;
  }
  ownsElements(owner);
}
