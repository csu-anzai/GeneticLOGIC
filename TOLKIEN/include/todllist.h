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


#ifndef _TObjDLList_h
#ifdef __GNUG__
#pragma interface
#endif
#define _TObjDLList_h 1

#include <Pix.h>
#include "toptrdef.h"
#include "collectn.h"
#include "errhndl.h"
#include "tvalues.h"

#ifndef _TObjDLListNode_h
#define _TObjDLListNode_h 1

struct TObjDLListNode
{
  TObjDLListNode*         bk;
  TObjDLListNode*         fd;
  PTObject              hd;
                         TObjDLListNode();
                         TObjDLListNode(PTObject h,
                                     TObjDLListNode* p = 0,
                                     TObjDLListNode* n = 0);
                         ~TObjDLListNode();
};

inline TObjDLListNode::TObjDLListNode() {}

inline TObjDLListNode::TObjDLListNode(PTObject h, TObjDLListNode* p,
                                TObjDLListNode* n)
  :bk(p), fd(n), hd(h){ }

inline TObjDLListNode::~TObjDLListNode() { }

typedef TObjDLListNode* TObjDLListNodePtr;

#endif

class TObjDLList : public Collection
{
public:

        DECLARE_RTTI()

                                TObjDLList(OwnerType dt = reference);
                                TObjDLList(RCTObjDLList,
                                           OwnerType dt = reference);
                                ~TObjDLList();

//        virtual int             empty() const;

                                _shallowCopy(TObjDLList)
        virtual void            deepenShallowCopy();
        virtual int             compare(RCTObject) const;

	virtual BOOL            add(PTObject pObj);
	virtual PTObject        at(sizeType) const;
	virtual PTObject        elem(sizeType) const;
	virtual void            setAt(sizeType, PTObject);
	virtual PTObject        insertAt(sizeType, PTObject);
	virtual PTObject        remove(RCTObject);
        virtual PTObject        removeAt(sizeType);
	virtual void            removeAll();

	virtual BOOL            isEmpty() const;
        virtual BOOL            isEqual( RCTObject ) const;
	virtual unsigned        occurrencesOf(RCTObject) const;
        virtual PTObject        includes(RCTObject) const;
	virtual sizeType        size() const;
	virtual void            resize(sizeType);
	virtual PIterator       initIterator() const;

        //      GNU's DLList functions
                void            clear();
                Pix             prepend(PTObject item);
                Pix             append(PTObject item);
                void            join(TObjDLList&);
                RPTObject       front() const;
                PTObject        remove_front();
                void            del_front();
                RPTObject       rear() const;
                PTObject        remove_rear();
                void            del_rear();
                RPTObject       operator () (Pix p) const;
                Pix             first() const;
                Pix             last() const;
                void            next(Pix& p) const;
                void            prev(Pix& p) const;
                int             owns(Pix p);
                Pix             ins_after(Pix p, PTObject item);
                Pix             ins_before(Pix p, PTObject item);
                void            del(Pix& p, int dir = 1);
                void            del_after(Pix& p);
                int             OK();

protected:

	virtual sizeType	sizeOfList() const;

        TObjDLListNode*           h;
	sizeType		curSz;

};

class TObjDLListIterator : public Iterator
{
public:

			TObjDLListIterator(RCTObjDLList);
			TObjDLListIterator(RCTObjDLListIterator);

    virtual PTObject    operator ()();
    virtual Pix         curPix();

#if defined ( __postfix_inc__ )
    virtual PTObject    operator ++ ( int );
#endif

    virtual PTObject    operator ++ ();
    virtual void        restart();
    virtual void        restart(RCTObjDLList);
    virtual 		operator int();
    virtual sizeType	indexOf() const;
    virtual PIterator   deepCopy() const;

protected:

    PCTObjDLList        pList;
    Pix                 cur, prev;
    sizeType		index;
};

inline  TObjDLList::TObjDLList(OwnerType ot)
{
	h = 0;
	curSz = 0;
	ownsElements(ot);
}

inline TObjDLList::~TObjDLList()
{
  clear();
}

inline BOOL TObjDLList::isEmpty() const
{
        return h == 0 ? TRUE : FALSE;
}

inline void TObjDLList::next(Pix& p) const
{
  p = (p == 0 || p == h->bk)? 0 : Pix(((TObjDLListNode*)p)->fd);
}

inline void TObjDLList::prev(Pix& p) const
{
  p = (p == 0 || p == h)? 0 : Pix(((TObjDLListNode*)p)->bk);
}

inline Pix TObjDLList::first() const
{
  return Pix(h);
}

inline Pix TObjDLList::last() const
{
  return (h == 0)? 0 : Pix(h->bk);
}

inline RPTObject TObjDLList::operator () (Pix p) const
{
  if (p == 0) error("null Pix");
  return ((TObjDLListNode*)p)->hd;
}

inline RPTObject TObjDLList::front() const
{
  if (h == 0) error("front: empty list");
  return h->hd;
}

inline RPTObject TObjDLList::rear() const
{
  if (h == 0) error("rear: empty list");
  return h->bk->hd;
}

inline  PIterator TObjDLList::initIterator() const
{
        return new TObjDLListIterator((RTObjDLList) *this);
}

inline BOOL     TObjDLList::add(PTObject pObj)
{
   if (append(pObj))
       return TRUE;
   else
       return FALSE;
}

inline void TObjDLList::removeAll()
{
	clear();
}

inline sizeType TObjDLList::size() const
{
	return curSz;
}

inline TObjDLListIterator::TObjDLListIterator(RCTObjDLList src) : pList(&src)
{
	restart();
}

inline TObjDLListIterator::TObjDLListIterator(RCTObjDLListIterator src) :
        pList(src.pList), cur(src.cur), prev(src.prev), index(src.index)
{
}

inline PIterator TObjDLListIterator::deepCopy() const
{
	return new TObjDLListIterator(*this);
}

inline PTObject TObjDLListIterator::operator ()()
{
        return cur != NULL ? (*pList)(cur) : NOOBJECT;
}

inline Pix TObjDLListIterator::curPix()
{
	return cur;
}

#if defined ( __postfix_inc__ )
inline PTObject TObjDLListIterator::operator ++ ( int )
{
       if( cur == NULL )
           return NOOBJECT;
       else {
              prev = cur;
	      pList->next(cur);
	      index++;
              return (*pList)(prev);
       }
}
#endif

inline PTObject TObjDLListIterator::operator ++ ()
{
       if ( cur == NULL )
           return NOOBJECT;
       else {
	   pList->next(cur);
           if ( cur ) {
               index++;
               return (*pList)(cur);
           }
           else
               return NOOBJECT;
       }
}

inline void TObjDLListIterator::restart()
{
        prev = 0;
	cur = pList->first();
	index = 0;
}

inline TObjDLListIterator::operator int()
{
	return cur != NULL;
}

inline sizeType TObjDLListIterator::indexOf() const
{
	return index;
}

inline  void    TObjDLListIterator::restart(RCTObjDLList src)
{
	pList = & src;
	restart();
}
#endif

