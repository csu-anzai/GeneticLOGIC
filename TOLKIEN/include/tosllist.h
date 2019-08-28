// This may look like C code, but it is really -*- C++ -*-
// WARNING: This file is obsolete.  Use ../SLList.h, if you can.
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


#ifndef _TObjSLList_h
#ifdef __GNUG__
#pragma interface
#endif
#define _TObjSLList_h 1

#include <Pix.h>

#ifndef _TObjSLListNode_h
#define _TObjSLListNode_h 1

#include "collectn.h"

struct TObjSLListNode
{
  TObjSLListNode*        tl;
  PTObject               hd;
                         TObjSLListNode() { }
                         TObjSLListNode(PTObject  h,
                                        TObjSLListNode* t = 0);
                         ~TObjSLListNode() { }
};


inline TObjSLListNode::TObjSLListNode(PTObject  h, TObjSLListNode* t)
:hd(h), tl(t) {}

typedef TObjSLListNode* TObjSLListNodePtr;

#endif


class TObjSLList : public Collection
{
public:
				DECLARE_RTTI()

				TObjSLList(OwnerType dt = reference);
				TObjSLList(RCTObjSLList a,
					   OwnerType dt = reference);
				~TObjSLList();

				_shallowCopy(TObjSLList)
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

	//      GNU's SLList functions
		void            clear();

		Pix             prepend(PTObject  item);
		Pix             append(PTObject  item);

		void            join(TObjSLList&);
		Pix             prepend(TObjSLListNode*);
		Pix             append(TObjSLListNode*);

		RPTObject       operator () (Pix p) const;
		Pix             first() const;
		void            next(Pix& p) const;
		int             owns(Pix p) const;
		Pix             ins_after(Pix p, PTObject  item);
		void            del_after(Pix p);

		// remove_after : added by Anthony Tang
		PTObject	remove_after(Pix p);

		RPTObject       front() const;
		RPTObject       rear() const;
		PTObject        remove_front();
		int             remove_front(RPTObject x);
		void            del_front();
		int             OK();

                friend class TObjSLListIterator;

protected:
                virtual sizeType        sizeOfList() const;
                TObjSLListNode*         last;
                sizeType                curSz;

};

class TObjSLListIterator : public Iterator
{
public:

			TObjSLListIterator(RTObjSLList);
			TObjSLListIterator(RTObjSLListIterator);

    virtual PTObject    operator ()();

#if defined ( __postfix_inc__ )
    virtual PTObject    operator ++ ( int );
#endif

    virtual PTObject    operator ++ ();
    virtual void        restart();
    virtual 		operator int();
    virtual sizeType	indexOf() const;
    virtual PIterator	deepCopy() const;

protected:

    PTObjSLList         pList;
    Pix                 cur, prev;
    sizeType		index;
};

inline TObjSLList::~TObjSLList()
{
  clear();
}

inline TObjSLList::TObjSLList(OwnerType ot)
{
  last = 0;
  curSz = 0;
  ownsElements(ot);
}


inline Pix TObjSLList::first() const
{
  return (last == 0)? 0 : Pix(last->tl);
}

inline void TObjSLList::next(Pix& p) const
{
  p = (p == 0 || p == last)? 0 : Pix(((TObjSLListNode*)(p))->tl);
}

inline RPTObject TObjSLList::operator () (Pix p) const
{
  if (p == 0) error("null Pix");
  return ((TObjSLListNode*)(p))->hd;
}

inline RPTObject TObjSLList::front() const
{
  if (last == 0) error("front: empty list");
  return last->tl->hd;
}

inline RPTObject TObjSLList::rear() const
{
  if (last == 0) error("rear: empty list");
  return last->hd;
}

inline BOOL     TObjSLList::add(PTObject pObj)
{
   if (append(pObj))
       return TRUE;
   else
       return FALSE;
}

inline void TObjSLList::removeAll()
{
	clear();
}

inline sizeType TObjSLList::size() const
{
	return curSz;
}

inline BOOL TObjSLList::isEmpty() const
{
	return curSz == 0 ? TRUE : FALSE;
}

inline PIterator TObjSLList::initIterator() const
{
	return new TObjSLListIterator((RTObjSLList) *this);
}

inline TObjSLListIterator::TObjSLListIterator(RTObjSLList src) : pList(&src)
{
	restart();
}

inline TObjSLListIterator::TObjSLListIterator(RTObjSLListIterator src) :
       pList(src.pList), cur(src.cur), prev(src.prev), index(src.index)
{
}

inline PIterator TObjSLListIterator::deepCopy() const
{
	return new TObjSLListIterator(*this);
}

inline PTObject TObjSLListIterator::operator ()()
{
        return cur != NULL ? (*pList)(cur) : NOOBJECT;
}

#if defined ( __postfix_inc__ )
inline PTObject TObjSLListIterator::operator ++ ( int )
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

inline PTObject TObjSLListIterator::operator ++ ()
{
       if ( cur == NULL )
           return NOOBJECT;
       else {
           prev = cur;
	   pList->next(cur);
           if ( cur ) {
               index++;
               return (*pList)(cur);
           }
           else
               return NOOBJECT;
       }
}

inline void TObjSLListIterator::restart()
{
        prev = 0;
	cur = pList->first();
	index = 0;
}

inline TObjSLListIterator::operator int()
{
	return cur != NULL;
}

inline sizeType TObjSLListIterator::indexOf() const
{
	return index;
}

#endif

