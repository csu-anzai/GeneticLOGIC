//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __COLLECTN_H )
#define    __COLLECTN_H

#include "tobject.h"
#include "townersh.h"

class Collection: public TObject, public TOwnership { // abstract class

public:
	DECLARE_RTTI()

	virtual                 ~Collection();

	virtual void 		deepenShallowCopy();
	virtual PTObject 	shallowCopy() const = 0;

	virtual BOOL 		isEmpty() const = 0;
	virtual sizeType        size() const = 0;

        virtual unsigned        occurrencesOf( RCTObject ) const = 0;
        virtual BOOL            isEqual( RCTObject ) const = 0;
	virtual PTObject        includes(RCTObject) const = 0;
	virtual int        	compare( RCTObject ) const = 0;

	virtual PTObject        at(sizeType) const = 0;
	virtual PTObject   	elem(sizeType) const = 0;
	virtual PTObject   	insertAt(sizeType, PTObject) = 0;
        virtual PTObject        removeAt(sizeType) = 0;
        virtual void            setAt(sizeType, PTObject) = 0;

	virtual BOOL 		add(PTObject) = 0;
	virtual RCCollection 	addAll(RCCollection);
	virtual RCollection 	addContentsTo(RCollection) const;

	virtual PTObject        remove(RCTObject) = 0;
	virtual void       	removeAll() = 0;
	virtual RCCollection    removeCltn(RCCollection);

	virtual void 		printOn( ostream  & out) const;
	virtual void 		printHeader( ostream& ) const;
	virtual void 		printSeparator( ostream& ) const;
	virtual void 		printTrailer( ostream& ) const;

	virtual PIterator 	initIterator() const = 0;

	friend class Iterator;
};

inline  Collection::~Collection()
{
}

inline  void Collection::deepenShallowCopy()
{
}

#include "iterator.h"

#endif

