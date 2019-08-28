//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __SORTCLTN_H )
#define    __SORTCLTN_H

#include "toarray.h"

class   SortedCltn : public Collection
{

public:
				SortedCltn(OwnerType ot = reference);
				SortedCltn(RCSortedCltn,
					   OwnerType ot = reference);
				~SortedCltn();

				_shallowCopy(SortedCltn)
	virtual void            deepenShallowCopy();

	virtual BOOL            isEmpty() const;
	virtual BOOL            isEqual( RCTObject ) const;
	virtual int             compare( RCTObject ) const;
	virtual unsigned        occurrencesOf(RCTObject ) const;
	virtual PTObject	includes(RCTObject ) const;

	virtual PTObject        at(sizeType ) const;
	virtual PTObject        elem(sizeType ) const;

	virtual BOOL            add(PTObject);
	virtual void            setAt(sizeType, PTObject);
	virtual PTObject        insertAt(sizeType, PTObject);
	virtual void            removeAll();
	virtual PTObject        remove(RCTObject );
	virtual PTObject        removeAt(sizeType);

	virtual sizeType        size() const;

	virtual PIterator       initIterator() const;

protected:

	TOArray contents;

	virtual sizeType    findIndexOf(RCTObject) const;
	virtual sizeType    findIndexOfLastKey(RCTObject, sizeType) const;
};

inline  SortedCltn::SortedCltn(OwnerType ot) : contents()
{
	contents.ownsElements(ot);
}

inline  SortedCltn::~SortedCltn()
{
}

inline void     SortedCltn::deepenShallowCopy()
{
        contents.deepenShallowCopy();
}

inline BOOL     SortedCltn::isEmpty() const
{
        return contents.isEmpty();
}

inline unsigned SortedCltn::occurrencesOf(RCTObject obj) const
{
        return contents.occurrencesOf(obj);
}

inline PTObject	SortedCltn::includes(RCTObject obj) const
{
	return contents.includes(obj);
}

inline PTObject SortedCltn::remove(RCTObject obj)
{
        return contents.remove(obj);
}

inline PTObject  SortedCltn::removeAt(sizeType index)
{
        return contents.removeAt(index);
}

inline PTObject  SortedCltn::at(sizeType pos) const
{
        return contents.at(pos);
}

inline PTObject  SortedCltn::elem(sizeType pos) const
{
        return contents.elem(pos);
}

inline PTObject SortedCltn::insertAt(sizeType pos, PTObject pNewObj)
{
        if ( contents.add(pNewObj) )
             return pNewObj;
        else
             return NOOBJECT;
}

inline void    SortedCltn::removeAll()
{
        contents.removeAll();
}

inline sizeType SortedCltn::size() const
{
        return contents.size();
}

inline PIterator SortedCltn::initIterator() const
{
        return contents.initIterator();
}

inline void     SortedCltn::setAt(sizeType pos, PTObject pNewObj)
{
        delete contents.removeAt(pos);
        contents.add(pNewObj);
}

inline BOOL SortedCltn::isEqual( RCTObject obj ) const
{
	if (! obj.isKindOf(isA()) )
	    return FALSE;
	else
	    return contents.isEqual( ((RCSortedCltn) obj).contents );
}

#endif

