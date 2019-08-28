//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __SET_H )
#define    __SET_H

#include  "collectn.h"
#include  "toarray.h"

class Set: public TOArray {
//
//      This class is based on the Set class of the NIH library
//
public:
                                DECLARE_RTTI()

                                Set(OwnerType ot = reference,
                                    sizeType = DEFAULT_COLLECTION_SIZE,
                                    unsigned = DEFAULT_GROW_SIZE);
                                Set(RCSet, OwnerType = reference);

                                _shallowCopy(Set)
        virtual BOOL            add(PTObject);
        virtual void            setAt(sizeType pos, PTObject pNewObj);
        virtual PTObject        insertAt(sizeType pos, PTObject pNewObj);
        virtual PTObject        includes(RCTObject) const;
        virtual BOOL            isEqual( RCTObject ) const;
};

inline  Set::Set(OwnerType ot,
		 sizeType size,
		 unsigned growSz) :
	TOArray(ot, size, growSz)
{
}

inline void     Set::setAt(sizeType pos, PTObject pNewObj)
{
        if ( includes(*pNewObj) == NOOBJECT )
            TOArray::setAt(pos, pNewObj);
}

inline PTObject Set::insertAt(sizeType pos, PTObject pNewObj)
{
        if ( includes(*pNewObj) == NOOBJECT )
            TOArray::setAt(pos, pNewObj);
        return pNewObj;
}

#endif

