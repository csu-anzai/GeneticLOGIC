//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#if !defined ( __HASHSET_H )
#define    __HASHSET_H

#include "hashtabl.h"

class   HashSet : public HashTable
{
public:
        DECLARE_RTTI()

                                HashSet( OwnerType = reference );
                                HashSet( RCHashSet , OwnerType = reference);

                                _shallowCopy(HashSet)

	virtual BOOL 		add(PTObject);
	virtual PTObject   	insertAt(sizeType, PTObject);
	virtual void       	setAt(sizeType, PTObject);
};

inline HashSet::HashSet( OwnerType ot) :
        HashTable(ot)
{
}

inline HashSet::HashSet( RCHashSet src, OwnerType ot) :
        HashTable((RCHashTable) src, ot)
{
}

#endif

