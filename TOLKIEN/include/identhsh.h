//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __IDENTHSH_H)
#define __IDENTHSH
#include "hashtabl.h"

class   IdentHashTable : public HashTable
{
public :

	DECLARE_RTTI()

                                IdentHashTable( OwnerType = reference );
                                IdentHashTable( RCIdentHashTable ,
                                                OwnerType = reference);

                                _shallowCopy(IdentHashTable)
	virtual BOOL 		add(PTObject);
	virtual PTObject        includes( RCTObject ) const;
};

inline IdentHashTable::IdentHashTable( OwnerType ot ) :
                        HashTable(ot)
{
}

inline IdentHashTable::IdentHashTable( RCIdentHashTable src,
                                       OwnerType ot) :
                        HashTable((RCHashTable) src, ot)
{
}

inline BOOL IdentHashTable::add(PTObject pObj)
{
        if (includes(*pObj) == NOOBJECT)
	    return HashTable::add(pObj);
	else
            return FALSE;
}

#endif

