//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#include "hashset.h"

static  PCTypeInfo hashsetBases[] = { &HashTable::infoObj, 0 };
const TypeInfo  HashSet::infoObj("HashSet", hashsetBases);


BOOL HashSet::add(PTObject pObj)
{
        if ( includes(*pObj) == NOOBJECT )
            return HashTable::add(pObj);
        else
            return FALSE;
}

PTObject HashSet::insertAt(sizeType index, PTObject pObj)
{
        if ( includes(*pObj) == NOOBJECT )
            return HashTable::insertAt(index, pObj);
        else
            return FALSE;
}

void HashSet::setAt(sizeType index, PTObject pObj)
{
        if ( includes(*pObj) == NOOBJECT )
            HashTable::setAt(index, pObj);
        else
            error("setAt : object already in set");
}
