//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "set.h"

static  PCTypeInfo setBases[] = { &Collection::infoObj, 0 };
const TypeInfo  Set::infoObj("Set", setBases);

Set::Set(RCSet set, OwnerType ot) : TOArray((RCTOArray) set, ot)
{
}

PTObject Set::includes(RCTObject obj) const
{
        PTObject pObj = NOOBJECT;
	CollectionIterator iter(*this);
	while ( iter ) {
		if (obj.isEqual(*iter())) {
		    pObj = iter();
		    break;
		}
                ++iter;
	}
	return pObj;
}

BOOL    Set::add(PTObject pObj)
{
        if ( includes(*pObj) != NOOBJECT )
            return FALSE;
        else {
            TOArray::add(pObj);
            return TRUE;
	}
}


BOOL   Set::isEqual( RCTObject obj ) const
{
	if ( obj.isA() != isA() )
	    return FALSE;
	else {
		sizeType i;
		if ( ( i = ((RCCollection) obj).size() ) != size() )
                    return FALSE;
                else {
		    RCSet set = (RCSet) obj;
                    while (i--)
                        if (set.includes(*pData[i]) == NOOBJECT )
                            return FALSE;
                    return TRUE;
                }
        }
}

