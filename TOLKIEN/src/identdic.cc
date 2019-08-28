//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "identdic.h"

static  PCTypeInfo identdicBases[] = { &Dictionary::infoObj, 0 };
const TypeInfo  IdentDict::infoObj("IdentDict", identdicBases);

PTObject IdentDict::includes(RCTObject key) const
{
        CollectionIterator  iter(*this);
        PTObject        pObj;

	while ( iter ) {
	     if (((PCLookupKey) iter())->key()->isSame(key)) {
		 pObj = iter();
		 return pObj;
	     }
             ++iter;
	}
        return NOOBJECT;
}

