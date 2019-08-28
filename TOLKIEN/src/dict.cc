//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "dict.h"

static  PCTypeInfo dictBases[] = { &Set::infoObj, 0 };
const TypeInfo  Dictionary::infoObj("Dictionary", dictBases);

PTObject Dictionary::includes(RCTObject key) const
{
        PTObject pObj = NOOBJECT;

	CollectionIterator iter(*this);
        while (iter && (pObj == NOOBJECT)) {
		if (((PLookupKey) iter())->key()->isEqual(key))
		   pObj = iter();
                ++iter;
	}
	return pObj;
}

PAssoc  Dictionary::addAssoc(RTObject key, RTObject value)
{
	PTObject pObj;
        if ((pObj = includes(key)) != NOOBJECT) {
		 error("Association already existed");
		 return (PAssoc) pObj;
	     }
	Assoc assoc(key, value);
        return (PAssoc) add(new Assoc(key,value));
}

PLookupKey Dictionary::assocAt(RCTObject key) const
{
	PTObject pObj;
        if ((pObj = includes(key)) != NOOBJECT)
	    return (PLookupKey) pObj;
	else
            return (PLookupKey) NOOBJECT;
}

PTObject   Dictionary::atKey(RCTObject key) const
{
	PTObject pObj;
        if ((pObj = includes(key)) != NOOBJECT)
	    return ((PAssoc) pObj)->value();
	else
            return NOOBJECT;
}

PTObject   Dictionary::atKey(RCTObject key, RTObject newValue)
{
	PTObject        oldValue, pObj;
        if ((pObj = includes(key)) != NOOBJECT) {
	     oldValue = ((PAssoc) pObj)->value(newValue);
	     return oldValue;
	}
	else
             return NOOBJECT;
}

BOOL Dictionary::includesAssoc(const LookupKey& asc) const
{
        CollectionIterator iter(*this);
        BOOL flag = FALSE;
        while (iter && (flag == FALSE)) {
	     if (((PLookupKey) iter())->key()->isEqual(*asc.key()))
		 if (((PAssoc) iter())->value()->isEqual(*asc.value()))
                     flag = TRUE;
             ++iter;
	}
	return flag;
}

BOOL Dictionary::includesKey(RCTObject key) const
{
        if (includes(key) != NOOBJECT)
            return TRUE;
	else
            return FALSE;
}

PTObject Dictionary::keyAtValue(RCTObject val) const
{
        PTObject pObj = NOOBJECT;
        CollectionIterator iter(*this);
        while (iter && (pObj == NOOBJECT)) {
	     if (((PAssoc) iter())->value()->isEqual(val))
		   pObj = ((PAssoc) iter())->key();
             ++iter;
	}
	return pObj;
}

PTObject Dictionary::remove(RCTObject asc)
{
	PRECONDITION(asc.isA() == Assoc::typeInfo());

        PTObject pObj = NOOBJECT;
	TOArrayIterator iter(*this);

        while (iter && (pObj == NOOBJECT)) {
	    if (((PLookupKey) iter())->key()->
		   isEqual(*((const LookupKey &) asc).key()))
		pObj = removeAt(iter.indexOf());
	    iter++;
	}
	return pObj;
}

void    Dictionary::removeAssoc(const LookupKey& asc)
{
	TOArrayIterator iter(*this);

	while ( iter ) {
	   if (((PLookupKey) iter())->key()->isEqual(*asc.key())) {
               delete removeAt(iter.indexOf());
	       return;
	   }
	   iter++;
	}
}

void    Dictionary::removeKey(RCTObject key)
{
	TOArrayIterator iter(*this);

	while ( iter ) {
		if (((PLookupKey) iter())->key()->isEqual(key)) {
                    delete removeAt(iter.indexOf());
		    return;
		}
		iter++;
	}
	return;
}

void    Dictionary::printOn(ostream & out) const
{
	CollectionIterator iter(*this);
	printHeader(out);

	while (iter) {
	     ((PAssoc) iter())->key()->printOn(out);
	     out << "=>";
	     ((PAssoc) iter())->value()->printOn(out);
	     out << endl;
             ++iter;
	}
	printTrailer(out);
}

#if !defined ( __postfix_inc__ )
unsigned Dictionary::occurrencesOf(RCTObject obj) const
{
	sizeType total = 0;
	CollectionIterator iter(*this);

	while ( iter ) {
		if (obj.isEqual(* iter()))
		    total++;
                ++iter;
	}
	return total;
}
#else
unsigned Dictionary::occurrencesOf(RCTObject obj) const
{
	sizeType total = 0;
	CollectionIterator iter(*this);

        while ( iter )
                if ( obj.isEqual(* iter++) )
		    total++;

	return total;
}
#endif
