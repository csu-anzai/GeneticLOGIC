//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "assoc.h"

static  PCTypeInfo assocBases[] = { &LookupKey::infoObj, 0 };
const TypeInfo  Assoc::infoObj("Assoc", assocBases);

Assoc::Assoc(TObject& newKey, TObject& newValue) : LookupKey(newKey)
{
	avalue = &newValue;
}

PTObject Assoc::value() { return avalue; }

PCTObject Assoc::value() const { return avalue; }

PTObject Assoc::value(TObject& newvalue)
{
	PTObject temp = avalue;
	avalue = &newvalue;
	return temp;
}
