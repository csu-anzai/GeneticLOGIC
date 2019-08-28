//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "lookupky.h"

static  PCTypeInfo lookupkeyBases[] = { &TObject::infoObj, 0 };
const TypeInfo  LookupKey::infoObj("LookupKey", lookupkeyBases);

LookupKey::LookupKey(RTObject newKey)
{
	akey = &newKey;
}

PTObject LookupKey::key(RTObject newkey)
{
        PTObject temp = akey;
	akey = &newkey;
	return temp;
}

BOOL LookupKey::isEqual(RCTObject ob) const { return ob.isEqual(*akey); }

int LookupKey::compare(RCTObject ob) const { return - ob.compare(*akey); }

void LookupKey::printOn(ostream& strm) const
{
	key()->printOn(strm);
	strm << "=>";
	value()->printOn(strm);
}

PCTObject LookupKey::value() const
{
        return NOOBJECT;
}

PTObject LookupKey::value()
{
        return NOOBJECT;
}

PTObject LookupKey::value(RTObject newvalue)
{
	return 0;
}

