//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "atom.h"
#include "trand.h"
#include "tvalues.h"

static  PCTypeInfo atombaseBases[] = { &TObject::infoObj, 0 };
const TypeInfo  AtomBase::infoObj("AtomBase", atombaseBases);

static  PCTypeInfo atomBases[] = { &AtomBase::infoObj, 0 };
const TypeInfo  Atom::infoObj("AtomBase", atomBases);

static  PCTypeInfo tcharBases[] = { &AtomBase::infoObj, 0 };
const TypeInfo  TChar::infoObj("TChar", tcharBases);

static  PCTypeInfo tintBases[] = { &AtomBase::infoObj, 0 };
const TypeInfo  TInteger::infoObj("TInteger", tintBases);

static  PCTypeInfo tlongBases[] = { &AtomBase::infoObj, 0 };
const TypeInfo  TLong::infoObj("TLong", tlongBases);

static  PCTypeInfo tdblBases[] = { &AtomBase::infoObj, 0 };
const TypeInfo  TDouble::infoObj("TDouble", tdblBases);


BOOL TChar::isEqual( RCTObject  obj ) const
{
	if ( ! obj.isKindOf(TChar::typeInfo()) )
	    return FALSE;
	else
	    return (chData == ((RCTChar) obj).chData );
}

BOOL TInteger::isEqual( RCTObject  obj ) const
{
	if ( ! obj.isKindOf(TInteger::typeInfo()) )
	    return FALSE;
	else
	    return (nData == ((RCTInteger) obj).nData );
}

BOOL TLong::isEqual( RCTObject  obj ) const
{
	if ( ! obj.isKindOf(TLong::typeInfo()) )
	    return FALSE;
	else
	    return (lData == ((RCTLong) obj).lData );
}

BOOL TDouble::isEqual( RCTObject  obj ) const
{
	if ( ! obj.isKindOf(TDouble::typeInfo()) )
	    return FALSE;
	else
	    return (flData == ((RCTDouble) obj).flData );
}

void   TChar::mutate(const float flRate)
{
	if (tRand.flip(flRate))
	    chData = 'A' + tRand(0,26);
}

void   TDouble::mutate(const float flRate)
{
	if (tRand.flip(flRate))
	    flData = tRand();
}

void   TInteger::mutate(const float flRate)
{
	if (tRand.flip(flRate))
	    nData = tRand.asInt();
}

void   TLong::mutate(const float flRate)
{
	if (tRand.flip(flRate))
	    lData = tRand.asLong();
}
