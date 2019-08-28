//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __LOOKUPKY_H)
#define  __LOOKUPKY_H

#include "tobject.h"
#include "errhndl.h"

class LookupKey: public TObject {
//
//      this class is based on the class with the same name of the NIH library
//
public:
	DECLARE_RTTI()

                                LookupKey(RTObject newKey =*nil);
                                ~LookupKey();

        virtual int             compare(RCTObject) const;
        virtual BOOL            isEqual(RCTObject) const;

        virtual PCTObject       key() const;
        virtual PTObject        key();
        virtual PTObject        key(RTObject newkey);

        virtual PTObject        value();
        virtual PCTObject       value() const;
        virtual PTObject        value(RTObject );

        virtual hashValueType   hashValue() const;
                                _shallowCopy(LookupKey)
        virtual void            deepenShallowCopy();

        virtual void            printOn( ostream  &) const;

protected:
	PTObject akey;
};

inline  LookupKey::~LookupKey()
{
}

inline  PCTObject  LookupKey::key() const
{
        return akey;
}

inline  PTObject   LookupKey::key()
{
        return akey;
}

inline  void       LookupKey::deepenShallowCopy()
{
}

inline hashValueType  LookupKey::hashValue() const
{
        return akey->hashValue();
}

#endif

