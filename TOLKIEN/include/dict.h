//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __DICT_H )
#define __DICT_H

#include "assoc.h"
#include "set.h"

class Dictionary : public Set {
//
//      this class is based on the class with the same name of the NIH library
//
public:

	DECLARE_RTTI()

                           Dictionary(OwnerType ot = reference);
                           Dictionary(RCDictionary src,
                                      OwnerType ot = reference);

                           _shallowCopy(Dictionary)

        virtual PAssoc     addAssoc(RTObject key, RTObject value);
        virtual PLookupKey assocAt(RCTObject key) const;
        virtual PTObject   atKey(RCTObject key) const;
        virtual PTObject   atKey(RCTObject key, RTObject newValue);
        virtual PTObject   includes(RCTObject) const;
        virtual BOOL       includesAssoc(RCLookupKey asc) const;
        virtual BOOL       includesKey(RCTObject key) const;
        virtual PTObject   keyAtValue(RCTObject val) const;
        virtual void       removeAssoc(RCLookupKey asc);
	virtual void       removeKey(RCTObject key);

        virtual void       printOn( ostream  & out) const;
        virtual unsigned   occurrencesOf(RCTObject) const;
        virtual PTObject   remove(RCTObject);
};

inline  Dictionary::Dictionary(OwnerType ot)
{
        ownsElements(ot);
}

inline  Dictionary::Dictionary(RCDictionary src, OwnerType ot)
{
        ownsElements(ot);
        Collection::addAll((RCCollection) src);
}

#endif

