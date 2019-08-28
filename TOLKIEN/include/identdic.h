//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __IDENTDIC_H)
#define __IDENTDIC

#include "dict.h"

class   IdentDict : public Dictionary
{
public:

	DECLARE_RTTI()

                                IdentDict(OwnerType ot = reference);
                                IdentDict(RCIdentDict src,
                                          OwnerType ot = reference);
        virtual PTObject        includes(RCTObject) const;
                                _shallowCopy(IdentDict)
};

inline  IdentDict::IdentDict(OwnerType ot) : Dictionary()
{
        ownsElements(ot);
}

inline  IdentDict::IdentDict(RCIdentDict src, OwnerType ot) :
        Dictionary((RCDictionary) src, ot)
{
}

#endif

