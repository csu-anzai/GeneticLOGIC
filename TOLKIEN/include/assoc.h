//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __ASSOC_H )
#define __ASSOC_H

#include "lookupky.h"

//
//      An Assoc instance is an association of a key and a value.
//      A dictionary is a collection of Assoc objects.
//
//      This class is based on the class with the same name of the NIH library
//

class Assoc : public LookupKey {
public:

	DECLARE_RTTI()

                                Assoc(RTObject newKey =*NOOBJECT,
                                      RTObject newValue =*NOOBJECT);
        virtual PTObject        value();
        virtual PCTObject       value() const;
        virtual PTObject        value(RTObject newvalue);

                                _shallowCopy(Assoc)

private:
	PTObject avalue;
};

#endif

