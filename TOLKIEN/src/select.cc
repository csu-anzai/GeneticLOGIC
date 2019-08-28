//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "select.h"

static  PCTypeInfo selectBases[] = { &TObject::infoObj, 0 };
const TypeInfo  SelectionScheme::infoObj("Select", selectBases);

int   SelectionScheme::compare( RCTObject obj ) const
{
        if (obj.isKindOf(isA())) {
                if (pPop == ((RCSelectionScheme) obj).pPop)
                    return 0;
                else
                    return -1;
        }
        else
            return 1;
}
