//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include <limits.h>
#include <math.h>
#include "tobject.h"
#include "identdic.h"

const TypeInfo  TObject::infoObj("TObject", (PCTypeInfo *) 0);

static  PCTypeInfo errorBases[] = { &TObject::infoObj, 0 };
const TypeInfo  TError::infoObj("TError", errorBases);

static  PCTypeInfo tgaobjBases[] = { &TObject::infoObj, 0 };
const TypeInfo	TGAObj::infoObj("TGAObj", tgaobjBases);

PTObject TObject::copy() const
{
	return shallowCopy();
}

TError tkErrorObject;

PTObject TObject::nil = &tkErrorObject;

static IdentDict* deepCopyDict =0;
	// object ID -> object copy dictionary for deepCopy()

PTObject TObject::deepCopy() const
{
	BOOL firstTObject = FALSE;

	if (deepCopyDict == 0) {
		deepCopyDict = new IdentDict(owner);
		firstTObject = TRUE;
	}
	Assoc* asc = (Assoc*)deepCopyDict->assocAt((RTObject) *this);
        if (asc == NOOBJECT) { // object has not been copied
		PTObject copy = shallowCopy();   // make a shallow copy
                deepCopyDict->add(new Assoc(*(PTObject)this,*copy));
                                // add to dictionary
                copy->deepenShallowCopy(); // convert shallow copy to deep copy
                if (firstTObject) {        // delete the deepCopy dictionary
		    delete deepCopyDict;
		    deepCopyDict = 0;
		}
		return copy;
	}
	else return asc->value();	
        // object already copied, just return object reference 
}

hashValueType TObject::hashValue() const
//
//
//      This hash function uses the address of the object as key and
//      mapped it to a value in [0, M - 1]. (see below for M)
//
//	The method used is the multiplication hashing described in Kunth's
//	The Art of Computer Programming, vol 3.
//
//      hashValue = floor( M * frac( Ak ) )
//
//	where A is the golden ratio ( 0.6180339887499 )
//	      k is the address of the object
//            M is a constant ( chosen to be HASH_MAX )
//
{	
	double integer;
	return floor(HASH_MAX * modf(((unsigned long) (PCTObject) this) * 0.6180339887499, &integer));
}

int compare_ob(const void* a, const void* b)
{
	return (*(PCTObject*)a)->compare(**(PCTObject*)b);
}

int TypeInfo::hasBase(PCTypeInfo pSrc) const
{
        PCTypeInfo p, pmy = this;
	int nI = 0;

        if (b)
            p = b[0];
        else
            p = 0;

        if (pSrc == pmy)
	    return 1;

	while (p) {
		if (p == pSrc)
		    return 1;
                else
                    if (p->hasBase(pSrc))
                        return 1;
                    else
                        p = b[++nI];
	}

	return 0;
}

int  TGAObj::compare( RCTObject obj ) const
{
	if ( obj.isKindOf(isA()) ) {
            double val1 = fitness(), val2 = ((RCTGAObj) obj).fitness();
	    if (val1 == val2)
		return 0;
	    else
		if (val1 > val2)
		    return 1;
	}

	return -1;
}

int  decFitness(const void *pMem1, const void *pMem2)
{
	//
        // sort elements in descending order of fitness
	//
        if ((*((PTGAObj *) pMem2))->fitness() >
            (*((PTGAObj *) pMem1))->fitness())
            return 1;
        else if ((*((PTGAObj *) pMem2))->fitness() <
                 (*((PTGAObj *) pMem1))->fitness())
                 return -1;
	else
	     return 0;
}

int  ascFitness(const void *pMem1, const void *pMem2)
{
	//
        // sort elements in descending order of fitness
	//
        if ((*((PTGAObj *) pMem1))->fitness() >
            (*((PTGAObj *) pMem2))->fitness())
            return 1;
        else if ((*((PTGAObj *) pMem1))->fitness() <
		 (*((PTGAObj *) pMem2))->fitness())
                 return -1;
	else
	     return 0;
}

int  decObjValue(const void *pMem1, const void *pMem2)
{
	//
        // sort elements in descending order of objective value
	//
        if ((*((PTGAObj *) pMem2))->objValue() >
            (*((PTGAObj *) pMem1))->objValue())
            return 1;
        else if ((*((PTGAObj *) pMem2))->objValue() <
                 (*((PTGAObj *) pMem1))->objValue())
	     return -1;
	else
	     return 0;
}

int  ascObjValue(const void *pMem1, const void *pMem2)
{
	//
        // sort elements in descending order of objective value
	//
        if ((*((PTGAObj *) pMem1))->objValue() >
            (*((PTGAObj *) pMem2))->objValue())
            return 1;
        else if ((*((PTGAObj *) pMem1))->objValue() <
                 (*((PTGAObj *) pMem2))->objValue())
	     return -1;
	else
	     return 0;
}
