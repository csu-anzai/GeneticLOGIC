//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#include "identhsh.h"


static  PCTypeInfo identhashtableBases[] = { &HashTable::infoObj, 0 };
const TypeInfo  IdentHashTable::infoObj("IdentHashTable", identhashtableBases);


PTObject IdentHashTable::includes( RCTObject obj ) const
{
        long prime = hash_primes[curBucket];

        unsigned long hash = getHashValue(obj) % prime;
        if (ppBuckets[hash])
            for (unsigned i = 0; i < ppBuckets[hash]->size; i++)
                 if (obj.isSame(* ppBuckets[hash]->data[i]))
                     return ppBuckets[hash]->data[i];
        return NOOBJECT;
}

