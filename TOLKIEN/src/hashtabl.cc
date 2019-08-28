//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
//      This class is a modification of the classes CoolBase_Hash_Table and
//      CoolHash_Table of the COOL class library.  The following is the
//      copyright notice.
//
// Copyright (C) 1991 Texas Instruments Incorporated.
//
// Permission is granted to any individual or institution to use, copy, modify,
// and distribute this software, provided that this complete copyright and
// permission notice is maintained, intact, in all copies and supporting
// documentation.
//
// Texas Instruments Incorporated provides this software "as is" without
// express or implied warranty.
//
//
#include "hashtabl.h"

long hash_primes[] = {3, 7, 13, 19, 29, 41, 53, 67, 83, 97, 113, 137,
                      163, 191, 223, 263, 307, 349, 401, 461, 521,
		      653, 719, 773, 839, 911, 983, 1049, 1123, 1201,
		      1279, 1367, 1459, 1549, 1657, 1759, 1861, 1973,
		      2081, 2179, 2281, 2383, 2503, 2617, 2729, 2843,
		      2963, 3089, 3203, 3323, 3449, 3571, 3697, 3833,
		      3967, 4099, 4241, 4391, 4549, 4703, 4861, 5011,
		      5171, 5333, 5483, 5669, 5839, 6029, 6197, 6361,
		      6547, 6761, 6961, 7177, 7393, 7517, 7727, 7951,
		      8101, 8209, 16411, 32771, 65537, 131301, 262147,
		      524287};

static  PCTypeInfo hashtableBases[] = { &Collection::infoObj, 0 };
const TypeInfo  HashTable::infoObj("HashTable", hashtableBases);

HashTable::HashTable( OwnerType ot ) :
		curBucket(0), items(0)
{
	long    prime;

        ownsElements(ot);
        prime = hash_primes[curBucket];

        if ((ppBuckets =
                (PHashTableBucket*) new PHashTableBucket[prime]) != NULL)
            for (long i = 0; i < prime; i++)
                 ppBuckets[i] = 0;
	else
            error("constructor : memory allocation error");
}

HashTable::HashTable( RCHashTable srcTable, OwnerType ot ) :
	items(srcTable.items),
        curBucket(srcTable.curBucket)
{
        ownsElements(reference);

        long prime = hash_primes[curBucket];
        ppBuckets = (PHashTableBucket*) new PHashTableBucket[prime];
        if ((ppBuckets =
                (PHashTableBucket*) new PHashTableBucket[prime]) != NULL)
            for (long i = 0; i < prime; i++)
                 if (srcTable.ppBuckets[i])
                     ppBuckets[i] =
                        new HashTableBucket(*srcTable.ppBuckets[i]);
                 else
                     ppBuckets[i] = NULL;
        else
            error("constructor : memory allocation error");

        if (ot == owner)
            deepenShallowCopy();
}

HashTable::~HashTable()
{
	long prime = hash_primes[curBucket], i, j;

        if ( items > 0 )
            for (i = 0; i < prime; i++)
		 if ( ppBuckets[i] ) {
                     if (delObj())
                         for (j = 0; j < ppBuckets[i]->size; j++)
			      delete ppBuckets[i]->data[j];
                     delete ppBuckets[i];
                 }

        delete [] ppBuckets;
}

PTObject HashTable::includes( RCTObject obj ) const
{
	long prime = hash_primes[curBucket];
        unsigned long hash = getHashValue(obj) % prime;

	if (ppBuckets[hash])
	    for (unsigned i = 0; i < ppBuckets[hash]->size; i++)
		 if (obj.isEqual(* ppBuckets[hash]->data[i]))
		     return ppBuckets[hash]->data[i];

        return NOOBJECT;
}

void  HashTable::deepenShallowCopy()
{
        if (ownsElements())
            return;
        else {
            long prime = hash_primes[curBucket], i, j;

            if ( items > 0 )
                for (i = 0; i < prime; i++)
                     if ( ppBuckets[i] )
                         for (j = 0; j < ppBuckets[i]->size; j++)
			      ppBuckets[i]->data[j] =
                                ppBuckets[i]->data[j]->deepCopy();
            ownsElements(owner);
        }
}

BOOL  HashTable::add(PTObject pObj)
{
 retry:
  long prime = hash_primes[curBucket]; // Prime number of buckets
  unsigned long hash = getHashValue(*pObj) % prime; // Get hash value

  if (ppBuckets[hash] == NULL)
      ppBuckets[hash] = new HashTableBucket();

  if (ppBuckets[hash]->size >= BUCKET_SIZE) {   // If bucket is full
    this->capacity (hash_primes[curBucket+1]*BUCKET_SIZE); // Grow
    goto retry;
  }

  ppBuckets[hash]->add(pObj);
  items++;

  return TRUE;			   // Indicate new
}

unsigned HashTable::occurrencesOf(RCTObject obj) const
{
	 long prime = hash_primes[curBucket]; // Prime number of buckets
         unsigned long hash = getHashValue(obj) % prime; // Get hash value

	 if (ppBuckets[hash] == NULL)
	     return 0;
	 else {
	     for (long i = 0, j = 0; i < ppBuckets[hash]->size; i++)
		  if (obj.isEqual(* ppBuckets[hash]->data[i]))
		      j++;
	     return j;
	 }
}

PTObject HashTable::remove(RCTObject obj)
{
	 long prime = hash_primes[curBucket]; // Prime number of buckets
         unsigned long hash = getHashValue(obj) % prime; // Get hash value
	 PTObject pObj;

	 if (ppBuckets[hash] == NULL)
             return NOOBJECT;
	 else {
	     for (long i = 0; i < ppBuckets[hash]->size; i++)
		  if (obj.isEqual(* ppBuckets[hash]->data[i])) {
		      pObj = ppBuckets[hash]->data[i];
		      memmove(& ppBuckets[hash]->data[i],
			      & ppBuckets[hash]->data[i+1],
			     (ppBuckets[hash]->size - i - 1) *
			      sizeof(PTObject));
                      ppBuckets[hash]->size--;
		      if (ppBuckets[hash]->size == 0) {
                          delete ppBuckets[hash];
                          ppBuckets[hash] = NULL;
		      }
		      items--;
		      return pObj;
		 }
	 }

         return NOOBJECT;
}

PTObject HashTable::at(sizeType index) const
{
	if (index < items) {
	    long i, j;
	    for (i=0, j = index; i < curBucket; i++)
		if (ppBuckets[i] != NULL)
		    if (j >= ppBuckets[i]->size)
			j -= ppBuckets[i]->size;
		    else
			return ppBuckets[i]->data[j];
	}

        return NOOBJECT;
}

void HashTable::setAt(sizeType, PTObject)
{
	error("setAt cannot be called");
}

PTObject HashTable::insertAt(sizeType, PTObject)
{
	error("insertAt cannot be called");
        return NOOBJECT;
}

PTObject HashTable::removeAt(sizeType)
{
	error("removeAt cannot be called");
        return NOOBJECT;
}

void HashTable::removeAll()
{
	long prime = hash_primes[curBucket], i, j;

	if ( items > 0 ) {
	    for (i = 0; i < prime; i++)
		 if ( ppBuckets[i] ) {
		     if (delObj())
			 for (j = 0; j < ppBuckets[i]->size; j++)
			      delete ppBuckets[i]->data[j];
		     delete ppBuckets[i];
		     ppBuckets[i] = NULL;
		 }
	    items = 0;
	}
}

void HashTable::moveAll(RCollection dest)
{
        if ( isSame(dest) )
            return;

	long prime = hash_primes[curBucket], i, j;

	if ( items > 0 ) {
	    for (i = 0; i < prime; i++)
		 if ( ppBuckets[i] ) {
		      for (j = 0; j < ppBuckets[i]->size; j++)
			   dest.add(ppBuckets[i]->data[j]);
		     delete ppBuckets[i];
		     ppBuckets[i] = NULL;
		 }
	    items = 0;
	}
}

void HashTable::capacity(sizeType newSize)
{
  PHashTableBucket*       ppNewBuckets; // Temporary variable

  long old_prime = hash_primes[curBucket]; // Get prime number
  long i;
  while (hash_primes[curBucket]*BUCKET_SIZE < newSize)
	 // Find prime big enough for number items
	 curBucket++;
 retry:
  long new_prime = hash_primes[curBucket]; // Get prime number
  ppNewBuckets = (PHashTableBucket*) new PHashTableBucket[new_prime];
  if (ppNewBuckets)
      for (i = 0; i < new_prime; i++)
           ppNewBuckets[i] = (PHashTableBucket) NULL;
  else
      error("capacity : memory allocation error");

  for (i = 0; i < old_prime; i++)
       // For each bucket count
       if ( ppBuckets[i] ) {
	   for (int j = 0; j < ppBuckets[i]->size; j++) {
		// For each item
		unsigned long hash =
                        getHashValue(*ppBuckets[i]->data[j]) % new_prime;

		if (ppNewBuckets[hash] == NULL) {
		    ppNewBuckets[hash] = new HashTableBucket();
		    ppNewBuckets[hash]->add(ppBuckets[i]->data[j]);
		}
		else
		    if (ppNewBuckets[hash]->size == BUCKET_SIZE) {
                        // Overflow bucket -- capacity

			// Delete allocated storage
			for (i=0; i < new_prime; i++)
			     if (ppNewBuckets[i])
				 delete ppNewBuckets[i];
			delete [] ppNewBuckets;

			curBucket++;    // Increment bucket count
			goto retry;     // Go retry again
		    }
		    else
			ppNewBuckets[hash]->add(ppBuckets[i]->data[j]);
	   }
      }

  // Free up old storage
  for (i=0; i < old_prime; i++)
	if (ppBuckets[i])
	    delete ppBuckets[i];
   delete [] ppBuckets;

   ppBuckets = ppNewBuckets;
}

void    HashTableIterator::restart()
//
// locate first object in table
//
{
        cur = pTable->size() + 1; // force int() to return false

	if (pTable->size() == 0)
	    return;
	else {
	    cur = 0;
	    prime = hash_primes[pTable->curBucket];
	    // locate first object
	    for (long i = 0; i < prime; i++)
		if (pTable->ppBuckets[i]) {
		    curBucket = i;
		    index = 0;
		    break;
		}
	}
}

PTObject  HashTableIterator::operator ++ ()
{
	if ( cur < pTable->size() ) {
	     cur++;
	     index++;
             if (index >= pTable->ppBuckets[curBucket]->size) {
                 // locate object in next bucket
                 for (long i = curBucket + 1; i < prime; i++)
		      if (pTable->ppBuckets[i]) {
                          curBucket = i;
			  index = 0;
                          break;
		      }
	     }
        }

        if ( cur >= pTable->size() )
             return NOOBJECT;
        else
             return pTable->ppBuckets[curBucket]->data[index];
}

int HashTable::compare( RCTObject obj ) const
{
        if ( obj.isA() == isA() ) {
                RCHashTable src = (RCHashTable) obj;
                PTObject pObj;
                int val;

                if ( src.size() > size() )
                    return -1;
                else
                if ( size() > src.size() )
                    return 1;

                CollectionIterator iter((RCollection) obj);
                while ( iter ) {
                        if ( ( pObj = includes( *iter() ) ) != NOOBJECT ) {
                             if ((val = pObj->compare(*iter())) != 0)
                                 return val;
                        }
                        else
                             return -1;
                        iter++;
                }
                return 0;
        }

        return -1;
}

#if ! defined ( __postfix_inc__ )
BOOL    HashTable::isEqual( RCTObject obj ) const
{
        if ( obj.isA() != isA() )
            return FALSE;
        else {
                if ( ((RCollection) obj).size() != size() )
                    return FALSE;
                CollectionIterator iter((RCollection) obj);
                while ( iter ) {
                        if ( includes( *iter() ) == NOOBJECT )
                            return FALSE;
                        else
                            ++iter;
                }
                return TRUE;
        }
}
#else
PTObject  HashTableIterator::operator ++ ( int )
{
        sizeType prevBucket;
        long     prevIndex;

        if (cur >= pTable->size())
            return NOOBJECT;
        else {
             prevBucket = curBucket;
             prevIndex  = index;
             cur++;
             index++;

	     if (index >= pTable->ppBuckets[curBucket]->size) {
                 // locate object in next bucket
		 for (long i = curBucket + 1; i < prime; i++)
		      if (pTable->ppBuckets[i]) {
                          curBucket = i;
			  index = 0;
                          break;
		      }
	     }
	}

	return pTable->ppBuckets[prevBucket]->data[prevIndex];
}

BOOL    HashTable::isEqual( RCTObject obj ) const
{
        if ( obj.isA() != isA() )
            return FALSE;
        else {
                if ( ((RCollection) obj).size() != size() )
                    return FALSE;
                CollectionIterator iter((RCollection) obj);
                while ( iter ) {
                        if ( includes( * (iter++) ) == NOOBJECT )
                            return FALSE;
                        else
                            ++iter;
                }
                return TRUE;
        }
}
#endif
