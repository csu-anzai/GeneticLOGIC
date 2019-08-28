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
#if !defined ( __HASHTABL_H )
#define    __HASHTABL_H

#include "collectn.h"

extern long hash_primes[];

#define BUCKET_SIZE     8

_CLASSDEF(HashTableBucket)

class   HashTableBucket
{
public :
        HashTableBucket() : size(0) {}
        HashTableBucket(const HashTableBucket & src) : size(src.size)
        {
                memcpy(data, src.data, sizeof(PTObject) * BUCKET_SIZE);
        }
        BOOL    add(PTObject pObj)
        {
		if (size < BUCKET_SIZE) {
                    data[size] = pObj;
                    size++;
		    return TRUE;
                }
                return FALSE;
        }

        PTObject        data[BUCKET_SIZE];
        unsigned char   size;
};

class HashTable : public Collection
{
public:

	DECLARE_RTTI()

				HashTable(OwnerType = reference);
				HashTable(RCHashTable , OwnerType = reference);
				~HashTable();

				_shallowCopy(HashTable)
	virtual void 		deepenShallowCopy();

	virtual BOOL 		isEmpty() const;
        virtual BOOL            isEqual( RCTObject ) const;
	virtual unsigned 	occurrencesOf(RCTObject) const;
        virtual PTObject        includes(RCTObject) const;
        virtual int             compare( RCTObject obj ) const;

	virtual BOOL 		add(PTObject);
	virtual PTObject   	remove(RCTObject);
	virtual PTObject  	at(sizeType) const;
	virtual PTObject   	elem(sizeType) const;

	virtual void       	setAt(sizeType, PTObject);
	virtual PTObject   	insertAt(sizeType, PTObject);
	virtual PTObject        removeAt(sizeType);
	virtual void       	removeAll();
	virtual void            moveAll(RCollection);

	virtual sizeType   	size() const;

	virtual PIterator       initIterator() const;

	friend class  HashTableIterator;

protected:

        HashTableBucket**       ppBuckets;
	sizeType                items;
	unsigned                curBucket;

        virtual void            capacity(sizeType);

        unsigned long           getHashValue(RCTObject) const;
};

class   HashTableIterator : public Iterator
{
public :
				HashTableIterator(RCHashTable);
				HashTableIterator(RCHashTableIterator);
	virtual PTObject        operator ()();

#if defined ( __postfix_inc__ )
	virtual PTObject        operator ++ ( int );
#endif
	virtual PTObject        operator ++ ();
	virtual void            restart();
	virtual void            restart(RCHashTable);

	virtual 		operator int();
	virtual sizeType	indexOf() const;
	virtual PIterator   	deepCopy() const;

protected:
	PCHashTable         pTable;
	sizeType            curBucket;
	long                index;      // index to curBucket
	long                prime;      // number of buckets in table
	sizeType            cur;
};

inline sizeType HashTable::size() const
{
	return items;
}

inline BOOL HashTable::isEmpty() const
{
	return items > 0 ? FALSE : TRUE;
}

inline PTObject HashTable::elem(sizeType index) const
{
	return at(index);
}

inline PIterator HashTable::initIterator() const
{
	return new HashTableIterator((RHashTable) *this);
}

inline unsigned long HashTable::getHashValue(RCTObject obj) const
//
//      This is the default hash function of the COOL class library
//
{
	unsigned long key = (unsigned long) & obj;
	return (key >> 2);
}

inline HashTableIterator::HashTableIterator(RCHashTable table) :
	pTable(& table)
{
	restart();
}

inline HashTableIterator::HashTableIterator(RCHashTableIterator src) :
	pTable(src.pTable),
	curBucket(src.curBucket),
	index(src.index),
	prime(src.prime),
	cur(src.cur)
{
}

inline PIterator HashTableIterator::deepCopy() const
{
	return new HashTableIterator(*this);
}

inline HashTableIterator::operator int()
{
	return cur < pTable->size();
}

inline sizeType	HashTableIterator::indexOf() const
{
	return cur;
}

inline PTObject HashTableIterator::operator ()()
{
	return cur < pTable->size() ?
                pTable->ppBuckets[curBucket]->data[index] : NOOBJECT;
}

inline void HashTableIterator::restart(RCHashTable table)
{
	pTable = (PHashTable) & table;
	restart();
}

#endif

