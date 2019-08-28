//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "toarray.h"
#include <string.h>
#include <limits.h>

static  PCTypeInfo toarrayBases[] = { &Collection::infoObj, 0 };
const TypeInfo  TOArray::infoObj("TOArray", toarrayBases);

TOArray::TOArray(RCTOArray src, OwnerType ot) :
		 curSz(src.curSz),
		 maxSz(src.maxSz),
		 uGrowSz(src.uGrowSz)
{
	ownsElements(reference);

	register sizeType i;

	if ( maxSz != 0 ) {
	     pData = (PTObject *) new PTObject [maxSz];
	     if (ot == reference)
		 memcpy(pData, src.pData, curSz * sizeof(PTObject));
	     else
		 for (i=0; i < curSz; i++)
		      pData[i] = src.pData[i]->deepCopy();
	}
	else {
	     pData = NULL;
	     curSz = 0;
        }

        if (ot == owner)
            deepenShallowCopy();
}

TOArray::TOArray(OwnerType ot,
                 sizeType  size,
                 unsigned  uGrowBy)
{
     curSz = 0;
     maxSz = (size <= 0 ? 0 : size);
     uGrowSz = uGrowBy;

     if (maxSz > 0) {
	if ((pData = (PTObject *) new PTObject [maxSz]) == NULL)
	    error("constructor : Memory allocation error!");
     }
     else
	pData = NULL;

     ownsElements(ot);
}

TOArray::TOArray(RCSubArray subarray, OwnerType ot) 
{
	maxSz = curSz = subarray.len;

	if (maxSz > 0) {
	    if ((pData = (PTObject *) new PTObject [maxSz]) == NULL)
		error("constructor : Memory allocation error!");
	    else
		memcpy(pData,
		       &(subarray.segment.pData[subarray.pos]),
		       curSz * sizeof(PTObject));
	}
	else
	    pData = NULL;

	if (ot == owner)
	    deepenShallowCopy();
	else
	    ownsElements(ot);
}

TOArray::~TOArray()
{
        if (pData) {
            removeAll();
            delete [] pData;
        }
}

void    TOArray::deepenShallowCopy()
{
	if (ownsElements())
            return;
        else {
            for (register sizeType i = 0; i < curSz; i++)
		 pData[i] = pData[i]->deepCopy();
            ownsElements(owner);
	}
}

BOOL    TOArray::grow()
{
	if (uGrowSz == 0)
	    return FALSE;

	PTObject    *pNewData;
	if ((pNewData = (PTObject *)
			  new PTObject[maxSz + uGrowSz]) != NULL) {
	    if (pData != NULL) {
		memcpy(pNewData, pData, curSz * sizeof(PTObject));
		delete [] pData;
	    }
	    pData = pNewData;
	    maxSz += uGrowSz;
            return TRUE;
	}
	else
           return FALSE;
}

PTObject  TOArray::insertAt(sizeType pos, PTObject pNewObj)
//
//      insert an object at pos
//
{
       if (pos >= curSz) {  // pos pass end of array, just append obj
           add(pNewObj);
           return pNewObj;
       }
       else {
           if (curSz >= maxSz)
               if (! grow()) {
                   cout << nameOf() << "Cannot expand array!";
                   return FALSE;
               }
           memmove(&pData[pos+1], &pData[pos],
                   (curSz - pos) * sizeof(PTObject));
           pData[pos] = pNewObj;
           curSz++;
           return pData[pos];
       }
}

BOOL    TOArray::add(PTObject pNewObj)
{
	if (curSz >= maxSz)
	    if (! grow()) {
		cout << nameOf() << "Cannot expand array!";
		return FALSE;
	    }

	pData[curSz] = pNewObj;
	curSz++;
        return TRUE;
}

void TOArray::capacity(sizeType newCapacity)
{
	PTObject   *pNewArray;
	register sizeType   i;

        if ((growSize() == 0) && newCapacity > maxSz)
            return;

        if (newCapacity <= 0) {
            newCapacity = 0;
	    if (delObj())
                for (i = 0; i < curSz; i++)
		     delete pData[i];
            delete [] pData;
            pData = NULL;
            curSz = maxSz = 0;
            return;
        }

        if (newCapacity < curSz) {
            if (delObj())
                for (i = newCapacity; i < curSz; i++)
                     delete pData[i];
            curSz = newCapacity;
        }

        if ((pNewArray = (PTObject *) new PTObject[newCapacity]) == NULL) {
            error("capacity : memory allocation error");
            return;
        }
        memcpy(pNewArray, pData, curSz * sizeof(PTObject));
        maxSz = newCapacity;
        delete [] pData;
	pData = pNewArray;
}

BOOL  TOArray::isEqual( RCTObject obj) const
{
        if ( obj.isA() != isA() )
            return FALSE;
	else {
		sizeType i;
		if (( i = ((RCollection) obj).size()) != size())
                    return FALSE;
                else {
                    register PTObject *a = pData;
                    register PTObject *b = ((RTOArray) obj).pData;
                    while (i--)
                        if (!((*a++)->isEqual(**b++)))
                            return FALSE;
                    return TRUE;
                }
        }
}

int      TOArray::compare( RCTObject obj ) const
{
        if ( obj.isA() == isA() ) {
                RCTOArray src = (RCTOArray) obj;
                int retval;
                for (sizeType i=0; i < size(); i++) {
                     if (i == src.size())  // this array is longer
                         return 1;
                     int retval;
                     if ((retval = elem(i)->compare(*src.elem(i))) != 0)
                          return retval;
                }
                if (size() == src.size())
                    return 0;
        }
        return -1;
}

unsigned TOArray::occurrencesOf(RCTObject obj) const
{
	unsigned res = 0;

        for (register sizeType i=0; i < curSz; i++)
             if (pData[i]->isEqual(obj)) res++;
	return res;
}

PTObject TOArray::includes(RCTObject obj) const
{
        for (register sizeType i = 0; i < curSz; i++)
             if (pData[i]->isEqual(obj))
                 return pData[i];
        return NOOBJECT;
}

void     TOArray::removeAll()
{
	if (delObj())
            for (register sizeType i=0; i < curSz; i++) {
                delete pData[i];
                pData[i] = NOOBJECT;
	    }
	curSz = 0;
}

PTObject TOArray::remove(RCTObject obj)
{
	PTObject pObj;
	for (register sizeType i=0; i < curSz; i++)
	     if (pData[i]->isEqual(obj)) {
		pObj = pData[i];
/*
		for (register sizeType j = i; j < curSz; j++)
		     pData[j] = pData[j+1];
*/
		memmove(&pData[i], &pData[i+1],
			(curSz - i - 1) * sizeof(PTObject));
		curSz--;
		return pObj;
	     }
        return NOOBJECT;
}

PTObject TOArray::removeAt(sizeType index)
{
	PRECONDITION(index < curSz);

	if (curSz == 0)
            return NOOBJECT;

	PTObject pObj = pData[index];

        pData[index] = NOOBJECT;
/*
	for (register sizeType i = index; i < curSz; i++)
	     pData[i] = pData[i+1];
*/
	memmove(&pData[index], &pData[index+1],
		(curSz - index - 1) * sizeof(PTObject));
	curSz--;
	return pObj;
}

RCollection TOArray::addContentsTo(RCollection dest) const
{
	if ( dest.isSame(*this) ||
	     ( dest.ownsElements() && ownsElements() ) )
            return dest;

	for (sizeType i = 0; i < curSz; i++)
	     dest.add(pData[i]);
	return dest;
}

RTOArray TOArray::operator=(RCTOArray src)
{
        if (! isSame(src) ) {
            removeAll();
            capacity(src.size());
	    if ( src.size() > 0 &&
		 capacity() >= src.size() ) {
		curSz = src.size();
		memcpy(pData, src.pData, curSz * sizeof(PTObject));
		if ( ownsElements() )
		     for (register i = curSz - 1; i >= 0; i--)
			  pData[i] = pData[i]->deepCopy();
            }
        }
        return *this;
}

RTOArray TOArray::operator=(RCSubArray src)
{
        SubArray sub(*this, 0, curSz);
        sub = src;
        return *this;
}

void TOArray::freeExtra()
//
//	release the unused memory locations
//
{
	if ((curSz > 0) && (maxSz > curSz)) {
	   PTObject *pNewData;
	   if ((pNewData = (PTObject *)
			    new PTObject[curSz]) != NULL) {
	       if (pNewData != NULL) {
		   memcpy(pNewData, pData, curSz * sizeof(PTObject));
		   delete [] pData;
		   pData = pNewData;
		   maxSz = curSz;
	       }
	   }
	}
}

void    SubArray::operator= (RCTOArray src)
//
//
//	when assignment invoked only one TOArray instance
//	and the array is owner of its elements,
//	the case when pos < len requires special attention :
//
//
//       ---------------------------------------------------
//      |   A    | B  |   DEL   |                           |  SRC
//       ---------------------------------------------------
//      ^        ^              ^
//      |
//      |       pos
//      |
//               v              v
//      0
//                ---------------
//               |     A   |  B  |                             DEST
//                ---------------
//
//               < ---  len --- >
//
//
//
//	       DEL is the segment that will be destroyed by the
//	       assignment, the length of DEL equals pos
//
//	       after the segment is moved, the pointers in A
//	       is deep copied
//
{
        register sizeType i, j, k, copylen;

	if (len > src.size())  // find the actual length of segment to be copied
	    copylen = src.size();
        else
            copylen = len;

        if (copylen == 0)
            return;

        if ( segment.isSame( (RCTObject) src) ) {

             if (pos == 0 && len == copylen)
                 // maximally overlapped, performs nothing
                 return;

	     //
	     // copy pointers, src and dest may overlap
	     //
	     if ( segment.ownsElements() ) {
		if (pos < len) {
		   //
		   // src and dest overlap
		   //
		   // delete the part of segement that
		   // is destroyed by the assignment
		   for (i = copylen, j = pos + copylen;
			i < j;
			i++)
			delete segment.pData[i];

		   //
		   // move the overlapped segment
		   //
		   memmove(&(segment.pData[pos]),
			   segment.pData,
			   len * sizeof(PTObject));
		   //
		   //  deep copied the dangling pointers
		   //
		   for (i=pos, j=pos+pos; i < j; i++)
			segment.pData[i] = segment.pData[i]->deepCopy();
		}
		else // not overlapping, deepcopy the whole segment
		    for (i=pos, j= len - 1, k = 0; j >= 0; j--, i++, k++) {
			 // delete object at dest
			 delete segment.pData[i];
			 // make a deep copy of src at dest
			 segment.pData[i] = segment.pData[k]->deepCopy();
		    }
	     }
	     else // just move segment of pointers
		  memmove(&(segment.pData[pos]),
			  segment.pData,
			  copylen * sizeof(PTObject));
	}
	else {
	     if ( segment.ownsElements() )
		  for (i = pos, j = pos + copylen, k = 0;
		       i < j; i++, k++) {
		       delete segment.pData[i];
		       segment.pData[i] = src.pData[k];
		  }
	     else
		memcpy(&(segment.pData[pos]), src.pData,
			copylen * sizeof(PTObject));
	}
}

void    SubArray::operator = (RCSubArray src)
{
	register sizeType i, j, k, copylen;

	//
	// find the actual length of segment to be copied
	// the shorter length is used
	//
	if (len > src.len)
	    copylen = src.len;
	else
	    copylen = len;

	if (copylen == 0)
	    return;

	if ( segment.isSame((RCTObject) src.segment) ) {

	     if (src.pos == pos && src.len == copylen)
		 // maximally overlapped, performs nothing
		 return;


	     if ( segment.ownsElements() ) {
		 if ( (pos < src.pos) &&
		      (pos + copylen > src.pos) ) { // segments overlapped
		    // delete the sub-segment that will be destroyed
		    // by the assignment
		    for (i=pos; i < src.pos; i++)
			 delete segment.pData[i];
		    //
		    // move the overlapped segment
		    //
		    memmove(&(segment.pData[pos]),
                            &(segment.pData[src.pos]),
			    copylen * sizeof(PTObject));
		    //
		    //  deep copied the dangling pointers
		    //
		    for (i = src.pos + 1, j = pos + copylen; i < j; i++)
			 segment.pData[i] = segment.pData[i]->deepCopy();
		 }
		 else
		     if ( (src.pos < pos) &&
			  (src.pos + copylen > pos) ) { // segments overlapped
			// delete the sub-segment that will be destroyed
			// by the assignment
			for (i = src.pos + copylen, j = pos + copylen;
			     i < j;
			     i++)
			     delete segment.pData[i];
			//
			// move the overlapped segment
			//
			memmove(&(segment.pData[pos]),
                                &(segment.pData[src.pos]),
				copylen * sizeof(PTObject));
			//
			//  deep copied the dangling pointers
			//
			for (i=pos, j = pos + pos - src.pos;
			     i < j; i++)
			     segment.pData[i] = segment.pData[i]->deepCopy();
		     }
                     else   // no overlap
			for (i=pos, j=src.pos, k=copylen-1;
			     k >= 0;
			     i++, j++, k--) {
			     delete segment.pData[i];
			     segment.pData[i] = segment.pData[j]->deepCopy();
			}
	     }
	     else
		memmove(&(segment.pData[pos]),
			&(src.segment.pData[src.pos]),
			copylen * sizeof(PTObject));
	}
	else // ! isSame(src)
	     if ( segment.ownsElements() )
		 for (i=pos, j=src.pos, k=copylen-1;
		      k >= 0;
		      i++, j++, k--) {
		      delete segment.pData[i];
		      segment.pData[i] = src.segment.pData[j]->deepCopy();
	     }
	     else
		memmove(&(segment.pData[pos]),
			&(src.segment.pData[src.pos]),
                        copylen * sizeof(PTObject));
}

SubArray TOArray::at(sizeType pos, sizeType len) const
{
        return SubArray(*this, pos, len);
}

RTOArray TOArray::operator+=(RCTOArray src)
{
        sizeType newSz =  curSz + src.curSz;
        if (! isSame(*this) ) {
            capacity(newSz);
            if (maxSz >= newSz)
                memcpy(&(pData[curSz]),
                       src.pData,
                       src.curSz * sizeof(PTObject));
        }
        else { // same array
                PTObject *pNewArray;
                if ((pNewArray = (PTObject *) new PTObject[newSz]) == NULL) {
		    error("operator+= : memory allocation error");
		    exit(-1);
		}
                memcpy(pNewArray,
                       pData,
                       curSz * sizeof(PTObject));
                memcpy(&(pNewArray[curSz]),
                       src.pData,
                       src.curSz * sizeof(PTObject));
                delete [] pData;
                pData = pNewArray;
        }
        if (ownsElements())
            for (register i = curSz; i < newSz; i++)
                 pData[i] = pData[i]->deepCopy();
        curSz = newSz;
        return *this;
}

RTOArray TOArray::operator+=(RCSubArray src)
{
	sizeType newSz =  curSz + src.len;

        if (! isSame(*this) ) {
            capacity(newSz);
            if (maxSz >= newSz)
                memcpy(&(pData[curSz]),
                       &(src.segment.pData[src.pos]),
                       src.len * sizeof(PTObject));
        }
	else {  // same array
                PTObject *pNewArray;
                if ((pNewArray = (PTObject *) new PTObject[newSz]) == NULL) {
		    error("operator+= : memory allocation error");
		    exit(-1);
		}
		memcpy(pNewArray,
		       pData,
		       curSz * sizeof(PTObject));
		memcpy(&(pNewArray[curSz]),
		       &(src.segment.pData[src.pos]),
                       src.len * sizeof(PTObject));
                delete [] pData;
                pData = pNewArray;
        }
        if (ownsElements())
            for (register i = curSz; i < newSz; i++)
                 pData[i] = pData[i]->deepCopy();
        curSz = newSz;
        return *this;
}

RCCollection TOArray::addAll(RCCollection src)
//
//      add all elements in src to this array
//
{
        CollectionIterator iter;

        if ( ownsElements() ) {
            // deep copy data from src
	    capacity(curSz + src.size());
            iter.restart(src); // set iterator on collection after calling
                               // 'capacity' makes it safe even src is
                               // the same collection as *this
            while ( iter ) {
#if !defined (__BORLANDC__)
                  pData[curSz++] = iter()->deepCopy();
#else
                  pData[curSz] = iter()->deepCopy();
                  curSz++;
#endif
		  ++iter;
            }
        }
	else {
	    //
	    // expand array to accomodate the elements in 'from'
	    //
            capacity(curSz + src.size());
            iter.restart(src); // set iterator on collection after calling
                               // 'capacity' makes it safe even src is
                               // the same collection as *this
             while ( iter ) {
#if !defined (__BORLANDC__)
                  pData[curSz++] = iter();
#else
                  pData[curSz] = iter();
                  curSz++;
#endif
                  ++iter;
	    }
	}
	return *this;
}
