//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "sortcltn.h"

SortedCltn::SortedCltn(RCSortedCltn cl, OwnerType ot) :
	contents(cl.contents, ot)
{
}

int SortedCltn::compare(RCTObject arg) const
// Compare two sequenced collections.  If *this > arg return >0,
// *this == arg return 0, and if *this < arg return <0.
{
	PRECONDITION(arg.isKindOf(Collection::typeInfo()));

	int nResult = -1;
        CollectionIterator i(*this);
        CollectionIterator j ((RCCollection) arg);
	PTObject p;      // pointer to next object in this SeqCltn
	PTObject q;      // pointer to next object in arg SeqCltn

	q = j();
        p = i();
	while (q && p) {
// previous elements compared equal; longer SeqCltn is therefore larger
                if (q == NOOBJECT) {
		    nResult = 1;
		    break;
		}
// compare() != 0 at any element determines ordering
		int val;
		if ((val = p->compare(*q)) != 0) {
		    nResult = val;
		    break;
		}
		i++;
		j++;
		q = j();
                p = i();
	}
// all elements in this SeqCltn compare() equal to arg SeqCltn
	if (q == 0) nResult = 0;	// size() == arg.size()

	return nResult;
}

sizeType    SortedCltn::findIndexOf(RCTObject key) const
{
//	This algorithm is adapted from Horowitz & Sahni,
//	"Fundamentals of Data Structures", 1976, Section 7.1,
//	algorithm BINSRCH.
//	This function will return (on failure to find):
//		(-1) if the key is less than all other keys; 
//		otherwise, returns i, such that i is the greatest index
//		whose key is less than the key value.
//	On successful search the algorithm returns an index to a
//	key which equals the key argument.  However, this is not
//	guaranteed to be the first key of a sequence, when such a
//	sequence exists.

	sizeType l = 0;
        sizeType u = size() - 1;
        sizeType m = 0;
        sizeType c;

        if (key.compare(contents[0]) < 0)
		return -1;

	while (l <= u) {
		m = (l + u) >> 1;
                if ((c = key.compare(contents[m])) > 0)
			l = m + 1;
		else
		if (c == 0)
			return m;
		else
			u = m - 1;
	}

//	Binary search will leave the final index searched either
//	just greater than the key or just less than, depending 
//	upon the relation of the search key to the existing
//	collection.  Must adjust here, in case it was placed just
//	to the right.

	if (m > 0 && c < 0)
		m--;

	return m; // Key not found.
}

BOOL SortedCltn::add(PTObject pObj)
{
        if (size()==0)                 // add first object to collection
            return contents.add(pObj);

        sizeType i = findIndexOf(*pObj);

        if (i == -1 || pObj->compare(contents[i]) != 0)
                contents.insertAt(i + 1, pObj);
	else
//              An object(s) equal to the argument "obj" already exists.
//		Add another one, after the sequence.
                contents.insertAt(findIndexOfLastKey(*pObj, i) + 1, pObj);

        return TRUE;
}

sizeType SortedCltn::findIndexOfLastKey(RCTObject key, sizeType index) const
{
        register sizeType max = size();
        for (register sizeType j = index + 1; j < max; j++)
                if (key.compare(contents[j]) != 0)
			break;
	return (j - 1);
}
