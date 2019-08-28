//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "collectn.h"

static  PCTypeInfo collectionBases[] = { &TObject::infoObj, 0 };
const TypeInfo  Collection::infoObj("Collection", collectionBases);

void Collection::printOn( ostream& outputStream) const
{
    CollectionIterator printIterator(*this);
    printHeader( outputStream );
    while ( printIterator )
	{
	printIterator()->printOn( outputStream );
        ++printIterator;

	if ( printIterator != 0 )
	    printSeparator( outputStream );
	else
	    break;
	}
    printTrailer( outputStream );
}

void Collection::printHeader( ostream& outputStream ) const
{
    outputStream << nameOf() << " {\n    ";
}

void Collection::printSeparator( ostream& outputStream ) const
{
    outputStream << ",\n    ";
}

void Collection::printTrailer( ostream& outputStream ) const
{
    outputStream << " }\n";
}

#if ! defined ( __postfix_inc__ )
RCCollection Collection::addAll(RCCollection from)
{
    if ( from.ownsElements() && ownsElements() )
	return *this;
    else {
	CollectionIterator fromIter(from);

	if ( !isSame(from) ) {
	     while ( fromIter ) {
		     add(fromIter());
		     ++fromIter;
	     }
	}
	else if (from.size() > 0) {
	     // copy myself
	     // iterator may not work for non-static collection

	     PTObject	*pData = new PTObject[from.size()];
	     register i = 0;

	     while ( fromIter ) {
		     pData[i++] = fromIter();
		     ++fromIter;
	     }

	     if ( ownsElements() )
		for (i=0; i < from.size(); i++)
		     add(pData[i]->deepCopy());
	     else
		for (i=0; i < from.size(); i++)
		     add(pData[i]);

	     delete pData;
	}
    }

    return *this;
}

RCollection Collection::addContentsTo(RCollection dest) const
{
    if ( isSame(dest) ||
         ( dest.ownsElements() && ownsElements() ) )
	return dest;

    CollectionIterator thisIter(*this);

    while ( thisIter ) {
	   dest.add(thisIter());
	   ++thisIter;
    }
    return dest;
}

RCCollection Collection::removeCltn(RCCollection from)
{

    if ( isSame(from) )
	removeAll();
    else {
	CollectionIterator fromIter(from);

	while ( fromIter ) {
	   remove( *fromIter() );
	   ++fromIter;
	}
    }
    return *this;
}
#else
RCCollection Collection::addAll(RCCollection from)
{

    if ( from.ownsElements() && ownsElements() )
	return *this;
    else {
	CollectionIterator fromIter(from);

	if ( !isSame(from) ) {
	     while ( fromIter )
		     add(fromIter++);
		     ++fromIter;
	}
	else if (from.size() > 0) {
	     // copy myself
	     // iterator may not work for non-static collection

	     PTObject	*pData = new PTObject[from.size()];
	     register i = 0;

	     while ( fromIter )
		     pData[i++] = fromIter++;


	     if ( ownsElements() )
		for (i=0; i < from.size(); i++)
		     add(pData[i]->deepCopy());
	     else
		for (i=0; i < from.size(); i++)
		     add(pData[i]);

	     delete pData;
	}
    }

    return *this;

}

RCollection Collection::addContentsTo(RCollection dest) const
{
    if ( isSame(dest) ||
         ( dest.ownsElements() && ownsElements() ) )
        return dest;

    CollectionIterator thisIter(*this);

    while ( thisIter )
           dest.add(thisIter++);

    return dest;
}

RCCollection Collection::removeCltn(RCCollection from)
{

    if ( isSame(from) )
	removeAll();
    else {
	CollectionIterator fromIter(from);

	while ( fromIter )
		remove( * fromIter++ );
    }

    return *this;
}
#endif
