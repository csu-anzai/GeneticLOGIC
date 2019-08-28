//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __ITERATOR_H )
#define    __ITERATOR_H

#include "tobject.h"

class Iterator
{
public:

    virtual             ~Iterator();

    virtual PTObject    operator ()() = 0;
    virtual PTObject    operator ++ () = 0;
#if defined( __postfix_inc__ )
    virtual PTObject    operator ++ (int) = 0;
#endif
    virtual void        restart() = 0;
    virtual 		operator int() = 0;
    virtual sizeType	indexOf() const = 0;
    virtual PIterator   deepCopy() const = 0;

protected:

    Iterator();
};

class CollectionIterator : public Iterator
{
public:
			CollectionIterator();
                        CollectionIterator(RCCollection);
                        CollectionIterator(RCCollectionIterator);
			~CollectionIterator();
    virtual PTObject    operator ()();
    virtual PTObject    operator ++ ();
#if defined( __postfix_inc__ )
    virtual PTObject    operator ++ (int);
#endif
    virtual void        restart();
    virtual void        operator = (RCCollectionIterator);
    virtual void        restart(RCCollection);
    virtual 		operator int();
    virtual sizeType	indexOf() const;
    virtual PIterator   deepCopy() const;

protected:
    PIterator pIter;
    PTObject  pCurObj;     // pointer to current object or NOOBJECT
};

inline  Iterator::Iterator()
{
}

inline  Iterator::~Iterator()
{
}

inline  CollectionIterator::CollectionIterator() :
        pIter(NULL), pCurObj(NOOBJECT)
{
}

inline  CollectionIterator::CollectionIterator(RCCollection src)
{
    pIter = src.initIterator();
    pCurObj = pIter->operator()();
}

inline  CollectionIterator::CollectionIterator(RCCollectionIterator src)
{
	if (src.pIter)
	    pIter = src.pIter->deepCopy();
	else
	    pIter = NULL;
	pCurObj = pIter->operator()();
}

inline CollectionIterator::~CollectionIterator()
{
    if (pIter)
        delete pIter;
}

inline PIterator CollectionIterator::deepCopy() const
{
        return new CollectionIterator(*this);
}

inline PTObject CollectionIterator::operator ()()
{
    return pCurObj;
}

inline PTObject  CollectionIterator::operator ++ ()
{
    return pCurObj = pIter->operator ++ ();
}

#if defined( __postfix_inc__ )
inline PTObject  CollectionIterator::operator ++ (int i)
{
    PTObject pObj = pIter->operator ++ (i);
    pCurObj = pIter->operator()();
    return pObj;
}
#endif

inline void CollectionIterator::restart()
{
    pIter->restart();
    pCurObj = pIter->operator()();
}

inline void CollectionIterator::restart(RCCollection cltn)
{
    if (pIter)
	delete pIter;
    pIter = cltn.initIterator();
    pCurObj = pIter->operator()();
}

inline void CollectionIterator::operator = (RCCollectionIterator src)
{
    if (pIter)
	delete pIter;
    pIter = src.pIter->deepCopy();
}

inline CollectionIterator::operator int()
{
	return pIter->operator int();
}

inline sizeType	CollectionIterator::indexOf() const
{
	return pIter->indexOf();
}

#endif

