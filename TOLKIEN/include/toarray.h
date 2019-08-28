//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TOARRAY_H )

#define __TOARRAY_H

#include "collectn.h"
#include "errhndl.h"

//
//      TOArray is an resizeable array of TObject instances.
//
//      The size of the array indicates the number of elements contained.
//
//      The capacity of the array is the storage allocated for
//      storing data.  The capacity of the array is larger than
//      its size unless the array is full.
//
//      The grow size of the array is the storage to be expanded
//      in case an element is added to a full array.
//
//
class   SubArray
{
        friend class      TOArray;

		    SubArray(RTOArray, sizeType, sizeType);
                    SubArray(RCTOArray, sizeType, sizeType);
                    SubArray(RCSubArray);

public:

  void              operator =  (RCTOArray);
  void              operator =  (RCSubArray);

  sizeType               length() const;
  BOOL                   empty() const;

protected:

	RTOArray     segment;
        sizeType     pos;
        sizeType     len;

};

class   TOArray : public Collection
{
            friend class SubArray;

public:

	DECLARE_RTTI()

                                TOArray(OwnerType = reference,
                                        sizeType = DEFAULT_COLLECTION_SIZE,
					unsigned = DEFAULT_GROW_SIZE);
				TOArray(RCTOArray, OwnerType = reference);
                                TOArray(RCSubArray, OwnerType = reference);
				~TOArray();

				_shallowCopy(TOArray)
	virtual void            deepenShallowCopy();

	virtual int             compare( RCTObject ) const;
        virtual PTObject        includes( RCTObject ) const;
	virtual BOOL            isEmpty() const;
        virtual BOOL            isEqual( RCTObject ) const;
        virtual unsigned        occurrencesOf( RCTObject ) const;

	virtual PTObject        at(sizeType index) const;
        virtual SubArray        at(sizeType pos, sizeType len) const;
	virtual PTObject        elem(sizeType index) const;
	virtual RTObject        operator[](sizeType index);
	virtual RCTObject       operator[](sizeType index) const;

	virtual PTObject        insertAt(sizeType, PTObject);
	virtual void            setAt(sizeType index, PTObject pNewObj);

	virtual BOOL            add(PTObject);
	virtual RCCollection    addAll(RCCollection);
	virtual RCollection 	addContentsTo(RCollection) const;

	virtual PTObject        remove(RCTObject);
	virtual PTObject        removeAt(sizeType);
	virtual void            removeAll();

        virtual RTOArray        operator=(RCSubArray);
        virtual RTOArray        operator=(RCTOArray);
        virtual RTOArray        operator+=(RCTOArray);
        virtual RTOArray        operator+=(RCSubArray);

	virtual void		freeExtra();

	virtual sizeType        size() const;

	virtual sizeType        capacity() const;
	virtual void            capacity(sizeType);

        virtual BOOL            grow();
        virtual BOOL            grow(unsigned uGrow);
        virtual unsigned        growSize() const;
        virtual void            growSize(unsigned uNewGrow);


	virtual void            sort(COMPAREFUNC func);
	virtual void            sort();

	virtual PIterator 	initIterator() const;
	virtual PIterator 	initIterator(sizeType, sizeType) const;

	friend class TOArrayIterator;

protected:
	sizeType  curSz;             // the current size of the array
	sizeType  maxSz;             // the maximum size, if any, of the array
	unsigned  uGrowSz;           // the fixed number of elements to grow
	PTObject  *pData;            // array of pointers to TObject
};

class TOArrayIterator : public Iterator
{
public:

	    TOArrayIterator(RTOArray);
	    TOArrayIterator(RTOArray, sizeType, sizeType);
	    TOArrayIterator(RCTOArrayIterator);

	    ~TOArrayIterator();

    virtual PTObject    operator ()();
    virtual PTObject    operator ++ ();
#if defined( __postfix_inc__ )
    virtual PTObject    operator ++ (int);
#endif

    virtual void 	restart( sizeType start, sizeType stop );
    virtual void 	restart();
    virtual sizeType 	indexOf() const;
    virtual 	 	operator int();
    virtual PIterator   deepCopy() const;

protected:

    PTOArray            pArray;
    sizeType            cur;
    sizeType            lower, upper;
};

inline  BOOL TOArray::grow(unsigned uGrow) // override uGrowSz once
{
	BOOL flag;
	unsigned uOldGrowSz = uGrowSz;

        growSize(uGrow);
	flag = grow();
        growSize(uOldGrowSz);  // restore grow size
	return flag;
}

inline  PTObject TOArray::at(sizeType index) const
{
	ASSERT(index < curSz);
	return pData[index];
}

inline  PTObject  TOArray::elem(sizeType index) const
{
	return pData[index];
}

inline  void TOArray::setAt(sizeType index, PTObject pNewObj)
{
	ASSERT(index < curSz);
	if (delObj())
	   delete pData[index];
	pData[index] = pNewObj;
}

inline  RTObject   TOArray::operator[](sizeType index)
{
	return *at(index);
}

inline  RCTObject  TOArray::operator[](sizeType index) const
{
        return * (PCTObject) at(index);
}

inline  unsigned   TOArray::growSize() const
{
	return uGrowSz;
}

inline  void TOArray::growSize(unsigned uNewGrow)
{
	uGrowSz = uNewGrow;
}

inline  BOOL  TOArray::isEmpty() const
{
        return curSz == 0 ? TRUE : FALSE;
}

inline  sizeType  TOArray::size() const
{
	return curSz;
}

inline  sizeType  TOArray::capacity() const
{
	return maxSz;
}

inline  void    TOArray::sort(COMPAREFUNC func)
{
	qsort((void *) pData, curSz, sizeof(PTObject), func);
}

inline  void    TOArray::sort()
{
	qsort((void *) pData, curSz, sizeof(PTObject), compare_ob);
}

inline  PIterator TOArray::initIterator() const
{
	return new TOArrayIterator((RTOArray) *this);
}

inline  PIterator TOArray::initIterator(sizeType start,
					sizeType stop) const
{
	return new TOArrayIterator((RTOArray) *this, start, stop);
}

inline  TOArrayIterator::TOArrayIterator(RTOArray src) : pArray(&src)
{
	restart(0, src.size());
}

inline  TOArrayIterator::TOArrayIterator(RTOArray src,
					 sizeType start,
					 sizeType stop) :
					 pArray(&src)
{
	restart(start, stop);
}

inline  TOArrayIterator::TOArrayIterator(RCTOArrayIterator src) :
	pArray(src.pArray), cur(src.cur),
	lower(src.lower), upper(src.upper)
{
}

inline  PIterator TOArrayIterator::deepCopy() const
{
	return new TOArrayIterator(*this);
}

inline  TOArrayIterator::~TOArrayIterator()
{
}

inline  PTObject TOArrayIterator::operator ()()
{
	return (cur < upper) ?
                pArray->pData[cur] : NOOBJECT;
}

#if defined( __postfix_inc__ )
inline PTObject  TOArrayIterator::operator ++ ( int )
{
	if( cur >= upper )
            return NOOBJECT;
	else
	    return pArray->pData[cur++];
}
#endif

inline PTObject TOArrayIterator::operator ++ ()
{
	if( cur < upper )
	    cur++;
	if( cur >= upper )
            return NOOBJECT;
	else
	    return pArray->pData[cur];
}

inline	void TOArrayIterator::restart( sizeType start, sizeType stop )
{
	if (pArray->size() == 0) {
	    cur = 0;
            upper = -1;  // force operator()() to return FALSE
	}
	cur = lower = start;
	upper = stop;
}

inline	void TOArrayIterator::restart()
{
	cur = lower = 0;
	if ((upper = pArray->size()) == 0)
	    upper = -1;  // force operator()() to return FALSE
}

inline	sizeType TOArrayIterator::indexOf() const
{
	return cur < upper ? cur : pArray->size();
}

inline TOArrayIterator::operator int()
{
	return cur < upper;
}

inline SubArray::SubArray(const SubArray& x) :
        segment(x.segment), pos(x.pos), len(x.len)
{
}

inline SubArray::SubArray(RTOArray x, sizeType p, sizeType l) :
	segment(x)
{
	pos = (sizeType) rangeCheck(p, 0, x.size() - 1, 0);
	len = l;
	if (pos + l > x.size())
	    len = x.size() - pos;

}

inline SubArray::SubArray(RCTOArray x, sizeType p, sizeType l) :
	segment((RTOArray) x)
{
	pos = (sizeType) rangeCheck(p, 0, x.size() - 1, 0);
	len = l;
	if (pos + l > x.size())
	    len = x.size() - pos;

}

inline  sizeType  SubArray::length() const
{
	return len;
}

inline  BOOL SubArray::empty() const
{
        return len == 0;
}

#endif

