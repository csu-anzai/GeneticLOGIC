//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#if !defined ( __SLQUEUE_H )
#define    __SLQUEUE_H

#include "tosllist.h"

class SLQueue : public Collection
{
public:
				DECLARE_RTTI()

                                SLQueue(OwnerType dt = reference);
                                SLQueue(RCSLQueue, OwnerType dt = reference);

                                _shallowCopy(SLQueue)

  		void          	enq(PTObject  item);
  		PTObject        deq();
	  	RPTObject       front();
  		void          	del_front();

  		void          	clear();
               
        virtual BOOL            add(PTObject);
        virtual BOOL            isEmpty() const;
        virtual unsigned        occurrencesOf(RCTObject) const;

        virtual BOOL            isEqual( RCTObject ) const;
        virtual PTObject        at(sizeType) const;
        virtual PTObject        elem(sizeType) const;
        virtual PTObject        includes(RCTObject) const;
        virtual sizeType        size() const;
        virtual int             compare( RCTObject ) const;

        virtual void            setAt(sizeType, PTObject);
        virtual PTObject        insertAt(sizeType, PTObject);
	virtual RCollection     addContentsTo(RCollection) const;
	virtual RCCollection    removeCltn(RCCollection);
	virtual PTObject        remove(RCTObject);
	virtual PTObject        removeAt(sizeType);
	virtual void            removeAll();

	virtual PIterator       initIterator() const;

        friend class SLQueueIterator;

private:
	TObjSLList      p;
};

class SLQueueIterator : public Iterator
{
public:
                        SLQueueIterator(RSLQueue);
                        ~SLQueueIterator();
    virtual PTObject    operator ()();

#if defined ( __postfix_inc__ )
    virtual PTObject    operator ++ ( int );
#endif

    virtual PTObject    operator ++ ();
    virtual void        restart();
    virtual void        operator = (RCCollection cltn);
    virtual 		operator int();
    virtual sizeType	indexOf() const;

protected:

    TObjSLListIterator  *pIterator;
};

inline SLQueue::SLQueue(OwnerType dt) :p()
{
	ownsElements(dt);
}

inline SLQueue::SLQueue(RCSLQueue q, OwnerType dt) : p(q.p)
{
	ownsElements(dt);
}

inline void SLQueue::enq(PTObject item)
{
  	p.append(item);
}

inline PTObject SLQueue::deq()
{
       return p.remove_front();
}

inline RPTObject SLQueue::front()
{
       return p.front();
}


inline void SLQueue::del_front()
{
	if (ownsElements()) 
	   delete p.remove_front();
	else
  	    p.del_front();
}

inline BOOL SLQueue::isEmpty() const
{
	return p.isEmpty();
}

inline sizeType SLQueue::size() const
{
	return p.size();
}

inline void SLQueue::clear()
{
     if (ownsElements()) {
	 p.ownsElements(owner);
	 p.clear();
	 p.ownsElements(reference);
     }
     else
	 p.clear();
}

inline BOOL   SLQueue::add(PTObject pObj)
{
	enq(pObj);
}

inline unsigned   SLQueue::occurrencesOf(RCTObject obj) const
{
	return p.occurrencesOf(obj);
}

inline PTObject   SLQueue::at(sizeType index) const
{
	return p.at(index);
}

inline PTObject   SLQueue::elem(sizeType index) const
{
	return p.elem(index);
}

inline PTObject   SLQueue::includes(RCTObject obj) const
{
	return p.includes(obj);
}

inline BOOL  SLQueue::isEqual( RCTObject obj ) const
{
	if ( obj.isA() != isA() )
	    return FALSE;
	else
            return ((RCSLQueue) obj).p.isEqual(p);
}

inline int        SLQueue::compare( RCTObject obj) const
{
	if ( obj.isA() != isA() )
	    return -1;
	else
            return ((RCSLQueue) obj).p.compare(p);
}

inline  void      SLQueue::setAt(sizeType index, PTObject pObj)
{
	error("setAt not defined for queue");
}

inline  PTObject  SLQueue::insertAt(sizeType index, PTObject pObj)
{
	error("insertAt not defined for queue");
}

inline RCollection   SLQueue::addContentsTo(RCollection cltn) const
{
	p.addContentsTo(cltn);
}

inline RCCollection  SLQueue::removeCltn(RCCollection cltn)
{
	error("removeCltn not defined for queue");
}

inline PTObject      SLQueue::remove(RCTObject obj)
{
	error("remove not defined for queue");
}

inline PTObject      SLQueue::removeAt(sizeType index)
{
	error("removeAt not defined for queue");
}

inline void          SLQueue::removeAll()
{
     if (ownsElements()) {
	 p.ownsElements(owner);
	 p.removeAll();
	 p.ownsElements(reference);
     }
     else
	 p.removeAll();
}

inline SLQueueIterator::SLQueueIterator(RSLQueue src)
{
	pIterator = (TObjSLListIterator *) src.p.initIterator();
}

inline SLQueueIterator::~SLQueueIterator()
{
	delete pIterator;
}

inline PTObject SLQueueIterator::operator ()()
{
	return pIterator->operator()();
}

#if defined ( __postfix_inc__ )
inline PTObject SLQueueIterator::operator ++ ( int dummy)
{
	return pIterator->operator++(dummy);
}
#endif

inline PTObject SLQueueIterator::operator ++ ()
{
	return pIterator->operator++();
}

inline void SLQueueIterator::restart()
{
	pIterator->restart();
}

inline SLQueueIterator::operator int()
{
	return pIterator->operator int();
}

inline sizeType SLQueueIterator::indexOf() const
{
	return pIterator->indexOf();
}

inline  void    SLQueueIterator::operator = (RCCollection cltn)
{
        if (cltn.isKindOf(SLQueue::typeInfo())) {
	    delete pIterator;
	    pIterator =
               (TObjSLListIterator *) ((RCSLQueue) cltn).p.initIterator();
	}
	else
            cltn.error("SLQueueIterator can only be used on queues");
}

#endif

