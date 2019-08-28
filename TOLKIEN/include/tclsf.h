//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined    ( __TCLSF_H )
#define     __TCLSF_H

#include        "clsifier.h"
#include	"csfmatch.h"

//
//      A Stimulus-Response type classifier containing
//      one condition and one action
//
class   TMatchList : public MatchList
{
public:
	DECLARE_RTTI()

                                TMatchList(RCTClassifier);

                                _shallowCopy(TMatchList)
                BOOL            isEqual( RCTObject ) const;
                void            deepenShallowCopy();
                void            printOn( ostream  & out) const;
                void            add(PCMessage);
                int             post(RMsgList) const;
                void            removeAll();
                sizeType        size() const;
                float           sumIntensities() const;
                RTMatchList     operator=(RCTMatchList obj);

protected:
        TOArray  msgs;
};

class   TClassifier : public Classifier
{
public:

	DECLARE_RTTI()
				TClassifier(unsigned, unsigned,
					    BOOL = ~ RANDOMIZE);
				TClassifier(RCTClassifier);
				TClassifier(RTritString, RTritString);

				_shallowCopy(TClassifier)
	virtual void            deepenShallowCopy();

	virtual BOOL            isEqual( RCTObject ) const;
	virtual void            printOn( ostream  & out) const;
	virtual void            randomize();
	virtual void            mutate(float flMRate);

        virtual RTClassifier    operator=(RCTClassifier);

	virtual void            oddPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual void            evenPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual void            uniformCrossover(RCTGAObj,float,RTGAObj) const;

        virtual RTritString     taxon(unsigned uIndex = 0);
        virtual RCTritString    taxon(unsigned uIndex = 0) const;
        virtual RTritString     action();
        virtual RCTritString    action() const;

	virtual RCMatchList     matchList() const;
	virtual int		matchMsgs(RCCollection);

	virtual BOOL		post(RMsgList, PCMessage,
				     PCMessage, float);

	virtual unsigned        matchCount(RCClassifier cls) const;
	virtual float           setSupport();

protected:

	TritString              txn, act;
	TMatchList              mlist;

				TClassifier();
	virtual void            findSpecificity();
	virtual unsigned        findLength();
};

inline  TClassifier::TClassifier() : Classifier(), mlist(*this)
{
	findSpecificity();
        findLength();
}

inline	TClassifier::TClassifier(unsigned uMsgLength,
				 unsigned uActLength,
				 BOOL	  flag) :
				 mlist(*this)
{
        txn.resize(uMsgLength);
        act.resize(uActLength);

	if (flag == RANDOMIZE) {
            txn.randomize();
            act.randomize();
	}

	findSpecificity();
        findLength();
}

inline  void TClassifier::deepenShallowCopy()
{
}

inline  void TClassifier::randomize()
{
        txn.randomize();
        act.randomize();
}

inline  void TClassifier::mutate(float flMRate)
{
	txn.mutate(flMRate);
	act.mutate(flMRate);
}

inline RTritString    TClassifier::taxon(unsigned uIndex)
{
        return txn;
}

inline RCTritString   TClassifier::taxon(unsigned uIndex) const
{
        return txn;
}

inline RTritString    TClassifier::action()
{
        return act;
}

inline RCTritString   TClassifier::action() const
{
        return act;
}

inline unsigned       TClassifier::matchCount(RCClassifier cls) const
{
//
//      count number of positions of similarity
//
	if (! cls.isKindOf(isA()))
	    return 0;

	return txn.matchCount(((RCTClassifier) cls).txn);
}

inline  unsigned     TClassifier::findLength()
{
	return uLength = txn.length() + act.length();
}

inline float TClassifier::setSupport()
{
	//
	// calculate support for classifier
	//
	return flSupport = mlist.sumIntensities();
}

inline RCMatchList TClassifier::matchList() const
{
	return mlist;
}

inline TMatchList::TMatchList(RCTClassifier cls) : MatchList(cls)
{
}

inline void TMatchList::deepenShallowCopy()
{
}

inline void TMatchList::add(PCMessage pSrc)
{
	msgs.add((PTObject) pSrc);
}

inline void TMatchList::removeAll()
{
	msgs.removeAll();
}

inline sizeType TMatchList::size() const
{
	return msgs.size();
}

inline RTMatchList  TMatchList::operator=(RCTMatchList src)
{
	if (! isSame(src) ) {
	    msgs = src.msgs;
	    pCls = src.pCls;
	    flStrength = src.flStrength;
	}
        return *this;
}

#endif

