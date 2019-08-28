//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TTCLSF_H )
#define __TTCLSF_H

#include        "clsifier.h"
#include	"csfmatch.h"

class   TTMatchList : public MatchList
{
public:
	DECLARE_RTTI()

				TTMatchList(RClassifier srcCls);

                                _shallowCopy(TTMatchList)
                BOOL            isEqual( RCTObject ) const;
                void            deepenShallowCopy();
                void            printOn( ostream  & out) const;
                void            add(PCMessage, PCMessage);
                int             post(RMsgList) const;
                void            removeAll();
                sizeType        size() const;
                float           sumIntensities() const;
                RTTMatchList    operator=(RCTTMatchList obj);

protected:
        TOArray msgs1, msgs2;
};

class   TTClassifier :  public Classifier
//
//	a two taxa one condition classifier
//	by convention only the second condition can be
//	a negated condition so the length of the first
//	condition equals the message length and the
//	length of the second condition equals message
//	length plus 1
//
{
public:

                                DECLARE_RTTI()

				TTClassifier();
				TTClassifier(unsigned,
					     BOOL = ~ RANDOMIZE);
				TTClassifier(RCTTClassifier);
				TTClassifier(RTritString,
					     RTritString,
					     RTritString);

	virtual BOOL            isEqual( RCTObject ) const;
                                _shallowCopy(TTClassifier)
        virtual void            deepenShallowCopy();

	virtual void            printOn( ostream  & out) const;
	virtual void            randomize();
	virtual void            mutate(float flMRate);

        virtual RTTClassifier   operator=(RCTTClassifier);
//
	virtual void            oddPtCrossover(RCTGAObj, int,
					       RTGAObj) const;
	virtual void            evenPtCrossover(RCTGAObj, int,
						RTGAObj) const;
	virtual void            uniformCrossover(RCTGAObj, float,
						 RTGAObj) const;

	virtual RTritString     taxon(unsigned uIndex = 0);
	virtual RCTritString    taxon(unsigned uIndex = 0) const;
	virtual RTritString     action();
	virtual RCTritString    action() const;

	virtual RCMatchList     matchList() const;
	virtual int             matchMsgs(RCCollection);
	virtual BOOL		post(RMsgList, PCMessage, PCMessage, float);
	virtual unsigned        matchCount(RCClassifier cls) const;
	virtual float 		setSupport();

protected:
	TritString      taxon1, taxon2, act;
	TTMatchList     mlist;

	virtual void            findSpecificity();
	virtual unsigned        findLength();
	virtual void		setNegationBit();
};

inline TTClassifier::TTClassifier() : Classifier(), mlist(*this)
{
	findSpecificity();
	findLength();
}

inline TTClassifier::TTClassifier(unsigned msgLength,
				  BOOL flag) :
		     Classifier(), mlist(*this)
{
	taxon1.resize(msgLength);
	taxon2.resize(msgLength + 1);
	act.resize(msgLength);
	if (flag) {
	    taxon1.randomize();
	    taxon2.randomize();
	    setNegationBit();
	    act.randomize();
	}
	findSpecificity();
	findLength();
}

inline void TTClassifier::deepenShallowCopy()
{
}

inline void     TTClassifier::randomize()
{
	taxon1.randomize();
	taxon2.randomize();
	act.randomize();
        setNegationBit();
}

inline void     TTClassifier::mutate(float flMRate)
{
	taxon1.mutate(flMRate);
	taxon2.mutate(flMRate);
	act.mutate(flMRate);
	setNegationBit();
}

inline RTritString    TTClassifier::taxon(unsigned uIndex)
{
	if (uIndex > 0)
	    return taxon2;
	else
	    return taxon1;
}

inline RCTritString   TTClassifier::taxon(unsigned uIndex) const
{
	if (uIndex > 0)
	    return taxon2;
	else
	    return taxon1;
}

inline RTritString    TTClassifier::action()
{
	return act;
}

inline RCTritString   TTClassifier::action() const
{
	return act;
}

inline unsigned TTClassifier::matchCount(RCClassifier cls) const
{
//
//      count number of positions of similarity
//
	if (! cls.isKindOf(isA()))
	    return 0;

	return taxon1.matchCount(((RCTTClassifier) cls).taxon1) +
	       taxon2.matchCount(((RCTTClassifier) cls).taxon2);
}

inline unsigned  TTClassifier::findLength()
{
	//
	// the negation bit is not counted
	//
	return uLength = 3 * taxon1.length();
}

inline float TTClassifier::setSupport()
{
	//
	// calculate support for classifier
	//
	float	flOldVal = flSupport;
	flSupport = mlist.sumIntensities();
	return flOldVal;
}

inline RCMatchList TTClassifier::matchList() const
{
	return mlist;
}

inline void TTClassifier::setNegationBit()
{
	if (taxon2.valueAt(0) == (int) dontCare) {
	    // the value at this locus cannot be a don't care
	    if (tRand.flip(0.5))
		taxon2.setAt(0,1);
	    else
		taxon2.setAt(0,0);
	}
}

inline TTMatchList::TTMatchList(RClassifier srcCls) :MatchList(srcCls)
{
	if (! srcCls.isKindOf(TTClassifier::typeInfo()) )
	    error("Constructor error : invalid classifier type");
}

inline void TTMatchList::deepenShallowCopy()
{
}

inline void TTMatchList::printOn( ostream  & out) const
{
        MatchList::printOn(out);
        out << endl;
        msgs1.printOn(out);
        out << endl;
        msgs2.printOn(out);
}

inline void TTMatchList::add(PCMessage pMsg1, PCMessage pMsg2)
{
	msgs1.add((PTObject) pMsg1);
        msgs2.add((PTObject) pMsg2);
}

inline void TTMatchList::removeAll()
{
	msgs1.removeAll();
	msgs2.removeAll();
}

inline sizeType TTMatchList::size() const
{
	return msgs1.size();
}

inline RTTMatchList  TTMatchList::operator=(RCTTMatchList src)
{
        if (! isSame(src) ) {
	    msgs1 = src.msgs1;
	    msgs2 = src.msgs2;
	    pCls = src.pCls;
	    flStrength = src.flStrength;
	}
        return *this;
}

#endif

