//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
//
#if !defined( __CLSIFIER_H )
#define __CLSIFIER_H

#include <iostream.h>
#include "tobject.h"
#include "tritstr.h"
#include "toarray.h"
#include "tosllist.h"

class   Classifier : public TGAObj
{
public:

	DECLARE_RTTI()

	virtual BOOL            isEqual( RCTObject ) const = 0;
	virtual void            printOn( ostream  & ) const = 0;
	virtual PTObject        shallowCopy() const = 0;
	virtual void            deepenShallowCopy() = 0;
//
        virtual PTGAObj         oddPtCrossover(RCTGAObj, int) const;
	virtual void            oddPtCrossover(RCTGAObj, int,
					       RTGAObj) const = 0;
        virtual PTGAObj         evenPtCrossover(RCTGAObj, int) const;
	virtual void            evenPtCrossover(RCTGAObj, int,
						RTGAObj) const = 0;
        virtual PTGAObj         uniformCrossover(RCTGAObj, float) const;
	virtual void            uniformCrossover(RCTGAObj, float,
						 RTGAObj) const = 0;
//
	virtual void            randomize() = 0;
	virtual void            mutate(float) = 0;
//
	virtual RTritString     taxon(unsigned uIndex = 0) = 0;
	virtual RCTritString    taxon(unsigned uIndex = 0) const = 0;
//
	virtual RTritString     action() = 0;
	virtual RCTritString    action() const = 0;

	virtual int		matchMsgs(RCCollection) = 0;
	virtual BOOL		post(RMsgList, PCMessage,
				     PCMessage, float) = 0;
	virtual BOOL		post(RMsgList, PCMessage,
				     PCMessage);

	virtual unsigned        matchCount(RCClassifier) const = 0;
	virtual RCMatchList     matchList() const = 0;
	virtual float           setSupport() = 0;
	virtual void            reward(float flReward);


		int             length() const;
		float&          bid();
		float           bid() const;
		float&          ebid();
		float           ebid() const;
		unsigned        msgsProduced() const;
		void		reset();
		void		canReplace(BOOL);
		BOOL            canReplace() const;
		double          strChg() const;
		double&         strChg();
		float           support() const;
		float           specificity() const;


protected:

	double    flStrChg;
	float     flBid, flEbid;
	float     flSpecificity;
	float     flSupport;

	unsigned  uMsgProd;     // messages produced in the most recent
				// cycle that this classifier is active

	unsigned  uLength;      // length of classifier
	BOOL      rplFlag;      // whether the classifier can
				// be replaced by GA, etc.

	unsigned  uTMtch;
	unsigned  uTMsgProd;


				Classifier();
	virtual void            findSpecificity() = 0;
	virtual unsigned        findLength() = 0;

};

inline  Classifier::Classifier() : TGAObj(), flSpecificity(0),
				   uLength(0), flBid(0), flSupport(0),
				   flEbid(0), uMsgProd(0),
				   rplFlag(FALSE), flStrChg(0),
				   uTMtch(0), uTMsgProd(0)
{
}

inline  int  Classifier::length() const
{
	return uLength;
}

inline  float&  Classifier::bid()
{
	return flBid;
}

inline float   Classifier::bid() const
{
	return flBid;
}

inline float&  Classifier::ebid()
{
	return flEbid;
}

inline float   Classifier::ebid() const
{
	return flEbid;
}

inline float Classifier::specificity() const
{
	return flSpecificity;
}

inline void  Classifier::reward(float flReward)
{
        fitness() = flReward;
}

inline	float  Classifier::support() const
{
	return flSupport;
}

inline  unsigned  Classifier::msgsProduced() const
//
//      this count is only valid for the classifiers in
//      the matchSet of each cycle of a ClassifierSystem
//
{
	return uMsgProd;
}

inline  void Classifier::reset()
{
	uMsgProd = 0;
}

inline  BOOL Classifier::canReplace() const
{
	return rplFlag;
}

inline  void Classifier::canReplace(BOOL flag)
{
	rplFlag = flag;
}

inline  double Classifier::strChg() const
{
	return flStrChg;
}

inline  double& Classifier::strChg()
{
	return flStrChg;
}

inline BOOL Classifier::post(RMsgList msgList, PCMessage pMsg1,
			     PCMessage pMsg2)
{
	return post(msgList, pMsg1, pMsg2, bid());
}

inline PTGAObj  Classifier::oddPtCrossover(RCTGAObj mate,
                                           int nXpts) const
{
	PTGAObj pChild = (PTGAObj) NOOBJECT;

        if (mate.isKindOf(isA())) {
            pChild = (PTGAObj) mate.deepCopy();
            oddPtCrossover(mate, nXpts, *pChild);
        }
	
	return pChild;
}
inline PTGAObj  Classifier::uniformCrossover(RCTGAObj mate,
                                             float flXRate) const
{
	PTGAObj pChild = (PTGAObj) NOOBJECT;

        if (mate.isKindOf(isA())) {
            pChild = (PTGAObj) mate.deepCopy();
            uniformCrossover(mate, flXRate, *pChild);
        }
	
	return pChild;
}

inline PTGAObj Classifier::evenPtCrossover(RCTGAObj mate,
                                           int nXpts) const
{
	PTGAObj pChild = (PTGAObj) NOOBJECT;

        if (mate.isKindOf(isA())) {
            pChild = (PTGAObj) mate.deepCopy();
            evenPtCrossover(mate, nXpts, *pChild);
        }
	
	return pChild;
}

#endif

