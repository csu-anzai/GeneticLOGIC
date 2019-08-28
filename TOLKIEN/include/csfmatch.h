//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __CSFMATCH_H )
#define   __CSFMATCH_H

#include "message.h"
#include "clsifier.h"

//
//      MatchList is implemented as derived class for TGAObj
//      for used by SelectScheme
//      one MatchList is a list of messages matched
//      by a certain classifier
//
class   MatchList : public TGAObj
{
public:
	DECLARE_RTTI()

	virtual                 ~MatchList();

	virtual int             compare( RCTObject ) const;
        virtual BOOL            isEqual( RCTObject ) const = 0;
        virtual void            printOn( ostream  & out) const;
	virtual PTObject        shallowCopy() const = 0;
	virtual void            deepenShallowCopy() = 0;

	virtual int             post(RMsgList) const = 0;
        virtual float           sumIntensities() const = 0;

                PTGAObj         oddPtCrossover(RCTGAObj, int) const
                {
                        return NULL;
                }

                void            oddPtCrossover(RCTGAObj, int, RTGAObj) const
                {
                }

                PTGAObj         evenPtCrossover(RCTGAObj, int) const
                {
                        return NULL;
                }

                void            evenPtCrossover(RCTGAObj, int, RTGAObj) const
                {
                }

                PTGAObj         uniformCrossover(RCTGAObj, float) const
                {
                        return NULL;
                }
                void            uniformCrossover(RCTGAObj, float,
						 RTGAObj) const

                {
                }
                void            mutate(float = 0)
                {
                }

                void            randomize()
                {
                }

                double&         fitness();
                double&         objValue();
                double          fitness() const;
                double          objValue() const;

                int             length() const;

                RCClassifier    csf() const;


protected:

	PClassifier             pCls;
	double                  flStrength; // the effective bid of cls

	MatchList(RCClassifier);
	MatchList();
};

inline MatchList::MatchList() : pCls(NULL), flStrength(0)
{
}

inline MatchList::MatchList(RCClassifier src) :
			    pCls((PClassifier) &src),
			    flStrength(src.ebid())
{
}

inline MatchList::~MatchList()
{
}

inline  int     MatchList::compare( RCTObject obj ) const
{
	if (obj.isKindOf(isA()))
	    return flStrength - ((RMatchList) obj).flStrength;
	else
	    return 1;
}

inline double& MatchList::fitness()
{
	return flStrength;
}

inline double& MatchList::objValue()
{
        return flStrength;
}

inline double  MatchList::fitness() const
{
	return flStrength;
}

inline double  MatchList::objValue() const
{
        return flStrength;
}

inline int    MatchList::length() const
{
        return 0;
}

inline  RCClassifier    MatchList::csf() const
{
        return *pCls;
}

inline  void  MatchList::printOn( ostream  & out) const
{
        pCls->printOn(out);
	out << '{' << flStrength << '}';
}

#endif

