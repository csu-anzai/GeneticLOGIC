//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
//      A number of ideas are borrowed from CFS-C by Dr. Rick L. Riolo
//
//              producer tax
//
//
//
#if !defined ( __CSFSYS_H )
#define   __CSFSYS_H

#include "toarray.h"
#include "Normal.h"
#include "xover.h"
#include "slcttour.h"
#include "msglist.h"

extern  Normal noise;   // noise for stochastic bidding

extern  int     decEBid(const void *pMem1, const void *pMem2);
extern  int     ascEBid(const void *pMem1, const void *pMem2);

class   ClassifierSystem
{
public:

	ClassifierSystem(PMsgList	  pMsgList,
			 PSelectionScheme pSelect,
			 PCrossover       pCross);

	virtual ~ClassifierSystem();

//
        virtual void            run();

//      providing access to protected members

                RMsgList        msgs();
                RCMsgList       oldMsgs();
                RTOArray        csfs();
                RCMessage       msg(sizeType index);
                RCClassifier    csf(sizeType index);

//
                float           mutRate() const;
                float           xoverRate() const;
                BOOL            useSupport() const;
                float           bidRatio() const;
                float           lifeTaxRate() const;
                float           bidTaxRate() const;
                float           bidSp() const;
                float           eBidSp() const;
                BOOL            runBB() const;
                float           csfStrMax() const;
                float           csfStrMin() const;
                float           prdTaxMax() const;
                float           prdTaxPow() const;
                int             newCsfStr() const;
                unsigned        crowdingFactor() const;
                unsigned        crowdingSubPop() const;
                int             trials() const;

//      parameters setting
                void            mutRate(float flVal);
                void            xoverRate(float flVal);
                void            useSupport(BOOL);
                void            bidRatio(float);
                void            bidSp(float);
                void            eBidSp(float);
                void            lifeTaxRate(float);
                void            bidTaxRate(float);
                void            runBB(BOOL);
                void            csfStrMax(float);
                void            csfStrMin(float);
                void            prdTaxMax(float);
                void            prdTaxPow(float);
                void            newCsfStr(int);
                void            crowdingFactor(unsigned);
                void            crowdingSubPop(unsigned);

//      error handling
    virtual     void            error(const char *szMsg);
    virtual     void            setMsgListCapacity(const sizeType capacity);

protected:
//
// Parameter				    	        Default Value
// ---------------------------------	    	  --------------------------
// flBidRt              bid ratio                  0.1
// flBidSp              bid parameter              1.0
// flEBidSp             effective bid parameter    1.0
// flLifeTaxRate        existence tax rate
//                      for each classifier        0.01
// flBidTaxRate         bid tax rate for each
//                      classifer which make a
//                      bid in a cycle             0.01
// flXRate              crossover rate

	float                   flBidRt;
	float                   flBidSp, flEBidSp;
	float                   flLifeTaxRate;
	float                   flBidTaxRate;
	float                   flMRate;
	float                   flTotalSupport;
	float                   flAvgCsfStr;
	float                   flCsfStrMax;
	float                   flCsfStrMin;
	float                   flPrdTaxMax;
	float                   flPrdTaxPow;
	BOOL                    oneMsgPCsf;
	BOOL                    bbFlag;
	PSelectionScheme        pSSch;
	PMsgList                pCurMsgList, pOldMsgList, pNewMsgList;
        TOArray                 csfList;
	TOArray                 matchSet;
	PCrossover              pXover;

	int                     nNewCsfStr;

	BOOL                    supportFlag;
        int                     nTrials;
	int                     nMatches; // number of matches
                                      // between classifiers and messages
                                      // if this value is larger than
                                      // the capacity of the message list
                                      // an auction is made to decide
                                      // which classifiers can post
                                      // messages

	unsigned                uCrowdingFactor;
	unsigned                uCrowdingSubPop;
//
//
	virtual float           findAvgCsfStr();
	virtual void            setBid(PClassifier);
	virtual void            setEBid(PClassifier);
	virtual void            preProcessing();
	virtual void            swapMsgList();
	virtual void            findMatch();
	virtual PTClassifier    auction(RCollection, RCMessage);
	virtual void            produceMsgs();
	virtual void            bucketBrigade();
	virtual void            updateCsfStr();
	virtual void            newClassifiers(float);
	virtual void            detectors() = 0;
	virtual void            effectors() = 0;
	virtual void            ga(RCollection, float,
                                   RSelectionScheme, RCrossover);
	virtual sizeType        setRplFlags();
        virtual void            replaceBy(PCClassifier);
                sizeType        worstofn() const;
};

inline  MsgList&  ClassifierSystem::msgs()
{
	return (MsgList&) *pCurMsgList;
}


inline  RTOArray ClassifierSystem::csfs()
{
        return csfList;
}

inline  RCMessage  ClassifierSystem::msg(sizeType index)
{
        return * ((PCMessage) pCurMsgList->at(index));
}

inline  RCClassifier ClassifierSystem::csf(sizeType index)
{
        return (RCClassifier) csfList[index];
}

inline void     ClassifierSystem::bidSp(float flVal)
{
	flBidSp = rangeCheck(flVal, 0.0, 1.0, 1.0);
}

inline void     ClassifierSystem::eBidSp(float flVal)
{
        flEBidSp = rangeCheck(flVal, 0.0, 1.0, 1.0);
}

inline	void	ClassifierSystem::useSupport(BOOL flag)
{
        supportFlag = flag;
}

//      error handling
inline  void    ClassifierSystem::error(const char *szMsg)
{
	(*lib_error_handler)("Classifier System", szMsg);
}

inline  void    ClassifierSystem::setMsgListCapacity(const sizeType capacity)
{
        pNewMsgList->capacity(capacity);
        pCurMsgList->capacity(capacity);
        pOldMsgList->capacity(capacity);
}

inline  int     ClassifierSystem::trials() const
{
	return  nTrials;
}

inline  RCMsgList  ClassifierSystem::oldMsgs()
{
	return * (PCMsgList) pOldMsgList;
}

inline  void   ClassifierSystem::swapMsgList()
{
	//
	// swap message lists
	//
	// current message list becomes old message list
	// new message list becomes current message list
	// old message list becomes new message list
	//
	PMsgList pOld = pOldMsgList,
		 pCur = pCurMsgList,
		 pNew = pNewMsgList;

	pOldMsgList = pCur;
	pCurMsgList = pNew;
	pNewMsgList = pOld;
}

inline float ClassifierSystem::mutRate() const
{
	return flMRate;
}

inline float ClassifierSystem::xoverRate() const
{
	return pXover->rate();
}

inline BOOL  ClassifierSystem::useSupport() const
{
        return supportFlag;
}

inline float ClassifierSystem::bidSp() const
{
	return flBidSp;
}

inline float ClassifierSystem::eBidSp() const
{
        return flBidSp;
}

inline float ClassifierSystem::bidRatio() const
{
        return flBidRt;
}

inline float ClassifierSystem::lifeTaxRate() const
{
	return flLifeTaxRate;
}

inline float ClassifierSystem::bidTaxRate() const
{
	return flBidTaxRate;
}

inline void ClassifierSystem::bidRatio(float flNewVal)
{
        flBidRt = flNewVal;
}

inline void ClassifierSystem::lifeTaxRate(float flNewVal)
{
	flLifeTaxRate = flNewVal;
}

inline void ClassifierSystem::bidTaxRate(float flNewVal)
{
	flBidTaxRate = flNewVal;
}

inline void ClassifierSystem::mutRate(float flNewVal)
{
	flMRate = rangeCheck(flNewVal, 0.0, 1.0, DEFAULT_MUTATRATE);
}

inline void ClassifierSystem::xoverRate(float flNewVal) 
{
	pXover->rate(flNewVal);
}

inline void ClassifierSystem::runBB(BOOL flag)
{
        bbFlag = flag;
}

inline BOOL ClassifierSystem::runBB() const
{
        return bbFlag;
}

inline void ClassifierSystem::csfStrMax(float newVal)
{
        flCsfStrMax = newVal;
}

inline void ClassifierSystem::csfStrMin(float newVal)
{
        flCsfStrMin = newVal;
}

inline float ClassifierSystem::csfStrMax() const
{
        return flCsfStrMax;
}

inline float ClassifierSystem::csfStrMin() const
{
        return flCsfStrMin;
}

inline void ClassifierSystem::prdTaxMax(float newVal)
{
        flPrdTaxMax = newVal;
}

inline void ClassifierSystem::prdTaxPow(float newVal)
{
        flPrdTaxPow = newVal;
}

inline float ClassifierSystem::prdTaxMax() const
{
        return flPrdTaxMax;
}

inline float ClassifierSystem::prdTaxPow() const
{
        return flPrdTaxPow;
}

inline void ClassifierSystem::newCsfStr(int newVal)
{
        nNewCsfStr = newVal;
}

inline int ClassifierSystem::newCsfStr() const
{
        return nNewCsfStr;
}

inline void ClassifierSystem::crowdingFactor(unsigned newVal)
{
        uCrowdingFactor = newVal;
}

inline void ClassifierSystem::crowdingSubPop(unsigned newVal)
//
//      this value must be in the range [1, number of classifiers]
//
{
        uCrowdingSubPop = rangeCheck(newVal, 1, csfs().size(), 2);
}

inline unsigned ClassifierSystem::crowdingFactor() const
{
        return uCrowdingFactor;
}

inline unsigned ClassifierSystem::crowdingSubPop() const
{
        return uCrowdingSubPop;
}

#endif

