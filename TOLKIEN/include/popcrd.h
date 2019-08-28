//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __POPCRD_H )
#define     __POPCRD_H

#include "populatn.h"

class   CrowdingPopulation : public Population
{
public:
	DECLARE_RTTI()

                                CrowdingPopulation(PHENOTYPEFUNC,
                                                   PCollection=NULL);
				CrowdingPopulation(RCCrowdingPopulation);
				_shallowCopy(CrowdingPopulation)

                void            crowdingFactor(unsigned);
                unsigned        crowdingFactor() const;
                void            crowdingSubPop(unsigned);
                unsigned        crowdingSubPop() const;

        virtual void            replaceBy(RCollection, BOOL=FALSE);

protected:

        unsigned  uCrowdingFactor, uCrowdingSubPop;

                sizeType        worstInd() const;
                sizeType        select(RCTGAObj) const;
};

inline CrowdingPopulation::CrowdingPopulation(PHENOTYPEFUNC pFunc,
                                              PCollection pCltn) :
                Population(pFunc, pCltn),
		uCrowdingFactor(2), uCrowdingSubPop(2)
{
}

inline CrowdingPopulation::CrowdingPopulation(RCCrowdingPopulation src) :
	Population((RCPopulation) src),
	uCrowdingFactor(src.uCrowdingFactor),
	uCrowdingSubPop(src.uCrowdingSubPop)
{
}

inline void CrowdingPopulation::crowdingFactor(unsigned newval)
{
	uCrowdingFactor = (rangeCheck(newval,1,5,1));
}

inline unsigned CrowdingPopulation::crowdingFactor() const
{
	return uCrowdingFactor;
}

inline void CrowdingPopulation::crowdingSubPop(unsigned newval)
{
	uCrowdingSubPop = newval;
}

inline unsigned CrowdingPopulation::crowdingSubPop() const
{
        return uCrowdingSubPop;
}

#endif

