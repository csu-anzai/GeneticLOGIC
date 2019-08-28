//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __POPELITE_H )
#define     __POPELITE_H

#include "populatn.h"

class   ElitePopulation : public Population
{
public:
	DECLARE_RTTI()

                                ElitePopulation(PHENOTYPEFUNC,PCollection=NULL);
				ElitePopulation(RCElitePopulation);
				_shallowCopy(ElitePopulation)

        virtual void            replaceBy(RCollection, BOOL=FALSE);
};

inline ElitePopulation::ElitePopulation(PHENOTYPEFUNC pFunc,
                                        PCollection pCltn) :
                        Population(pFunc, pCltn)
{
}

inline ElitePopulation::ElitePopulation(RCElitePopulation src) :
        Population((RCPopulation) src)
{
}

#endif

