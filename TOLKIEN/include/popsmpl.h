//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __POPSMPL_H )
#define     __POPSMPL_H

#include "populatn.h"
#include "IntVec.h"

class   SimplePopulation : public Population
{
public:
				DECLARE_RTTI()

                                SimplePopulation(PHENOTYPEFUNC,PCollection=NULL);
				SimplePopulation(RCSimplePopulation);

				_shallowCopy(SimplePopulation)

        virtual void            replaceBy(RCollection, BOOL=FALSE);

protected:

	IntVec	ndx;	// indices to member in individuals
			// used by function replaceBy
};

inline SimplePopulation::SimplePopulation(PHENOTYPEFUNC pFunc,
                                          PCollection pCltn) :
                         Population(pFunc, pCltn)
{
}

inline SimplePopulation::SimplePopulation(RCSimplePopulation src) :
	Population(src)
{
}

#endif

