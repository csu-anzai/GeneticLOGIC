//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#if !defined (__GATSP_H__)
#define __GATSP_H__

#include "ga.h"
#include "tour.h"
#include "tsp.h"

_CLASSDEF(POP_GA)
_CLASSDEF(TSP_GA)

extern TourInfo	*pTourInfo;

inline 	double cost_function(RCTGAObj obj)
{
	return pTourInfo->cost((RCTSPind) obj);
}

class   TSP_GA : public GeneticAlgorithm
{
public:

TSP_GA(PPopulation  pInitPop,
       PSelectionScheme pSelectScheme,
       PCrossover pCrossover,
       float flMutate,
       float flGenGap,
       PScalingScheme pScaleScheme,
       BOOL  reEval = FALSE); 

	virtual ~TSP_GA()
	{
	}

	virtual void generation();
	virtual void statistics();

	BOOL run(int);

	BOOL converged() const;

protected :
	int	retries;
	void	restart();
};

class	POP_GA : public Population
{
public:

				POP_GA(PHENOTYPEFUNC);
				POP_GA(RCPOP_GA);
				_shallowCopy(POP_GA)

	virtual void            findBestandWorst(RPTGAObj, RPTGAObj);
	virtual void            replaceBy(RCollection, BOOL = FALSE);
};

inline POP_GA::POP_GA(PHENOTYPEFUNC pFunc) : Population(pFunc)
{
       pInds = new TOArray(owner);
}

inline POP_GA::POP_GA(RCPOP_GA src) : Population(src)
{
}

#endif
