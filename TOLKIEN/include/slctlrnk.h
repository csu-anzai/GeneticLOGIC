//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SLCTLRNK_H)
#define __SLCTLRNK_H

#include "select.h"

#ifndef DEFAULT_BIAS
#define DEFAULT_BIAS    1.5
#endif

class  LinearRanking : public SelectionScheme
{
//
//      Linear ranking Selection Scheme as described in
//      ICGA-91 : The GENITOR Algorithm and Selection Pressure :
//                Why Rank-Based Allocation of Reproductive Trials is Best
//
//
public:
				DECLARE_RTTI()

                                LinearRanking(double=DEFAULT_BIAS);
                                LinearRanking(RCLinearRanking);
                                LinearRanking(RCCollection);
                                ~LinearRanking();

                                _shallowCopy(LinearRanking)
        virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void 		reset(RCCollection src);
        virtual double          bias() const;
        virtual double          bias(double);

protected:
	double          flBias, flBiasSqr, flBiasLessOne;
	TOArray         sorted;
};

inline LinearRanking::LinearRanking(double val) : SelectionScheme()
{
        bias(val);
}

inline LinearRanking::LinearRanking(RCLinearRanking src) :
        SelectionScheme(src), flBias(src.flBias), sorted(src.sorted)
{
}

inline LinearRanking::~LinearRanking()
{
}

inline void    LinearRanking::printOn( ostream  & out) const
{
        out << nameOf() << " : " << flBias << endl << *pPop;
}

inline  void   LinearRanking::reset(RCCollection src)
{
        sorted.removeAll();
        sorted.addAll(src);
        sorted.sort(decFitness);
}

inline  double LinearRanking::bias() const
{
        return flBias;
}

inline  double LinearRanking::bias(double newBias)
{
	double tmp = flBias;
	flBias = newBias > 1 ? newBias : DEFAULT_BIAS;
	flBiasSqr = flBias * flBias;
	flBiasLessOne = flBias - 1;
	return tmp;
}

#endif

