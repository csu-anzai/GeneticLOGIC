//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SLCTRW_H)
#define __SLCTRW_H

#include "select.h"
#include "IntVec.h"
#include "DblVec.h"

class  RW_Select : public SelectionScheme
{
//
//      Roulette Wheel Selection Scheme
//
public:
				DECLARE_RTTI()

                                RW_Select();
                                RW_Select(RCRW_Select);
				RW_Select(RCCollection);
                                ~RW_Select();

                                _shallowCopy(RW_Select)
        virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void 		reset(RCCollection src);

protected:
        double                  flSumFitness;
        DblVec                  cumSlots; // store the cumluative totals
        IntVec                  ndx;    // ndx is a random sequence of
                                        // the set of integers [0 .. N-1]
					// this is used to cancel
                                        // positional bias in the
                                        // population to be selected

	virtual unsigned        _select();
        virtual void            sumFitness();

};

inline RW_Select::RW_Select() : SelectionScheme()
{
}

inline RW_Select::RW_Select(RCRW_Select src) :
        SelectionScheme(src),
        ndx(src.ndx),
        cumSlots(src.cumSlots),
        flSumFitness(src.flSumFitness)
{
}

inline RW_Select::~RW_Select()
{
}

inline void    RW_Select::printOn( ostream  & out) const
{
        out << nameOf() << " : " << flSumFitness << endl << *pPop;
}

inline PCTGAObj  RW_Select::select()
{
   return (PCTGAObj) member(_select());
}

#endif

