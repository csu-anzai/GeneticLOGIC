//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SLCTRWWR_H)
#define __SLCTRWWR_H

#include "select.h"
#include "todllist.h"

class  RWwoR_Select : public SelectionScheme
{
//
//      Roulette Wheel Selection Scheme without replacement
//
public:
				DECLARE_RTTI()

				RWwoR_Select();
				RWwoR_Select(RCCollection);

				_shallowCopy(RWwoR_Select)
        virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void 		reset(RCCollection src);

protected:
                double          flSumFitness;
                TObjDLList      objlist;
                sizeType        curSz;

        virtual void            buildList();
        virtual void            sumFitness();
};

inline RWwoR_Select::RWwoR_Select() : SelectionScheme(), curSz(0)
{
}

inline void    RWwoR_Select::printOn( ostream  & out) const
{
	out << nameOf() << " : " << flSumFitness << endl << objlist;
}
#endif

