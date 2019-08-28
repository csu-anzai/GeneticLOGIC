//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SLCTSUS_H)
#define __SLCTSUS_H

#include "select.h"
#include "IntVec.h"

//
//      Stochastic Universal Sampling
//

class   SUS_Select : public SelectionScheme
{
public:
				DECLARE_RTTI()

				SUS_Select();
				SUS_Select(RCCollection);
				SUS_Select(RCCollection, sizeType);

				_shallowCopy(SUS_Select)
	virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void            reset(RCCollection);
        virtual void            reset(RCCollection, sizeType);
	virtual void            reset();
        virtual void            reset(sizeType);

protected :
	TOArray  mates;
	sizeType remains;
        IntVec   ndx;    // ndx is a random sequence of
                         // the set of integers [0 .. N-1]
                         // this is used to cancel
                         // positional bias in the
                         // population to be selected

};

inline	SUS_Select::SUS_Select() : SelectionScheme(), remains(0)
{
}

inline void SUS_Select::printOn( ostream  & out) const
{
	out << nameOf() << " : " << mates;
}

inline	void SUS_Select::reset(RCCollection src)
{
	reset(src,src.size());
}

inline	void SUS_Select::reset()
{
	reset(pPop->size());
}
#endif

