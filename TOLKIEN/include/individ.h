//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __INDIVID_H )
#define __INDIVID_H

#include "chromo.h"

class   Individual : public TGAObj
{
public:
				DECLARE_RTTI()

	virtual PTObject        shallowCopy() const = 0;
	virtual void            deepenShallowCopy() = 0;

        virtual BOOL            isEqual( RCTObject ) const = 0;
	virtual void            printOn( ostream  &) const = 0;

	virtual PTGAObj         oddPtCrossover(RCTGAObj, int) const = 0;
	virtual void            oddPtCrossover(RCTGAObj, int,
					       RTGAObj) const = 0;
	virtual PTGAObj         evenPtCrossover(RCTGAObj, int) const = 0;
	virtual void            evenPtCrossover(RCTGAObj, int,
						RTGAObj) const = 0;
	virtual PTGAObj         uniformCrossover(RCTGAObj, float) const = 0;
	virtual void            uniformCrossover(RCTGAObj, float,
						 RTGAObj) const = 0;
	virtual void            mutate(float) = 0;
	virtual void            randomize() = 0;
	virtual int             length() const = 0;
	virtual                 operator RCChromosome() const = 0;
};

#endif

