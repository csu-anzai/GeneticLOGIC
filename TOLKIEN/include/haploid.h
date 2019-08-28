//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if     !defined( __HAPLOID_H )

#define __HAPLOID_H

#include "individ.h"

class   Haploid : public Individual
{
public:

                                DECLARE_RTTI()

	virtual BOOL            isEqual( RCTObject ) const;
	virtual void            printOn( ostream  & ) const;
        virtual PTObject        shallowCopy() const = 0;
        virtual void            deepenShallowCopy();

        virtual PTGAObj         oddPtCrossover(RCTGAObj, int) const;
        virtual void            oddPtCrossover(RCTGAObj, int, RTGAObj) const;
        virtual PTGAObj         evenPtCrossover(RCTGAObj, int) const;
        virtual void            evenPtCrossover(RCTGAObj, int, RTGAObj) const;
        virtual PTGAObj         uniformCrossover(RCTGAObj, float) const;
        virtual void            uniformCrossover(RCTGAObj,float,RTGAObj) const;
        virtual void            mutate(float);
        virtual void            randomize();
        virtual                 operator RCChromosome() const;
        virtual int             length() const;
        virtual RHaploid        operator=(RCHaploid);
        virtual RHaploid        operator=(RCDiploid);

protected:

	Chromosome      genes;

	Haploid();
	Haploid(RCHaploid);
	Haploid(RCChromosome src);
};

inline  Haploid::Haploid() : Individual(), genes()
{
}

inline  Haploid::Haploid(RCChromosome src)
{
        genes = src;  // shallow copy
}

inline void Haploid::printOn( ostream  & out) const
{
	genes.printOn(out);
}

inline void Haploid::randomize()
{
	mutate(1.0);
}

inline Haploid::operator RCChromosome() const
{
	return genes;
}

inline int Haploid::length() const
{
	return genes.length();
}

#endif

