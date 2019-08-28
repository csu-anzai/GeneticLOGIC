//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if     !defined( __DIPLOID_H )

#define __DIPLOID_H

#include "haploid.h"
#include "atom.h"

class   Diploid : public Individual
{
public:

	DECLARE_RTTI()

	virtual BOOL            isEqual( RCTObject ) const;
	virtual void            printOn( ostream  & out) const;

	virtual PTObject        shallowCopy() const = 0;
	virtual void            deepenShallowCopy();

	virtual PTGAObj         oddPtCrossover(RCTGAObj, int) const;
	virtual void            oddPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual PTGAObj         evenPtCrossover(RCTGAObj, int) const;
	virtual void            evenPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual PTGAObj         uniformCrossover(RCTGAObj, float) const;
	virtual void            uniformCrossover(RCTGAObj, float,
						 RTGAObj) const;

	virtual void            mutate(float);
	virtual void            randomize();
	virtual                 operator RCChromosome() const;
	virtual int             length() const;

	virtual RCChromosome    hmlgs1() const;
	virtual RCChromosome    hmlgs2() const;

	virtual RCChromosome    hmlgs1(RCChromosome);
	virtual RCChromosome    hmlgs2(RCChromosome);

	virtual RDiploid        operator=(RCHaploid);
	virtual RDiploid        operator=(RCDiploid);

protected:

	Chromosome      hmlg1,          // homologous pair
			hmlg2,          //
			expressed;      // the expressed genotype

	Diploid();
	Diploid(RChromosome, RChromosome);

	virtual     void        setDominance();
	virtual     Atom	mapDominance(RCAtomBase, RCAtomBase) const = 0;
	virtual     void        normalize();

	virtual PTGAObj  _oddPtCrossover(RCDiploid, int) const;
	virtual PTGAObj  _oddPtCrossover(RCHaploid, int) const;
	virtual void     _oddPtCrossover(RCDiploid, int, RDiploid) const;
	virtual void     _oddPtCrossover(RCHaploid, int, RDiploid) const;

	virtual PTGAObj  _evenPtCrossover(RCDiploid, int) const;
	virtual PTGAObj  _evenPtCrossover(RCHaploid, int) const;
	virtual void     _evenPtCrossover(RCDiploid, int, RDiploid) const;
	virtual void     _evenPtCrossover(RCHaploid, int, RDiploid) const;

	virtual PTGAObj  _uniformCrossover(RCDiploid, float) const;
	virtual PTGAObj  _uniformCrossover(RCHaploid, float) const;
	virtual void     _uniformCrossover(RCDiploid, float, RDiploid) const;
	virtual void     _uniformCrossover(RCHaploid, float, RDiploid) const;

};

inline  Diploid::Diploid() : hmlg1(), hmlg2(), expressed()
{
}

inline  void  Diploid::randomize()
{
	mutate(1);
}

inline  Diploid::operator RCChromosome() const
{
	return expressed;
}

inline int Diploid::length() const
{
	return expressed.length();
}

inline  RCChromosome  Diploid::hmlgs1() const
{
	return hmlg1;
}

inline  RCChromosome  Diploid::hmlgs2() const
{
	return hmlg2;
}

inline  RCChromosome  Diploid::hmlgs1(RCChromosome src)
{
	hmlg1 = src;
	normalize();
	setDominance();
	return hmlg1;
}

inline  RCChromosome  Diploid::hmlgs2(RCChromosome src)
{
	hmlg2 = src;
	normalize();
	setDominance();
	return hmlg2;
}

#endif

