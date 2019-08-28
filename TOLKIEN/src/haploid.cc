//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "haploid.h"
#include "trand.h"
#include "IntVec.h"
#include "xover.h"
#include "diploid.h"
#include "atom.h"

static  PCTypeInfo haploidBases[] = { &Individual::infoObj, 0 };
const TypeInfo  Haploid::infoObj("Haploid", haploidBases);

Haploid::Haploid(RCHaploid  src)
{
        genes = src.genes;      // shallowcopy
        TGAObj::operator=(src);
}

void    Haploid::deepenShallowCopy()
{
        if (!genes.ownsElements()) {
             genes.deepenShallowCopy();
	     genes.ownsElements(owner);
        }
}

void     Haploid::oddPtCrossover(RCTGAObj mate, int nXpt, RTGAObj child) const
{
	RCIndividual materef = (RIndividual) mate;
	RCChromosome genesref = genes;

	if (child.isKindOf(isA()))
	    ((RHaploid) child).genes.oddPtCrossover(genesref,
						    (RCChromosome) materef,
						    nXpt);
	else
	    mate.oddPtCrossover(*this, nXpt, child);
}

void     Haploid::evenPtCrossover(RCTGAObj mate, int nXpts,
                                  RTGAObj child) const
{
	RCIndividual materef = (RIndividual) mate;
	RCChromosome genesref = genes;

	if (child.isKindOf(isA()))
	    ((RHaploid) child).genes.evenPtCrossover(genesref,
						    (RCChromosome) materef,
                                                    nXpts);
	else
            mate.evenPtCrossover(*this, nXpts, child);
}

PTGAObj  Haploid::oddPtCrossover(RCTGAObj mate, int nXpt) const
{
	PRECONDITION(mate.isKindOf(isA()) ||
		     mate.isKindOf(& Diploid::infoObj));

	PHaploid     pChild = (PHaploid) this->copy(); // create a shallow copy first
	pChild->genes.oddPtCrossover(genes,
				((RCIndividual) mate).operator RCChromosome(),
				nXpt);
	pChild->deepenShallowCopy();    // deepen pointers
	return pChild;
}

PTGAObj  Haploid::evenPtCrossover(RCTGAObj mate, int nXpts) const
{
	PRECONDITION(mate.isKindOf(isA()) ||
		     mate.isKindOf(& Diploid::infoObj));

	PHaploid     pChild = (PHaploid) this->copy(); // create a shallow copy first
	pChild->genes.evenPtCrossover(genes,
			       ((RCIndividual) mate).operator RCChromosome(),
                                      nXpts);

	pChild->deepenShallowCopy();    // deepen pointers
	return pChild;
}

PTGAObj  Haploid::uniformCrossover(RCTGAObj mate, float flXRate) const
{
	PRECONDITION(mate.isKindOf(isA()) ||
		     mate.isKindOf(& Diploid::infoObj));

	PHaploid     pChild = (PHaploid) this->copy(); // create a shallow copy first

	pChild->genes.uniformCrossover(genes,
                                 ((RCIndividual) mate).operator RCChromosome(),
                                       flXRate);
	pChild->deepenShallowCopy();    // deepen pointers
	return pChild;
}

void     Haploid::uniformCrossover(RCTGAObj mate, float flXRate,
				   RTGAObj child) const
{
	if (child.isKindOf(isA()))
	    ((RHaploid) child).genes.uniformCrossover(genes,
				 ((RCIndividual) mate).operator RCChromosome(),
						      flXRate);
	else
	    mate.uniformCrossover(*this, flXRate, child);
}

void    Haploid::mutate(float flRate)
{
	for (register int nI = 0; nI < genes.length(); nI++)
	     ((RAtomBase) genes[nI]).mutate(flRate);
}

RHaploid  Haploid::operator=(RCHaploid src)
{
        if ( ! isSame(src) ) {
	    genes = src.genes;
	    TGAObj::operator=(src);
	}
	return *this;
}

RHaploid  Haploid::operator=(RCDiploid src)
{
        genes = (RCChromosome) src;
        TGAObj::operator=(src);
        return *this;
}

BOOL Haploid::isEqual( RCTObject obj ) const
{
	if ( isA() == obj.isA() &&
	     TGAObj::isEqual(obj) )
	     return genes.isEqual(((RCHaploid) obj).genes);
	else
	     return FALSE;
}
