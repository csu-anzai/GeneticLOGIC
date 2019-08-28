//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "diploid.h"

static  PCTypeInfo diploidBases[] = { &Individual::infoObj, 0 };
const TypeInfo  Diploid::infoObj("Diploid", diploidBases);

Diploid::Diploid(RChromosome chromo1, RChromosome chromo2) :
        hmlg1(chromo1, owner), hmlg2(chromo2, owner),
        expressed(chromo1, owner)
{
	setDominance();
}

void    Diploid::deepenShallowCopy()
{
	if (!hmlg1.ownsElements()) {
	     hmlg1.deepenShallowCopy();
	     hmlg1.ownsElements(owner);
	}

	if (!hmlg2.ownsElements()) {
	     hmlg2.deepenShallowCopy();
	     hmlg2.ownsElements(owner);
	}
	if (!expressed.ownsElements()) {
	     expressed.deepenShallowCopy();
	     expressed.ownsElements(owner);
	}
}

void    Diploid::printOn( ostream & out) const
{
	hmlg1.printOn(out);
	out << endl;
	hmlg2.printOn(out);
        out << endl;
        expressed.printOn(out);
        out << endl;
}

void    Diploid::normalize()
{
	if (hmlg1.length() > hmlg2.length())
            hmlg2.capacity(hmlg1.length());
	else if (hmlg2.length() > hmlg1.length())
            hmlg1.capacity(hmlg2.length());

        expressed.capacity(hmlg1.length());
}


PTGAObj  Diploid::oddPtCrossover(RCTGAObj mate, int nXpt) const
{
	if (mate.isKindOf(Haploid::typeInfo()))
	    return _oddPtCrossover((RCHaploid) mate, nXpt);
	else
	    if (mate.isKindOf(Diploid::typeInfo()))
		return _oddPtCrossover((RCDiploid) mate, nXpt);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    Diploid::oddPtCrossover(RCTGAObj mate, int nXpt, RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
	    _oddPtCrossover((RCDiploid) mate, nXpt, (RDiploid) child);
	else if (mate.isKindOf(Haploid::typeInfo()))
	    _oddPtCrossover((RCHaploid) mate, nXpt, (RDiploid) child);
}

PTGAObj  Diploid::evenPtCrossover(RCTGAObj mate, int nXpts) const
{
	if (mate.isKindOf(Haploid::typeInfo()))
            return _evenPtCrossover((RCHaploid) mate, nXpts);
	else
	    if (mate.isKindOf(Diploid::typeInfo()))
                return _evenPtCrossover((RCDiploid) mate, nXpts);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    Diploid::evenPtCrossover(RCTGAObj mate, int nXpts,
                                 RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
            _evenPtCrossover((RCDiploid) mate, nXpts, (RDiploid) child);
	else if (mate.isKindOf(Haploid::typeInfo()))
            _evenPtCrossover((RCHaploid) mate, nXpts, (RDiploid) child);
}

PTGAObj  Diploid::uniformCrossover(RCTGAObj mate, float flXRate) const
{
	if (mate.isKindOf(Haploid::typeInfo()))
	    return _uniformCrossover((RCHaploid) mate, flXRate);
	else
	    if (mate.isKindOf(Diploid::typeInfo()))
		return _uniformCrossover((RCDiploid) mate, flXRate);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    Diploid::uniformCrossover(RCTGAObj mate, float flXRate,
				  RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
	    _uniformCrossover((RCDiploid) mate, flXRate, (RDiploid) child);
	else if (mate.isKindOf(Haploid::typeInfo()))
	    _uniformCrossover((RCHaploid) mate, flXRate, (RDiploid) child);
}

PTGAObj  Diploid::_oddPtCrossover(RCDiploid mate, int nXpt) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   pChild->hmlg2.oddPtCrossover(mate.hmlg1, mate.hmlg2, nXpt);
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

PTGAObj  Diploid::_oddPtCrossover(RCHaploid mate, int nXpt) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   pChild->hmlg2 = (RCChromosome) mate;
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    Diploid::_oddPtCrossover(RCHaploid mate, int nXpt,
                                 RDiploid child) const
{
   child.hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   child.hmlg2 = (RCChromosome) mate;
   child.normalize();
   child.setDominance();
}

void    Diploid::_oddPtCrossover(RCDiploid mate, int nXpt,
                                 RDiploid child) const
{
   child.hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   child.hmlg2.oddPtCrossover(mate.hmlg1, mate.hmlg2, nXpt);
   child.normalize();
   child.setDominance();
}

PTGAObj  Diploid::_evenPtCrossover(RCDiploid mate, int nXpts) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   pChild->hmlg2.evenPtCrossover(mate.hmlg1, mate.hmlg2, nXpts);
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

PTGAObj  Diploid::_evenPtCrossover(RCHaploid mate, int nXpts) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   pChild->hmlg2 = (RCChromosome) mate;
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    Diploid::_evenPtCrossover(RCHaploid mate, int nXpts,
                                 RDiploid child) const
{
   child.hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   child.hmlg2 = (RCChromosome) mate;
   child.normalize();
   child.setDominance();
}

void    Diploid::_evenPtCrossover(RCDiploid mate, int nXpts,
                                 RDiploid child) const
{
   child.hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   child.hmlg2.evenPtCrossover(mate.hmlg1, mate.hmlg2, nXpts);
   child.normalize();
   child.setDominance();
}

PTGAObj  Diploid::_uniformCrossover(RCDiploid mate, float flXRate) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.uniformCrossover(this->hmlg1, this->hmlg2, flXRate);
   pChild->hmlg2.uniformCrossover(mate.hmlg1, mate.hmlg2, flXRate);
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

PTGAObj  Diploid::_uniformCrossover(RCHaploid mate, float flXRate) const
{
   PDiploid  pChild = (PDiploid) this->copy(); // create shallow copy first

   pChild->hmlg1.uniformCrossover(this->hmlg1, this->hmlg2, flXRate);
   pChild->hmlg2 = (RCChromosome) mate;
   pChild->deepenShallowCopy();

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    Diploid::_uniformCrossover(RCHaploid mate, float flXRate,
				   RDiploid child) const
{
   child.hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   child.hmlg2 = (RCChromosome) mate;
   child.normalize();
   child.setDominance();
}

void    Diploid::_uniformCrossover(RCDiploid mate, float flXRate,
				   RDiploid child) const
{
   child.hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   child.hmlg2.uniformCrossover(mate.hmlg1, mate.hmlg2, flXRate);
   child.normalize();
   child.setDominance();
}

void    Diploid::mutate(float flRate)
{
	for (register int nI = 0; nI < expressed.length(); nI++) {
	     ((RAtomBase) hmlg1[nI]).mutate(flRate);
	     ((RAtomBase) hmlg2[nI]).mutate(flRate);
	}
	setDominance();
}

RDiploid  Diploid::operator=(RCDiploid src)
{
	if (! isSame(src) ) {
	    hmlg1 = src.hmlg1;
	    hmlg2 = src.hmlg2;
	    expressed = src.expressed;
	    TGAObj::operator=(src);
	}
	return *this;
}

RDiploid  Diploid::operator=(RCHaploid src)
{
	hmlg1 = (RCChromosome) src;
	hmlg2 = (RCChromosome) src;
	setDominance();
	TGAObj::operator=(src);
	return *this;
}

BOOL Diploid::isEqual( RCTObject obj) const
{
	if ( isA() == obj.isA()  &&
	     TGAObj::isEqual(obj) ) {
	    RCDiploid dp = (RCDiploid) obj;
	    return hmlg1.isEqual(dp.hmlg1) &&
		   hmlg2.isEqual(dp.hmlg2);
	}
	else
	    return FALSE;
}

#if !defined ( __postfix_inc__ )
void    Diploid::setDominance()
{
	CollectionIterator  iter1(hmlg1);
	CollectionIterator  iter2(hmlg2);
	CollectionIterator  eiter(expressed);
	Atom dom;

	while (eiter) {
              dom = mapDominance(* (PAtomBase) iter1(), * (PAtomBase) iter2());
	      *((PAtomBase) eiter()) = (RCTDouble) dom;
	       ++eiter;
	       ++iter1;
	       ++iter2;
	}
}
#else
void    Diploid::setDominance()
{
	CollectionIterator  iter1(hmlg1);
	CollectionIterator  iter2(hmlg2);
	CollectionIterator  eiter(expressed);
	Atom dom;

	while (eiter) {
              dom = mapDominance(* (PAtomBase) iter1++, * (PAtomBase) iter2++);
	      *((PAtomBase) eiter()) = (RCTDouble) dom;
	       eiter++;
	}
}
#endif
