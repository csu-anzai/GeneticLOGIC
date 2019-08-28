//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "dpldbin.h"
#include "trand.h"
#include "xover.h"

static  PCTypeInfo bindiploidBases[] = { &BinIndividual::infoObj, 0 };
const TypeInfo  BinDiploid::infoObj("BinDiploid", bindiploidBases);

void    BinDiploid::normalize()
{
	if (hmlg1.length() > hmlg2.length())
	    hmlg2.resize(hmlg1.length());
	else if (hmlg2.length() > hmlg1.length())
	    hmlg1.resize(hmlg2.length());

	expressed.resize(hmlg1.length());
}

void    BinDiploid::setDominance()
{
      for (register int nI = expressed.length() - 1; nI >= 0; nI--)
	   mapDominanceAt(nI);
}

void    BinDiploid::printOn( ostream  & out) const
{
	    hmlg1.printOn(out);
	    out << " : ";
	    hmlg2.printOn(out);
	    out << " => ";
	    expressed.printOn(out);
}

RBinIndividual BinDiploid::operator=(RCBitString bits)
{
        hmlg1 = bits;
        hmlg2 = hmlg1;
	normalize();
	setDominance();
	return *this;
}

RBinIndividual BinDiploid::operator=(RCBinHaploid source)
{
	TGAObj::operator=(source);
	hmlg1 = (RCBitString) source;
	hmlg2 = hmlg1;
	normalize();
	setDominance();
	return *this;
}

RBinIndividual BinDiploid::operator=(RCBinDiploid source)
{
	if (! isSame(source) ) {
	     TGAObj::operator=(source);
	     hmlg1 = ((RCBinDiploid) source).hmlg1;
	     hmlg2 = ((RCBinDiploid) source).hmlg2;
	     normalize();
	     setDominance();
	}
	return *this;
}

void    BinDiploid::randomize()
{
	hmlg1.randomize();
	hmlg2.randomize();
	setDominance();
}

void     BinDiploid::mutate(float flRate)
{
	for (register int nI=length() - 1; nI >=0; nI--)
	     if (tRand.flip(flRate)) {
		 hmlg1.setRand(nI);
		 hmlg2.setRand(nI);
		 mapDominanceAt(nI);
	     }
}

void BinDiploid::resize(unsigned uNewLen)
{
	    uNewLen = rangeCheck(uNewLen,1,MAX_INDLEN,DEFAULT_INDLEN);
            hmlg1.resize(uNewLen);
            hmlg2.resize(uNewLen);
            expressed.resize(uNewLen);
}

PTGAObj  BinDiploid::oddPtCrossover(RCTGAObj mate, int nXpt) const
{
	if (mate.isKindOf(BinHaploid::typeInfo()))
            return _oddPtCrossover((RCBinHaploid) mate, nXpt);
	else
	    if (mate.isKindOf(BinDiploid::typeInfo()))
                return _oddPtCrossover((RCBinDiploid) mate, nXpt);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    BinDiploid::oddPtCrossover(RCTGAObj mate, int nXpt, RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
            _oddPtCrossover((RCBinDiploid) mate, nXpt, (RBinDiploid) child);
	else if (mate.isKindOf(BinHaploid::typeInfo()))
            _oddPtCrossover((RCBinHaploid) mate, nXpt, (RBinDiploid) child);
}

PTGAObj  BinDiploid::evenPtCrossover(RCTGAObj mate, int nXpts) const
{
	if (mate.isKindOf(BinHaploid::typeInfo()))
            return _evenPtCrossover((RCBinHaploid) mate, nXpts);
	else
	    if (mate.isKindOf(BinDiploid::typeInfo()))
                return _evenPtCrossover((RCBinDiploid) mate, nXpts);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    BinDiploid::evenPtCrossover(RCTGAObj mate, int nXpts, RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
            _evenPtCrossover((RCBinDiploid) mate, nXpts, (RBinDiploid) child);
	else if (mate.isKindOf(BinHaploid::typeInfo()))
            _evenPtCrossover((RCBinHaploid) mate, nXpts, (RBinDiploid) child);
}

PTGAObj  BinDiploid::uniformCrossover(RCTGAObj mate, float flXRate) const
{
	if (mate.isKindOf(BinHaploid::typeInfo()))
            return _uniformCrossover((RCBinHaploid) mate, flXRate);
	else
	    if (mate.isKindOf(BinDiploid::typeInfo()))
                return _uniformCrossover((RCBinDiploid) mate, flXRate);
	    else
		error("Invalid mate species!");
        return (PTGAObj) NOOBJECT;
}

void    BinDiploid::uniformCrossover(RCTGAObj mate, float flXRate,
				     RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	if (mate.isKindOf(isA()))
            _uniformCrossover((RCBinDiploid) mate, flXRate,
			      (RBinDiploid) child);
	else if (mate.isKindOf(BinHaploid::typeInfo()))
	    _uniformCrossover((RCBinHaploid) mate, flXRate,
			      (RBinDiploid) child);
}

PTGAObj  BinDiploid::_oddPtCrossover(RCBinDiploid mate, int nXpt) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   pChild->hmlg2.oddPtCrossover(mate.hmlg1, mate.hmlg2, nXpt);

   pChild->normalize();
   pChild->setDominance();

   return pChild;
}

PTGAObj  BinDiploid::_oddPtCrossover(RCBinHaploid mate, int nXpt) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   pChild->hmlg2 = (RCBitString) mate;

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    BinDiploid::_oddPtCrossover(RCBinHaploid mate, int nXpt,
                                    RBinDiploid child) const
{
   child.hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   child.hmlg2 = mate;

   child.normalize();
   child.setDominance();
}

void    BinDiploid::_oddPtCrossover(RCBinDiploid mate, int nXpt,
                                    RBinDiploid child) const
{
   child.hmlg1.oddPtCrossover(hmlg1, hmlg2, nXpt);
   child.hmlg2.oddPtCrossover(mate.hmlg1, mate.hmlg2, nXpt);

   child.normalize();
   child.setDominance();
}

PTGAObj  BinDiploid::_evenPtCrossover(RCBinDiploid mate, int nXpts) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   pChild->hmlg2.evenPtCrossover(mate.hmlg1, mate.hmlg2, nXpts);

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

PTGAObj  BinDiploid::_evenPtCrossover(RCBinHaploid mate, int nXpts) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   pChild->hmlg2 = (RCBitString) mate;

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    BinDiploid::_evenPtCrossover(RCBinHaploid mate, int nXpts,
                                     RBinDiploid child) const
{
   child.hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   child.hmlg2 = mate;

   child.normalize();
   child.setDominance();
}

void    BinDiploid::_evenPtCrossover(RCBinDiploid mate, int nXpts,
                                     RBinDiploid child) const
{
   child.hmlg1.evenPtCrossover(hmlg1, hmlg2, nXpts);
   child.hmlg2.evenPtCrossover(mate.hmlg1, mate.hmlg2, nXpts);

   child.normalize();
   child.setDominance();
}

PTGAObj  BinDiploid::_uniformCrossover(RCBinDiploid mate, float flXRate) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   pChild->hmlg2.uniformCrossover(mate.hmlg1, mate.hmlg2, flXRate);

   pChild->normalize();
   pChild->setDominance();

   return pChild;
}

PTGAObj  BinDiploid::_uniformCrossover(RCBinHaploid mate, float flXRate) const
{
   PBinDiploid  pChild = (PBinDiploid) deepCopy();

   pChild->hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   pChild->hmlg2 = (RCBitString) mate;

   pChild->normalize();
   pChild->setDominance();
   return pChild;
}

void    BinDiploid::_uniformCrossover(RCBinHaploid mate, float flXRate,
                                      RBinDiploid child) const
{
   child.hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   child.hmlg2 = mate;

   child.normalize();
   child.setDominance();
}

void    BinDiploid::_uniformCrossover(RCBinDiploid mate, float flXRate,
                                      RBinDiploid child) const
{
   child.hmlg1.uniformCrossover(hmlg1, hmlg2, flXRate);
   child.hmlg2.uniformCrossover(mate.hmlg1, mate.hmlg2, flXRate);

   child.normalize();
   child.setDominance();
}

unsigned    BinDiploid::hammingDistance(RCBinDiploid src) const
{
        return expressed.hammingDistance(src.expressed);
}

unsigned    BinDiploid::hammingDistance(RCBitString src) const
{
        return expressed.hammingDistance(src);
}

double      BinDiploid::distance(RCTGAObj obj) const
{
        if ( obj.isKindOf(isA()) )
            return hammingDistance((RCBinDiploid) obj);
        else if ( obj.isKindOf(BinHaploid::typeInfo()) )
                  return hammingDistance((RCBinHaploid) obj);
        else
	     return MAXDOUBLE;
}

BinDiploid::BinDiploid(RCTritString src1,
                       RCTritString src2,
                       BOOL grayFlag)
{
	hmlg1 = src1;
        hmlg2 = src2;
	normalize();
	setDominance();
        expressed.isGrayCode(grayFlag);
}

BinDiploid::BinDiploid(unsigned nLength,
                       unsigned uFlag,
                       BOOL grayFlag) :
	    hmlg1(rangeCheck(nLength,1,MAX_CHROMLEN,DEFAULT_CHROMLEN), uFlag),
	    hmlg2(rangeCheck(nLength,1,MAX_CHROMLEN,DEFAULT_CHROMLEN), uFlag)
{
        expressed.resize(hmlg1.length());
	setDominance();
        expressed.isGrayCode(grayFlag);
}

BOOL BinDiploid::isEqual( RCTObject obj ) const
{
	if ( obj.isA() == isA() &&
	     TGAObj::isEqual(obj) ) {
		if ( ! ( hmlg1 == ((RCBinDiploid) obj).hmlg1) )
		     return FALSE;
		else
		     return hmlg2 == ((RCBinDiploid) obj).hmlg2;
	}
        else
            return FALSE;

}

void  BinDiploid::invert(int nIndex)
{
	// add 1 to each allele
	// 1 + 1 -> -1, 0 + 1 -> 1, -1 + 1 -> 0
	int nVal;

	expressed.invert(nIndex);
	if ((nVal = hmlg1.valueAt(nIndex) + 1) > 1)
	    nVal = dontCare;
	hmlg1.setAt(nIndex, nVal);
	if ((nVal = hmlg2.valueAt(nIndex) + 1) > 1)
	    nVal = dontCare;
	hmlg2.setAt(nIndex, nVal);
}

void  BinDiploid::set(int nIndex)
{
	PTritString pH1, pH2;

	if (tRand.flip(0.5)) {
	    pH1 = & hmlg1;
	    pH2 = & hmlg2;
	}
	else {
	    pH1 = & hmlg2;
	    pH2 = & hmlg1;
	}

	pH1->setAt(nIndex,1);
	if (tRand.flip(0.5))
	    pH2->setAt(nIndex, 0);
	else
	    pH2->setAt(nIndex, dontCare);

	expressed.set(nIndex);
}

void  BinDiploid::clear(int nIndex)
{
	PTritString pH1, pH2;

	if (tRand.flip(0.5)) {
	    pH1 = & hmlg1;
	    pH2 = & hmlg2;
	}
	else {
	    pH1 = & hmlg2;
	    pH2 = & hmlg1;
	}

	pH1->setAt(nIndex,0);
	if (tRand.flip(0.5))
	    pH2->setAt(nIndex, 0);
	else
	    pH2->setAt(nIndex, dontCare);

	expressed.clear(nIndex);
}
