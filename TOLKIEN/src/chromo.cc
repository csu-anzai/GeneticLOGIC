//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include        "chromo.h"
#include	"trand.h"
#include	"xover.h"
#include        "IntVec.h"
#include	"ranksb.h"

static  PCTypeInfo chromoBases[] = { &TOArray::infoObj, 0 };
const TypeInfo  Chromosome::infoObj("Chromosome", chromoBases);

Chromosome::Chromosome(OwnerType ot, const sizeType size) :
	    TOArray(ot, size)
{
}

Chromosome::Chromosome(RCChromosome src, OwnerType dt) :
	TOArray((RCTOArray) src, dt)
{
}

void    Chromosome::evenPtCrossover(RCChromosome mother,
                                    RCChromosome father,
                                    int nXpts)
//
//      no checking on the length of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
{
        PCChromosome pShorter, pLonger;
	register int i, j, k, l, len, nXpt;
	BOOL   flag;

	removeAll();        // empty child first
	OwnerType  ot = this->getOwnerType();
	ownsElements(reference);

	if (mother.length() >= father.length()) {
	    pLonger = (PCChromosome) &mother;
	    pShorter = (PCChromosome) &father;
	}
	else {
	    pLonger =(PCChromosome) &father;
	    pShorter = (PCChromosome) &mother;
	}

	len = pShorter->length() - 1;
	*this = *pLonger;

        IntVec xpts(nXpts);

	//
	// sorted a sorted sequence of random numbers
	// in the range 0 to pShorter->length() - 1
	//
        ranksb.ordered(xpts, nXpts, pShorter->length());

	if (xpts[0] == 0) {
	    i = xpts.capacity() - 1;
	    if (xpts[i] == len)
	       xpts[i]--;
	}

        for (i = 0; i < nXpts; i+= 2) {
		j = xpts[i];
		k = xpts[i+1] - j;
		at(j, k) = pShorter->at(j, k);
	}

	if (ot == owner)
	    deepenShallowCopy();
}

void    Chromosome::oddPtCrossover(RCChromosome mother,
				   RCChromosome father,
				   int nXpt)
{
	PCChromosome pCurMate, pNextMate, pTemp;
	int cur, i, j, k, len;

	removeAll();        // empty child first
	OwnerType  ot = this->getOwnerType();
	ownsElements(reference);

	if (mother.length() >= father.length()) {
	    pNextMate = (PCChromosome) &mother;
	    pCurMate = (PCChromosome) &father;
	}
	else {
	    pNextMate = (PCChromosome) &father;
	    pCurMate = (PCChromosome) &mother;
	}

        //
	// generate a sorted sequence of random numbers
	// in the range 0 to pCurMate->length() - 1
	//

	IntVec xpts(nXpt);
	ranksb.ordered(xpts, nXpt, pCurMate->length());

	for (i=0, cur=0; i < nXpt; i++) {
	     *this += pCurMate->at(cur, xpts[i] - cur + 1);
/*
	       for (j = cur, k = xpts[i] - cur; k >=0; j++, k--)
		    setAt(j, pCurMate->elem(j));
*/
	       // swap parents
	       pTemp = pCurMate;
	       pCurMate = pNextMate;
	       pNextMate = pTemp;
               cur = xpts[i] + 1;
	}

	if (cur <= pCurMate->length())
	    // copy last segment
            *this += pCurMate->at(cur, pCurMate->length() - cur);
/*
	    for (j = cur, k = pCurMate->length() - cur - 1; k >=0; j++, k--)
		 setAt(j, pCurMate->elem(j));
*/
	if (ot == owner)
	    deepenShallowCopy();
}

void    Chromosome::uniformCrossover(RCChromosome mother,
				     RCChromosome father,
				     float flXRate)
{
        removeAll();
	OwnerType  ot = this->getOwnerType();
        ownsElements(reference);
        TOArray::operator=(mother);
        //
	// just copy pointers first
	//
        for (register int i=0; i < size(); i++)
	     if (tRand.flip(flXRate))
                  setAt(i, father.elem(i));

	if (ot == owner)
	    deepenShallowCopy();
}

void    Chromosome::printOn( ostream  & out) const
{
	CollectionIterator iter(*this);
	while ( iter ) {
	       out << *iter() << ' ';
               ++iter;
	}
}
