//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "slctrw.h"
#include "ranksb.h"

static  PCTypeInfo rwselectBases[] = { &SelectionScheme::infoObj, 0 };
const TypeInfo  RW_Select::infoObj("RW_Select", rwselectBases);

RW_Select::RW_Select(RCCollection src) :
           SelectionScheme(src), cumSlots(src.size() + 1)
{
        reset(src);
}

void  RW_Select::reset(RCCollection src)
{
	int i, j, k, tmp;

	SelectionScheme::reset(src);
        // generate a random sequence of [0 .. N-1]
        ranksb.random(ndx, src.size(), src.size());
        sumFitness();

}

void RW_Select::sumFitness()
{
        int nI, nJ;

        if (cumSlots.capacity() <= pPop->size())
            cumSlots.resize(pPop->size() + 1);

        cumSlots[0] = 0;
        for (nI = 0, nJ = 1, flSumFitness = 0; nI < pPop->size(); nI++, nJ++) {
             flSumFitness += member(ndx[nI])->fitness();
             cumSlots[nJ] = flSumFitness;
        }
}

unsigned RW_Select::_select()
{
    double flRand = tRand.asFloat() * flSumFitness;
    sizeType first = 1, last = pPop->size(), index;

    while (first <= last) {
	index = (first + last) / 2;
	// return index if flRand is smaller than or equal to
	// the cumulative total at index and larger than the
	// cumulative total at (index - 1)
        if ((flRand <= cumSlots[index]) && (flRand > cumSlots[index - 1]))
            return ndx[index - 1];
        else if (flRand < cumSlots[index])
                 last = index - 1;
        else
                first = index + 1;
    }

    error("_select error");     // should not reach here
}

/*
unsigned RW_Select::_select()
{
    int nI = -1, nPopSizeLessOne = pPop->size() - 1;
    double flPartSum = 0;

    double flRand = tRand.asFloat() * flSumFitness;

    do {    nI++;
            flPartSum += member(ndx.elem(nI))->fitness();
    } while ((flPartSum < flRand) && (nI < nPopSizeLessOne));
    return nI;
}
*/


