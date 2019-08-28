//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "slctsus.h"
#include "trand.h"
#include "DblVec.h"
#include "ranksb.h"

static  PCTypeInfo susBases[] = { &SelectionScheme::infoObj, 0 };
const TypeInfo  SUS_Select::infoObj("SUS_Select", susBases);

SUS_Select::SUS_Select(RCCollection src)
{
	SelectionScheme::reset(src);
	mates.capacity(src.size());
	reset();
}

SUS_Select::SUS_Select(RCCollection src, sizeType nInd)
{
	SelectionScheme::reset(src);
	if ((nInd == 0) || (nInd > src.size()))
	    nInd = src.size();
	mates.capacity(nInd);
	reset(nInd);
}

void    SUS_Select::reset(sizeType nInd)
{
	sizeType i;
	double factor, sum, ptr;
	double incr;
	DblVec expVal(pPop->size());

	if ((nInd == 0) || (nInd > pPop->size()))
	    nInd = pPop->size();

	remains = nInd;

	if (nInd != pPop->size()) {
	    incr = pPop->size() / (double) nInd;
	    ptr = tRand(0, incr);
	}
	else {
	    incr = 1;
	    ptr = tRand.asFloat();
	}

	mates.removeAll();
	ranksb.random(ndx, pPop->size(), pPop->size());

	// calculate sum of fitness
        for (i = 0, factor = 0; i < pPop->size(); i++)
	     factor += ((PCTGAObj) pPop->elem(i))->fitness();

	factor = pPop->size() / factor;

	for (i = 0; i < pPop->size(); i++)
	     expVal[i] = ((PCTGAObj) pPop->elem(ndx[i]))->fitness() * factor;

	for (sum=i=0; i < pPop->size(); i++)
	     for (sum += expVal[i]; sum > ptr; ptr += incr)
		  mates.add((PTObject) pPop->elem(ndx[i]));
}

void    SUS_Select::reset(RCCollection src, sizeType nInd)
{
	SelectionScheme::reset(src);
	reset(nInd);
}

PCTGAObj SUS_Select::select()
{
    if (remains-- <= 0)
	reset();
    return (PCTGAObj) mates.elem(remains);
}
