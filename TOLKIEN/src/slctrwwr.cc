//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "slctrwwr.h"
#include "ranksb.h"

static  PCTypeInfo rwwrBase[] = { &SelectionScheme::infoObj, 0 };
const TypeInfo  RWwoR_Select::infoObj("RWwoR_Select", rwwrBase);

RWwoR_Select::RWwoR_Select(RCCollection src) :
	    SelectionScheme(src)
{
        curSz = src.size();
        buildList();
	sumFitness();
}

void  RWwoR_Select::buildList()
{
	objlist.removeAll();

        if (pPop) {
            IntVec ndx(pPop->size());
            ranksb.random(ndx,pPop->size(),pPop->size());
            // ndx is used to prevent positional bias

	    for (sizeType i = 0; i < pPop->size(); i++)
                objlist.add(pPop->at(ndx.elem(i)));
        }
}

void  RWwoR_Select::reset(RCCollection src)
{
        SelectionScheme::reset(src);
        curSz = src.size();
	sumFitness();
        buildList();
}

void RWwoR_Select::sumFitness()
{
	int nI;
	for (nI = 0, flSumFitness = 0.0; nI < pPop->size(); nI++)
             flSumFitness += member(nI)->fitness();
}

PCTGAObj RWwoR_Select::select()
{
    double flPartSum;
    Pix p;
    double flRand;
    PCTGAObj pObj;

    if (curSz > 0) {
	flRand = tRand.asFloat() * flSumFitness;
	p = objlist.first();
	pObj = (PCTGAObj) objlist.front();
	flPartSum = ((PCTGAObj) objlist(p))->fitness();
	if ((flRand > flPartSum) || (curSz == 1)) {
	    objlist.del(p);
	    flSumFitness -= pObj->fitness();
	    curSz--;
            return pObj;
	}
	else {
	    do {
		objlist.next(p);
		flPartSum += ((PCTGAObj) objlist(p))->fitness();
	    } while ((flPartSum < flRand) && (p != objlist.last()));
	}

	pObj = (PCTGAObj) objlist(p);
	objlist.del(p);
	flSumFitness -= pObj->fitness();
	curSz--;
        return pObj;
    }

    return (PCTGAObj) NOOBJECT;
}

