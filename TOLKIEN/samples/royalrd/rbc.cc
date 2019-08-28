#include "ranksb.h"
#include "dpldbin.h"

//
//      This function is Davis' random bit climbing algorithm
//	described in 4th ICGA
//
//	this function may perform much better than a GA
//	for royal road functions (and other low epistasis functions)
//
BOOL	rbc(RBinIndividual obj, PHENOTYPEFUNC func)
{
	int i;
	IntVec flipList(obj.length());
	double bestEval = obj.objValue(); // assumed obj is already evaluated by func
	double newEval;
	BOOL   bChange = FALSE;
	PBinIndividual pInd = obj.deepCopy();

	ranksb.random(flipList);
	for (i = 0; i < obj.length(); i++) {
	     pInd->invert(flipList[i]);
             newEval = func(*pInd);
	     if ( newEval > obj.objValue() ) {
                  obj.invert(flipList[i]);
		  obj.objValue() = newEval;
		  bChange = YES;
             }
             else
	          pInd->invert(flipList[i]); // undo
        }

	delete pInd;
	return bChange;
}
