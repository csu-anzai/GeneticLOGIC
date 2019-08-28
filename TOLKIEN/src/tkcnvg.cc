//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#include        "tkcnvg.h"
#include        "dpldbin.h"

//
// it is assumed that the population of the GA
// contains only instances of the classes BinHaploid and/or BinDiploid
//

BOOL    HammingConvergence::operator()(RCGeneticAlgorithm ga)
{
	CollectionIterator      iter((RCTOArray) ga.pop());
        PCBinIndividual         pBestInd;

        if (ga.bestInd().isKindOf(BinIndividual::typeInfo()))
            pBestInd = (PCBinIndividual) & ga.bestInd();
	else
            return FALSE;

	while ( iter ) {
		if (! pBestInd->isSame(*iter()))
                    if (pBestInd->hammingDistance((RCBitString)
                                            (*(PCBinIndividual) iter())) > hd)
                        return FALSE;
                ++iter;
	}
        return TRUE;
}
