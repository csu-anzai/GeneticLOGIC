//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#include "popsmpl.h"
#include "ranksb.h"

static  PCTypeInfo spopBases[] = { &Population::infoObj, 0 };
const TypeInfo  SimplePopulation::infoObj("SimplePopulation", spopBases);

void SimplePopulation::replaceBy( RCollection newInds ,
				  BOOL reEvaluate )
//
//      old individuals are replaced in a random manner
//      newInds SHOULD NOT be the owner of its elements
//
{
	CollectionIterator 	iter(newInds);
	sizeType		i = 0;

	// generate a random subset of integers in [0,pPop->size() - 1]
        ranksb.random(ndx,
                      newInds.size() < pInds->size() ?
                                newInds.size() : pInds->size(),
                      pInds->size());

	if ( reEvaluate ) {
	    //
	    // all individuals are evaluated
	    //

            while ( iter && i < pInds->size() ) {
		pInds->setAt(ndx[i++], iter());
		iter++;
	    }

            while ( iter ) { // remove the new individuals in excess
		delete iter();
                iter++;
            }

	    iter.restart(*pInds);
	    while ( iter ) {
		((PTGAObj) iter())->phenotype(pfuncPhenotype);
		iter++;
	    }
	}
	else {
	    //
	    // only new individuals are evaluated
	    //
            while ( iter && i < pInds->size() ) {
		((PTGAObj) iter())->phenotype(pfuncPhenotype);
		pInds->setAt(ndx[i++], iter());
		iter++;
	    }

            while ( iter ) { // remove the new individuals in excess
		delete iter();
                iter++;
            }
        }
}

