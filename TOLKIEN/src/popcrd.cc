//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "popcrd.h"

static  PCTypeInfo popcrdBases[] = { &Population::infoObj, 0 };
const TypeInfo  CrowdingPopulation::infoObj("CrowdingPopulation", popcrdBases);

sizeType CrowdingPopulation::worstInd() const
//
//      select worst individual from a random subpopulation
//      of size uCrowdingSubPop
//
//      this function assumes the size of the population is nonzero
//
{
        sizeType  uWorst, uCandidate;
	float   flWorstStrength, flVal;

	uWorst = tRand(0, length() - 1);

        flWorstStrength = ((PCTGAObj) pInds->elem(uWorst))->fitness();

        for (register sizeType i = uCrowdingSubPop - 1; i >= 0; i--) {
             uCandidate = tRand(0, length() - 1);
             flVal = ((PCTGAObj) pInds->elem(uCandidate))->fitness();
             if (flWorstStrength > flVal) {
                 uWorst = uCandidate;
                 flWorstStrength = flVal;
             }
        }

	return uWorst;
}

sizeType CrowdingPopulation::select(RCTGAObj child) const
//
//      return an index to the individual to be replaced
//      using modified De Jong crowding
//
{
	int             nMatch, nMatchMax = -1;
	sizeType        member, mostSimilar;

	for (register sizeType j = uCrowdingFactor - 1; j >=0 ; j--) {
                member = worstInd();
		nMatch = child.distance(* ((PTGAObj) elem(member)));
		if (nMatch > nMatchMax) {
		    nMatchMax = nMatch;
		    mostSimilar = member;
		}
	}

	return mostSimilar;
}

void CrowdingPopulation::replaceBy( RCollection newInds ,
                                    BOOL reEvaluate )
//
//      replace old individuals with members in newInds using De Jong's
//      crowding model
//
//      newInds SHOULD NOT be the owner of its elements
//
{
	CollectionIterator 	iter(newInds);
	sizeType		index;

	//
        // if uCrowdingFactor or uCrowdingSubPop is zero or
        // newInds is larger than inds replaces individuals
        // in a random manner
	//
        if ( uCrowdingFactor == 0 ||
             uCrowdingSubPop == 0 ||
             newInds.size() >= pInds->size() ) {
            Population::replaceBy(newInds, reEvaluate);
	    return;
	}

	if ( reEvaluate ) {
            //
            // all individuals are evaluated
            //
	    while ( iter ) {
		index = select(* (PCTGAObj) iter());
		pInds->setAt(index, iter());
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
	    while ( iter ) {
                ((PTGAObj) iter())->phenotype(pfuncPhenotype);
                index = select(* (PCTGAObj) iter());
                pInds->setAt(index, iter());
		iter++;
	    }
	}
}

