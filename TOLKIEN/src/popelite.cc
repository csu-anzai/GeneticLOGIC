//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "popelite.h"

static  PCTypeInfo popeliteBases[] = { &Population::infoObj, 0 };
const TypeInfo  ElitePopulation::infoObj("ElitePopulation", popeliteBases);

void ElitePopulation::replaceBy( RCollection newInds ,
                                 BOOL reEvaluate )
//
//      replace old individuals with members in newInds
//      individuals with the least fitness is replaced first
//
//      newInds SHOULD NOT be the owner of its elements
//
{
        sizeType i, j, nTemp;
        PTGAObj  pObj;

        if (newInds.size() >= pInds->size()) {
            //
	    // the whole population is replaced
            //
            // if the size of newInds is greater than inds
            // the extra elements in newInds are deleted
            //

            CollectionIterator iter(newInds);

            i = pInds->size();
            pInds->removeAll();

	    while (i-- > 0) {
		((PTGAObj) iter())->phenotype(pfuncPhenotype);
                pInds->add(iter());
                ++iter;
	    }

            while ( iter ) {
                //
                // delete the extra elements in newInds
                //
                delete iter();
                iter++;
            }
	}
        else {
            //
            // sort member in ascending order of fitness
            //

	    ((PTOArray) pInds)->sort(ascFitness);

            if ( reEvaluate ) {
                //
                // evaluate all individuals
                //
                for (i=newInds.size() - 1; i >= 0; i--)
                     pInds->setAt(i, newInds.elem(i));
                for (i = pInds->size() - 1; i >= 0; i--)
                     elem(i)->phenotype(pfuncPhenotype);
            }
            else
                for (i=0,  j = newInds.size() - 1; j >= 0; i++, j--) {
                     //
                     // evaluate new individuals only
                     //
                     pObj = (PTGAObj) newInds.elem(j);
		     pObj->phenotype(pfuncPhenotype);
                     pInds->setAt(i, (PTObject) pObj);
                }
        }
}

