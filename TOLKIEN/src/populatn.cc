//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "populatn.h"

static  PCTypeInfo populationBases[] = { &TGAObj::infoObj, 0 };
const TypeInfo  Population::infoObj("Population", populationBases);

void    Population::printOn( ostream & out) const
{
            for (register sizeType index=0;index < length(); index++) {
		elem(index)->printOn(out);
		out << " : " << elem(index)->objValue() << "," \
		<< elem(index)->fitness() << endl;
	    }
}

RPopulation Population::operator=(RCPopulation source)
{
	if (! isSame(source) ) {
            pInds->removeAll();
	    TGAObj::operator=(source);
	    pfuncPhenotype = ((RCPopulation) source).pfuncPhenotype;
	    *pInds = * source.pInds;
	}
	return *this;
}

void    Population::addAll(RCCollection from, BOOL evaluate)
{
	PRECONDITION( ! from.ownsElements() );

        CollectionIterator iter(from);
        PTObject           pObj;

        if (evaluate)
            while ( iter ) {
                pObj = iter();
                if ( pObj->isKindOf(TGAObj::typeInfo()) ) {
                    ((PTGAObj) pObj)->phenotype(pfuncPhenotype);
                    pInds->add(pObj);
                }
                else
                    error("addAll : object is not a kind of TGAObj");
                ++iter;
            }
        else
            while ( iter ) {
                pObj = iter();
                if ( pObj->isKindOf(TGAObj::typeInfo()) )
                    pInds->add(pObj);
                else
                    error("addAll : object is not a kind of TGAObj");
                ++iter;
            }
}

void  Population::randomize()
{
        for (sizeType index=pInds->size() - 1; index >= 0; index++)
	     elem(index)->randomize();
}

void     Population::mutate(float flRate)
{
	CollectionIterator iter(*pInds);
	while ( iter ) {
	      ((PTGAObj) iter())->mutate(flRate);
	      ++iter;
	}
}

void    Population::findBestandWorst(RPTGAObj pBestInd, RPTGAObj pWorstInd) const
//
//      use tournment selection to find the individuals with the largest and
//      smallest objective values
//
{
	sizeType            nInds;
	CollectionIterator  iter(*pInds);
	PTGAObj             pInd1, pInd2;

#if !defined ( __postfix_inc__ )
	pBestInd = pWorstInd = (PTGAObj) iter();

	//
	//   find best and worst individuals by tournament selection
	//
	if ((nInds = pInds->size()) > 1) {
	    //
	    // initialization : compare the first two individuals
	    //

	    pWorstInd = (PTGAObj) ++iter;

	    if ( pBestInd->objValue() < pWorstInd->objValue() ) {
		 pInd1 = pBestInd;
		 pBestInd = pWorstInd;
		 pWorstInd = pInd1;
	    }

	    nInds -= 2; // one pair compared

	    while (nInds > 1) {
		//
		// do for each pair of individuals
		//
		pInd1 = (PTGAObj) ++iter;
		pInd2 = (PTGAObj) ++iter;

		if ( pInd1->objValue() > pInd2->objValue() ) {
		     if (pInd1->objValue() > pBestInd->objValue())
			 pBestInd = pInd1;
		     if (pInd2->objValue() < pWorstInd->objValue())
			 pWorstInd = pInd2;
		}
		else {
		     if (pInd2->objValue() > pBestInd->objValue())
			 pBestInd = pInd2;
		     if (pInd1->objValue() < pWorstInd->objValue())
			 pWorstInd = pInd1;
		}
		nInds -= 2;
	    }

	    if (nInds > 0) {
		//
		//  compare the current best and worst with the last
		//  remaining individual
		//
		pInd1 = (PTGAObj) ++iter;

		if ( pInd1->objValue() > pBestInd->objValue() )
		     pBestInd = pInd1;
		else
		   if ( pInd1->objValue() < pWorstInd->objValue() )
			pWorstInd = pInd1;
	    }
	}
#else
	pBestInd = pWorstInd = (PTGAObj) iter++;
	//
	//   find best and worst individuals by tournament selection
	//
	if ((nInds = pInds->size()) > 1) {
	    //
	    // initialization : compare the first two individuals
	    //

	    pWorstInd = (PTGAObj) iter++;

            if ( pBestInd->objValue() < pWorstInd->objValue() ) {
		 pInd1 = pBestInd;
		 pBestInd = pWorstInd;
		 pWorstInd = pInd1;
	    }

	    nInds -= 2; // one pair compared

	    while (nInds > 1) {
		//
		// do for each pair of individuals
		//
		pInd1 = (PTGAObj) iter++;
		pInd2 = (PTGAObj) iter++;

                if ( pInd1->objValue() > pInd2->objValue() ) {
                     if (pInd1->objValue() > pBestInd->objValue())
			 pBestInd = pInd1;
                     if (pInd2->objValue() < pWorstInd->objValue())
			 pWorstInd = pInd2;
		}
		else {
                     if (pInd2->objValue() > pBestInd->objValue())
			 pBestInd = pInd2;
                     if (pInd1->objValue() < pWorstInd->objValue())
			 pWorstInd = pInd1;
		}
		nInds -= 2;
	    }

	    if (nInds > 0) {
		//
		//  compare the current best and worst with the last
		//  remaining individual
		//
		pInd1 = (PTGAObj) iter++;

                if ( pInd1->objValue() > pBestInd->objValue() )
		     pBestInd = pInd1;
		else
                   if ( pInd1->objValue() < pWorstInd->objValue() )
			pWorstInd = pInd1;
	    }
	}
#endif
}

void Population::replaceBy( RCollection newInds , BOOL reEvaluate )
//
//      old individuals are replaced in a sequential manner
//      newInds SHOULD NOT be the owner of its elements
//
{
	CollectionIterator 	iter(newInds);
	sizeType		i = 0;

	if ( reEvaluate ) {
	    //
	    // all individuals are evaluated
	    //

	    while ( iter && i < pInds->size() ) {
		pInds->setAt(i, iter());
		i++;
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
		pInds->setAt(i, iter());
		i++;
		iter++;
	    }

	    while ( iter ) { // remove the new individuals in excess
		delete iter();
		iter++;
	    }
	}
}

