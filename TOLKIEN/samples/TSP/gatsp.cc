//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "gatsp.h"
#include "scale.h"
#include "slctrw.h"
#include "tsp.h"
#include "tour.h"

extern fstream 	msgout;
extern fstream 	statfile;
extern fstream 	profile;
extern fstream 	indfile;
extern fstream 	bestfile;
extern fstream 	bestcostfile;
extern fstream	bestinfile;

void    POP_GA::findBestandWorst(RPTGAObj pBestInd, RPTGAObj pWorstInd)
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

            if ( pBestInd->objValue() > pWorstInd->objValue() ) {
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

                if ( pInd1->objValue() < pInd2->objValue() ) {
		     if (pInd1->objValue() < pBestInd->objValue())
			 pBestInd = pInd1;
                     if (pInd2->objValue() > pWorstInd->objValue())
			 pWorstInd = pInd2;
		}
		else {
                     if (pInd2->objValue() < pBestInd->objValue())
			 pBestInd = pInd2;
                     if (pInd1->objValue() > pWorstInd->objValue())
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

                if ( pInd1->objValue() < pBestInd->objValue() )
		     pBestInd = pInd1;
		else
                   if ( pInd1->objValue() > pWorstInd->objValue() )
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

            if ( pBestInd->objValue() > pWorstInd->objValue() ) {
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

                if ( pInd1->objValue() < pInd2->objValue() ) {
                     if (pInd1->objValue() < pBestInd->objValue())
			 pBestInd = pInd1;
                     if (pInd2->objValue() > pWorstInd->objValue())
			 pWorstInd = pInd2;
		}
		else {
                     if (pInd2->objValue() < pBestInd->objValue())
			 pBestInd = pInd2;
                     if (pInd1->objValue() > pWorstInd->objValue())
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

                if ( pInd1->objValue() < pBestInd->objValue() )
		     pBestInd = pInd1;
		else
                   if ( pInd1->objValue() > pWorstInd->objValue() )
			pWorstInd = pInd1;
	    }
	}
#endif
}

BOOL TSP_GA::run(int nGen)
{
	int count = nGen;

        do {
           generation();
           if (trials() % 50 == 0)
               resetMonitor();

           if ( converged() ) {
		cout << "converged" << endl;
		restart();
		return FALSE;
	   }
        } while (count-- > 0);

	return TRUE;
}

void TSP_GA::restart()
{
	sizeType popSize = pPop->size() * 0.5;
	TOArray newInds;
	PTSPind	pTour, pTemplate = (PTSPind) bestInd().deepCopy();
	double oldCost = pTemplate->objValue();

	while ( --popSize > 0) {
		pTour = (PTSPind) pTemplate->deepCopy();
		pTour->mutate(1.0);
                pTour->phenotype(pPop->phenotypeFunc());
		pTour->optimizeLocal();
	   	newInds.add( (PTObject) pTour);

		pTour = (PTSPind) pTemplate->deepCopy();
		pTour->randomize();
                pTour->phenotype(pPop->phenotypeFunc());
		pTour->optimizeLocal();
	   	newInds.add( (PTObject) pTour);
	}

	pPop->Population::replaceBy(newInds, FALSE);
	CollectionIterator iter(pPop->operator RCCollection());
	while (iter) {
		cout << ((PCTGAObj) iter())->objValue() << " : " << ((PCTGAObj) iter())->fitness() << endl;
		iter++;
	}

	pTemplate->orOpt();
	pPop->setAt(0, (PTGAObj) pTemplate);

	statistics();
	resetMonitor();
	nTrials = 0;
}

void TSP_GA::generation()
{
	register sizeType i = nOffSprings, j, k;
	PTGAObj  pTmpInd;
	double   oldCost;
	PCTGAObj pMother, pFather;

	nTrials++;      // one more trial

	pSSch->reset(*pPop);

	TOArray  newInds(reference, nOffSprings);

        while (i-- > 0) {
		 pFather = pSSch->select();
		 pMother = pSSch->select();
		 pXover->setMate(pFather, pMother);
		 pTmpInd = pXover->create();
		 pTmpInd->mutate(flMRate);
		 pTmpInd->phenotype(pPop->phenotypeFunc());
                 oldCost = pTmpInd->objValue();
                 ((PTSPind) pTmpInd)->optimizeLocal();
                 if (oldCost == pTmpInd->objValue())
                     // no improvement using two-opt
                     ((PTSPind) pTmpInd)->orOpt();
                 newInds.add(pTmpInd);
        }

        pPop->replaceBy(newInds, reEvaluate);
	statistics();

	if (TSPind::printFlag) {
	    profile << TSPind::crossTotal << " : " << bestInd().objValue() << endl;
	    TSPind::printFlag = FALSE;
	}

	statfile << objStat.min() << ' ' << objStat.max()  << ' ' <<  objStat.mean();
	statfile  << ' ' << objStat.stdDev() << endl;
}

TSP_GA::TSP_GA( PPopulation pInds,
		PSelectionScheme pSelectScheme,
		PCrossover pCrossover,
		float flMutate,
		float flGenGap,
		PScalingScheme pScaleScheme,
		BOOL  reEval) :
		GeneticAlgorithm(),
		retries(0)
{
	double  flGenerationGap = rangeCheck(flGenGap,0.0,1.0,1.0);

	flMRate=rangeCheck(flMutate,0.0,1.0, DEFAULT_MUTATRATE);
	flGenerationGap=rangeCheck(flGenGap,0.0,1.0, 1.0);
	pPop=pInds;
	pXover=pCrossover;
	pSSch=pSelectScheme;
	pScale=pScaleScheme;
	reEvaluate=reEval;
	nTrials=1;
	nTrialsMonitored=0;
	flBestSoFar=FLT_MIN;
	flSumOnLines=0;
	flSumOffLines=0;

	PRECONDITION(pPop != NULL);

	//
	//      ensure an even number of individuals
	//
	if ((pPop->length() % 2) != 0) {
	    if (pPop->length() > 0)
		pPop->add( (PTGAObj) pPop->elem(0)->deepCopy() );

	    if ((pPop->length() % 2) != 0) {
		lib_error_handler("TSP_GA", "Requires a population \
with an even number of members");
		exit(-1);
	    }
	}

	if ((nOffSprings = pPop->length() * flGenerationGap) < 2)
	    nOffSprings = 2;      // minimum number of new offsprings is two

	statistics();
}

BOOL TSP_GA::converged() const
{
     if (worstInd().objValue() - bestInd().objValue() != 0.0)
	 return FALSE;
     else
	 return TRUE;
}

void TSP_GA::statistics()
{
	PTGAObj           pInd1, pInd2;
	sizeType          nInds;
	CollectionIterator      iter(*pPop);

	nTrialsMonitored++;     // one more trial

	objStat.reset();        // reset statistics

	while ( iter ) {
	     objStat += ((PCTGAObj) iter())->objValue();
	     ++iter;
	}

	if (pScale)
	   (*pScale)(*this,pBestInd,pWorstInd);       // scale fitness before taking
						      // fitness statistics
       else
           pPop->findBestandWorst(pBestInd,pWorstInd);

	fitStat.reset();           // reset statistics

        iter.restart();
	fitStat += pBestInd->fitness();

/*
	pBestInd->printOn(statfile);
	statfile << ' ' << pBestInd->objValue() << ',' << pBestInd->fitness() << endl;
	pWorstInd->printOn(statfile);
	statfile << ' ' << pWorstInd->objValue() << ',' << pWorstInd->fitness() << endl;
*/

	if (nTrialsMonitored >= onLineVec.capacity())
	    onLineVec.resize(onLineVec.capacity() + 50);
	if (nTrialsMonitored >= offLineVec.capacity())
	    offLineVec.resize(offLineVec.capacity() + 50);

        flSumOnLines += fitStat.mean();
        if (fitStat.max() > flBestSoFar)
	    flBestSoFar = fitStat.max();
	flSumOffLines += flBestSoFar;

	onLineVec[nTrialsMonitored - 1] = flSumOnLines / nTrialsMonitored;
	offLineVec[nTrialsMonitored - 1] = flSumOffLines / nTrialsMonitored;
}

void	POP_GA::replaceBy(RCollection newInds, BOOL reEvaluate)
//	the phenotype of individuals in newInds should already
// 	be evaluated
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
	    ((PTOArray) pInds)->sort(decObjValue);
	    if ( reEvaluate ) {
		//
		// evaluate all individuals
		//
		for (i=0; i < newInds.size(); i++)
		     pInds->setAt(i, newInds.elem(i));
		for (i = pInds->size() - 1; i >= 0; i--)
		     elem(i)->phenotype(pfuncPhenotype);
	    }
	    else
		for (i=0; i < newInds.size(); i++)
		     pInds->setAt(i, newInds.elem(i));
	}
}

