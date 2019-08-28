//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "ga.h"
#include "trand.h"
#include "IntVec.h"
#include "scale.h"
#include "slctrw.h"
#include "popsmpl.h"

GeneticAlgorithm::GeneticAlgorithm(PPopulation pInds,
				   PSelectionScheme pSelectScheme,
				   PCrossover pCrossover,
				   float flMutate,
				   float flGenGap,
				   PScalingScheme pScaleScheme,
				   BOOL  reEval) :
				   flMRate(rangeCheck(flMutate,0.0,1.0,
						      DEFAULT_MUTATRATE)),
				   pPop(pInds),
				   pXover(pCrossover),
				   pSSch(pSelectScheme),
				   pScale(pScaleScheme),
				   nTrials(1),
				   nTrialsMonitored(0),
				   flBestSoFar(FLT_MIN),
				   flSumOnLines(0),
				   flSumOffLines(0),
				   reEvaluate(reEval)
{
        double  flGenerationGap = rangeCheck(flGenGap,0.0,1.0,1.0);

	resetMonitor();

	PRECONDITION(pPop != NULL);

	//
	//      ensure an even number of individuals
	//
	if ((pPop->length() % 2) != 0) {
	    if (pPop->length() > 0)
		pPop->add( (PTGAObj) pPop->elem(0)->deepCopy() );

	    if ((pPop->length() % 2) != 0) {
		lib_error_handler("GeneticAlgorithm", "Requires a population \
with an even number of members");
		exit(-1);
	    }
	}

        if ((nOffSprings = pPop->length() * flGenerationGap) < 2)
            nOffSprings = 2;      // minimum number of new offsprings is two

	statistics();
}

GeneticAlgorithm::GeneticAlgorithm(PTGAObj pObj,
				   unsigned uPopSize,
				   PHENOTYPEFUNC pFunc) :
				   flMRate(0.003),
				   pXover(new OddPtCrossover(1,0.6)),
				   pSSch(new RW_Select()),
				   pScale(NULL),
				   reEvaluate(FALSE),
				   nTrials(1),
				   nTrialsMonitored(0),
				   flBestSoFar(FLT_MIN),
				   flSumOnLines(0),
				   flSumOffLines(0)

//
//      Constructor for a "simple" GA with the following parameters
//
//        1 point crossover with crossover rate 0.6
//        mutate rate = 0.003
//        roulette wheel selection scheme
//        generation gap = 1.0 (all individuals are replaced in each generation)
//        no scaling on individuals
//
//
//        the individual supplied are added to the population
{
	pPop = new SimplePopulation(pFunc);

        for (int i = uPopSize - 1; i >=0; i--)
	     pPop->add( (PTGAObj) pObj->deepCopy() );

	pPop->randomize();
        pPop->add(pObj);

	//
	//      ensure an even number of individuals
	//
	if ((pPop->length() % 2) != 0) {
	     pPop->add( (PTGAObj) pPop->elem(0)->deepCopy() );

	    if ((pPop->length() % 2) != 0) {
		lib_error_handler("GeneticAlgorithm", "Requires a population \
with an even number of members");
		exit(-1);
	    }
	}

        nOffSprings = pPop->length();

	statistics();
}

GeneticAlgorithm::~GeneticAlgorithm()
{
        if (pSSch)
            delete pSSch;
        if (pXover)
            delete pXover;
        if (pPop)
            delete pPop;
	if (pScale)
	    delete pScale;
}

void GeneticAlgorithm::deepenShallowCopy()
{
	pSSch = (PSelectionScheme) pSSch->deepCopy();
	pXover = (PCrossover) pXover->deepCopy();
	pPop = (PPopulation) pPop->deepCopy();
	if (pScale)
	    pScale = (PScalingScheme) pScale->deepCopy();
}

void GeneticAlgorithm::printOn(ostream& out) const
{
	pXover->printOn(out);
	out << endl;
	pSSch->printOn(out);
	out << endl;
	pScale->printOn(out);
	out << endl;
	pPop->printOn(out);
}

int GeneticAlgorithm::compare( RCTObject obj ) const
{
	if (isSame(obj))
	    return 0;
	else
	    return -1;
}

void GeneticAlgorithm::generation()
{
	register sizeType i, j;
        TOArray  newInds(reference, nOffSprings);
	PTGAObj  pTmpInd;

	nTrials++;      // one more trial

	pSSch->reset(*pPop);

        i = nOffSprings;
	while (i > 0) {
	     pXover->setMate(pSSch->select(),
			     pSSch->select());
	     for (j=2; j > 0 && i > 0; j--, i--) {
		  pTmpInd = pXover->create();
		  pTmpInd->mutate(flMRate);
		  newInds.add(pTmpInd);
	     }
	}

	pPop->replaceBy(newInds, reEvaluate);
	statistics();
}

void GeneticAlgorithm::scaleScheme(PScalingScheme pNewScale)
{
        if (pScale)
            delete pScale;
        pScale = pNewScale;
}

void GeneticAlgorithm::statistics()
{
	PTGAObj           pInd1, pInd2;
        sizeType          nInds;
	CollectionIterator      iter(pPop->operator RCCollection());

	nTrialsMonitored++;     // one more trial

	objStat.reset();        // reset statistics

#if !defined ( __postfix_inc__ )
	while ( iter ) {
	     objStat += ((PCTGAObj) iter())->objValue();
	     ++iter;
	}
#else
	if ( iter ) {
	     objStat += ((PCTGAObj) iter++)->objValue();
	     while ( iter )
		objStat += ((PCTGAObj) iter++)->objValue();
	}
#endif

	if (pScale) {

	    (*pScale)(*this,pBestInd,pWorstInd);

	    // fitness values may not equal objective values
	    // takes online and offline performance on objective values

	    if (nTrialsMonitored >= objOnLineVec.capacity()) {
		objOnLineVec.resize(objOnLineVec.capacity() + 50);
		objOffLineVec.resize(objOffLineVec.capacity() + 50);
	    }

	    flObjSumOnLines += objStat.mean();
	    if (objStat.max() > flObjBestSoFar)
		flObjBestSoFar = objStat.max();
	    flObjSumOffLines += flObjBestSoFar;

	    objOnLineVec[nTrialsMonitored - 1] =
		flObjSumOnLines / nTrialsMonitored;
	    objOffLineVec[nTrialsMonitored - 1] =
		flObjSumOffLines / nTrialsMonitored;

	}
	else
	   pPop->findBestandWorst(pBestInd,pWorstInd);

	fitStat.reset();           // reset statistics
	iter.restart();

#if !defined ( __postfix_inc__ )
	while ( iter ) {
	     fitStat += ((PCTGAObj) iter())->fitness();
	     ++iter;
	}
#else
	if ( iter ) {
	     fitStat += ((PCTGAObj) iter++)->fitness();
	     while ( iter )
		fitStat += ((PCTGAObj) iter++)->fitness();
	}
#endif

	if (nTrialsMonitored >= onLineVec.capacity()) {
	    onLineVec.resize(onLineVec.capacity() + 50);
	    offLineVec.resize(offLineVec.capacity() + 50);
	}

	flSumOnLines += fitStat.mean();
	if (fitStat.max() > flBestSoFar)
	    flBestSoFar = fitStat.max();
	flSumOffLines += flBestSoFar;

	onLineVec[nTrialsMonitored - 1] = flSumOnLines / nTrialsMonitored;
	offLineVec[nTrialsMonitored - 1] = flSumOffLines / nTrialsMonitored;
}
