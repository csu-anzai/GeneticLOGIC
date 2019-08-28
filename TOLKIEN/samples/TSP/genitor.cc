//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//
//      This program uses the edge recombination operator
//      to solve the symmetric TSP
//
//      The GA used is GENITOR-like
//
//
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "scale.h"
#include "select.h"
#include "tsp.h"
#include "tour.h"
#include "ga.h"
#include "sortcltn.h"

#ifndef DEFAULT_BIAS
#define DEFAULT_BIAS 1.25
#endif

unsigned nGen = 100 , uPopSize = 200;
float    flMRate = 0.1;
float    flXRate = 1;
TourInfo	*pTourInfo;

_CLASSDEF(Genitor)
_CLASSDEF(MyLinearRanking)

class  MyLinearRanking : public SelectionScheme
//
//      Whitley's linear ranking selection
//
//      it is assumed that the population to be selected
//      has already been sorted
//
{
public:
				DECLARE_RTTI()

				MyLinearRanking(double=DEFAULT_BIAS);
				MyLinearRanking(RCMyLinearRanking);
				MyLinearRanking(RCCollection);
				~MyLinearRanking();

				_shallowCopy(MyLinearRanking)
	virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void 		reset(RCCollection src);
	virtual double          bias() const;
	virtual double          bias(double);

protected:
	double          flBias, flBiasSqr, flBiasLessOne;
	PCollection	pSorted;
};

inline MyLinearRanking::MyLinearRanking(double val) : SelectionScheme()
{
        bias(val);
}

inline MyLinearRanking::MyLinearRanking(RCMyLinearRanking src) :
        SelectionScheme(src), flBias(src.flBias), pSorted(src.pSorted)
{
}

inline MyLinearRanking::~MyLinearRanking()
{
}

inline void    MyLinearRanking::printOn( ostream  & out) const
{
        out << nameOf() << " : " << flBias << endl << *pPop;
}

inline  void   MyLinearRanking::reset(RCCollection src)
{
	pSorted = (PCollection) & src;
}

inline  double MyLinearRanking::bias() const
{
        return flBias;
}

inline  double MyLinearRanking::bias(double newBias)
{
	double tmp = flBias;
	flBias = newBias > 1 ? newBias : DEFAULT_BIAS;
	flBiasSqr = flBias * flBias;
	flBiasLessOne = flBias - 1;
	return tmp;
}

static  PCTypeInfo linearRankBases[] = { &SelectionScheme::infoObj, 0 };
const TypeInfo  MyLinearRanking::infoObj("Linear Ranking", linearRankBases);

PCTGAObj MyLinearRanking::select()
{
	int index;

	index = pSorted->size() *
	  (flBias -
	   sqrt(flBiasSqr - tRand.asFloat() * 4 * flBiasLessOne)) /
	   (2 * flBiasLessOne);

	return (PCTGAObj) pSorted->elem(index);
}

class   Genitor : public GeneticAlgorithm
//
//	This Genitor is tailor-made for TSP problems : 
//		no scaling is performed and the best individual is the 
//		one with the smallest objective value (tour length)
//
//      Individuals are stored in an instance of class 'SortedCltn'
//      The compare function provided by class 'TSPind' ensures
//      that the individuals are stored in ascending order of
//      objective values
{
public:

Genitor(sizeType popSize,
	PHENOTYPEFUNC pFunc,
        PCrossover pCrossover,
        float flMutate);

	virtual ~Genitor()
	{
	}

	virtual void generation();
	BOOL run(int);

protected:

        SortedCltn      pop;    // ensure the individuals are sorted

	PHENOTYPEFUNC	pFunc;
	double		minTourLength, maxTourLength, avgTourLength;
	void	debug()  const
	{
		CollectionIterator iter(pop);
		while (iter) {
		   cout << ((PCTGAObj) iter())->objValue() << endl;
                   iter++;
		}
		cout << endl;
        }
};

double cost_function(RCTGAObj obj)
{
	return pTourInfo->cost((RCTSPind) obj);
}

BOOL Genitor::run(int nGen)
{
	int count = nGen;

        do {
           generation();
           if (trials() % 50 == 0)
               resetMonitor();
        } while (count-- > 0);

	return TRUE;
}

void Genitor::generation()
//
//      only one individual is replaced in each generation
//
{
        PTGAObj  pTmpInd;
	double   oldWorst = ((PCTGAObj) pop.elem(pop.size() - 1))->objValue();
	PCTGAObj pMother, pFather;
	sizeType insPoint;

        nTrials++;      // one more trial

        pSSch->reset(pop);

	pFather = pSSch->select();
	pMother = pSSch->select();
        pXover->setMate(pFather, pMother);
        pTmpInd = pXover->create();
        pTmpInd->mutate(flMRate);
        pTmpInd->phenotype(pFunc);
        pTmpInd->fitness() = 1 / pTmpInd->objValue();

	delete pop.removeAt(pop.size() - 1); // deletes the worst individual
        pop.add(pTmpInd);

        pBestInd = (PTGAObj) pop.elem(0);
        pWorstInd = (PTGAObj) pop.elem(pop.size() - 1);
    
	minTourLength = pBestInd->objValue();
	maxTourLength = pWorstInd->objValue();

        avgTourLength = (avgTourLength * pop.size() - oldWorst + pTmpInd->objValue()) / pop.size();

	if (TSPind::printFlag) {
	    cout << TSPind::crossTotal << " : " << minTourLength << ' ' << maxTourLength << ' ' << avgTourLength << endl;
	    TSPind::printFlag = FALSE;
	}

}

Genitor::Genitor( sizeType popSize,
		  PHENOTYPEFUNC pCostFunc,
		  PCrossover pCrossover,
		  float flMutate) :
                  GeneticAlgorithm(),
                  pFunc(pCostFunc)
{
	flMRate=rangeCheck(flMutate,0.0,1.0, DEFAULT_MUTATRATE);
	pXover=pCrossover;
	pSSch = new MyLinearRanking(1.25);
	pScale = NULL;
	nTrials=1;
	nTrialsMonitored=1;
	flBestSoFar=FLT_MIN;
	flSumOnLines=0;
	flSumOffLines=0;

        pPop = NULL;
	pop.ownsElements(owner);
        objStat.reset();        // reset statistics

	PTSPind pTour;
	for (sizeType i=0; i<popSize; i++) {
	     pTour = new TSPind(pTourInfo->cities(), TRUE);
	     pTour->phenotype(pFunc);
	     pTour->fitness() = 1.0 / pTour->objValue();
	     objStat += pTour->objValue();
	     pop.add(pTour);
	}

	pBestInd = (PTGAObj) pop.elem(0);
	pWorstInd = (PTGAObj) pop.elem(pop.size() - 1);
	minTourLength = objStat.min();
        maxTourLength = objStat.max(); 
        avgTourLength = objStat.mean();
	cout << pBestInd->objValue() << " : " << minTourLength << endl;
	cout << pWorstInd->objValue() << " : " << maxTourLength << endl;
}

void	main(int argv, char *argc[])
{
	int  i, j;
	ShortVec ints;
	PTSPind	pChild;
	char filename[100];

        if (argv == 1) {
	   cout << "no tour information supplied" << endl;
           exit(-1);
        }
	
	if (argv > 1) {
	   strcpy(filename, argc[1]);
	   strcat(filename, ".tsp");
	   pTourInfo = new TourInfo(filename);

	   if (argv > 2) {
	      fstream cfgfile(argc[2], ios::in);
	      cfgfile >> nGen;
	      cfgfile >> uPopSize;
	      cfgfile >> flXRate;
	      cfgfile >> flMRate;
	   }
	}
	else
	   exit(-1);

	pTourInfo->canonical();

        cout << "Number of Generations : " << nGen << endl;
        cout << "Population Size : " << uPopSize << endl;
        cout << "Crossover  Rate : " << flXRate << endl;
        cout << "Mutation   Rate : " << flMRate << endl;

	ints.resize(pTourInfo->cities());
	if (ints.capacity() > 10) 
	    TSPind::fraction = 10.0 / ints.capacity();
	else
	    TSPind::fraction = 0.5;

	edgeMap.resize(pTourInfo->cities());

	Genitor		   ga(uPopSize,
			      cost_function,
			      (PCrossover) new OddPtCrossover(1, flXRate),
			      flMRate);

	ga.run(nGen);

	cout << ga.bestInd() << endl << endl << ga.worstInd() << endl << endl;

	cout << "Number of crossover performed : " << TSPind::crossTotal << endl;

	// pTourInfo->printLocations(cout, ints);
	delete pTourInfo;
}

