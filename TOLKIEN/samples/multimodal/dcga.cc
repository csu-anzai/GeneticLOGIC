//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      A GA program developed using TOLKIEN to solve
//      some multimodal functions by Mahfoud's deterministic crowding
//
//      Reference :
//
//      Mahfoud, S.W. (1992).  Crowding and Preselection Revisited.
//      In Parallel Problem Solving from Nature, 2.  Springer-Verlag. 1992.
//
//      This GA (or my implementation of it) works only for
//      function optimizations such that the fittest individual
//      has the largest objective value.
//      ( ref. DeterminsiticCrowding::generation() )
//
//
#include <iostream.h>
#include <fstream.h>
#include "ga.h"
#include "popsmpl.h"
#include "slctrw.h"
#include "slcttour.h"
#include "cfgfile.h"
#include "urndbits.h"
#include "mmfuncs.h"
#include "ranksb.h"

double  dummy(RCTGAObj ind)
{
        return tRand();
}

//
//      Initialization of global variables
//
TInteger nGen = 500, uPopSize = 50, uTest = 1;
TDouble    flMRate = 0.001;
TDouble    flXRate = 1.0;
PScalingScheme pScale = (PScalingScheme) new SigmaScaling(1.5);
PHENOTYPEFUNC	pMMFunc = evenpeaks;
PSelectionScheme pSch = (PSelectionScheme) new TournSelect(2);
PCrossover pXvr = (PCrossover) new EvenPtCrossover(2,flXRate);

class	DeterministicCrowding : public GeneticAlgorithm
{
public:
	DeterministicCrowding(unsigned,
			      RCollection,
			      PHENOTYPEFUNC pFunc,
			      PSelectionScheme,
			      PCrossover,
			      float,
			      PScalingScheme = NULL);

	virtual void    generation();

protected:
	IntVec  mates;  // indices to individuals
	int	func;
	TournSelect selector;
};

DeterministicCrowding::DeterministicCrowding(unsigned i,
					     RCollection cltn,
					     PHENOTYPEFUNC pFunc,
				   PSelectionScheme pSelectScheme,
				   PCrossover pCrossover,
				   float flMutate,
				   PScalingScheme pScaleScheme) :
		       GeneticAlgorithm(), func(i)
{
	flMRate=rangeCheck(flMutate,0.0,1.0, DEFAULT_MUTATRATE);
	pXover=pCrossover;
	pSSch=pSelectScheme;
	pScale=pScaleScheme;
	reEvaluate=TRUE;
	nTrials=1;
	nTrialsMonitored=0;
	flBestSoFar=FLT_MIN;
	flSumOnLines=0;
	flSumOffLines=0;
	pPop = new SimplePopulation(pFunc);
	pPop->addAll( cltn );

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

	statistics();
}

void DeterministicCrowding::generation()
//
//	arrange the parents in a random manner into n/2 pairs
//
//      for each pair of individuals do
//              generate two offsprings,
//              the best two individuals among
//              the two parents and the two offsprings
//              are allowed to survive
//      enddo
//
{
	register sizeType i, j;
	PTGAObj  pTmpInd, pChild1, pChild2;
	PCTGAObj pParent1, pParent2;
	double   diff1, diff2;
	int tmp;

	nTrials++;      // one more trial

	mates.resize(pPop->size());
	// create a random sequence
	for (i = 0; i < pPop->size(); i++)
	     mates[i] = i;
	for (i = pPop->size() - 1; i > 0; i--) {
		j = i * tRand();
		tmp = mates[j];
		mates[j] = mates[i];
		mates[i] = tmp;
	}

	for (i=0; i < pPop->size(); i+= 2) {
	     pParent1 = pPop->elem(mates[i]);
	     pParent2 = pPop->elem(mates[i+1]);
	     if (pParent1 == (PTGAObj) NOOBJECT)
		 exit(-1);
	     if (pParent2 == (PTGAObj) NOOBJECT)
		 exit(-1);
	     pXover->setMate(pParent1, pParent2);
	     pChild1 = pXover->create();
	     pChild2 = pXover->create();
	     pChild1->mutate(flMRate);
	     pChild2->mutate(flMRate);
	     pChild1->phenotype(pPop->phenotypeFunc());
	     pChild2->phenotype(pPop->phenotypeFunc());

	     diff1 = abs(pParent1->objValue() - pChild1->objValue()) +
		     abs(pParent2->objValue() - pChild2->objValue());
	     diff2 = abs(pParent1->objValue() - pChild2->objValue()) +
		     abs(pParent2->objValue() - pChild1->objValue());

	     if (diff2 > diff1) {
		// parent1 <-> child1, parent2 <-> child2
		if (pParent1->objValue() < pChild1->objValue())
		    pPop->setAt(mates[i], pChild1, FALSE);
		else
		    delete pChild1;
		if (pParent2->objValue() < pChild2->objValue())
		    pPop->setAt(mates[i+1], pChild2, FALSE);
		else
		    delete pChild2;
	     }
	     else {
		// parent1 <-> child2, parent2 <-> child1
		if (pParent2->objValue() < pChild1->objValue())
		    pPop->setAt(mates[i+1], pChild1, FALSE);
		else
		    delete pChild1;
		if (pParent1->objValue() < pChild2->objValue())
		    pPop->setAt(mates[i], pChild2, FALSE);
		else
		    delete pChild2;
	     }
	}

	statistics();
}

void main(int argv, char* argc[])
{
	int optfunc = 1, chromlen;
	sizeType nI;
	TOArray cltn;
	TLong seed1 = 0, seed2 = (unsigned long) time(NULL);
	MLCG    myMLCG(0,(unsigned long) time(NULL));

	if (argv < 2) {
	    cout << "usage : " << argc[0] << " function [config file]" << endl;
            cout << "where function is an integer in [1,7]" << endl;
	    cout << "    1 : two peaks" << endl;
	    cout << "    2 : Himmelblau's function" << endl;
	    cout << "    3 : even peaks" << endl;
	    cout << "    4 : uneven peaks" << endl;
	    cout << "    5 : uneven decreasing peaks" << endl;
	    cout << "    6 : two peaks trap" << endl;
	    cout << "    7 : uneven cos" << endl;
            return;
        }

        optfunc = atoi(argc[1]);

        if (argv > 2) {
              fstream cfgfile(argc[2], ios::in);
	      ConfigFile cf(cfgfile);
	      cf.read("generation", nGen);
	      cf.read("population size", uPopSize);
	      cf.read("crossover rate", flXRate);
	      cf.read("mutation rate", flMRate);
	      cf.read("seed1", seed1);
	      cf.read("seed2", seed2);
              myMLCG.reseed(seed1,seed2);
        }

	cout << "generation " << nGen << endl;
	cout << "population size " << uPopSize << endl;
	cout << "crossover rate " << flXRate << endl;
	cout << "mutation rate " << flMRate << endl;

	switch(optfunc) {
		case 2 :
		pScale = new SigmaScaling(2);
		pMMFunc = hmb;
		chromlen = 40;
		break;
		case 3 :
		pScale = NULL;
		pMMFunc = evenpeaks;
		chromlen = 30;
		break;
		case 4 :
		pScale = NULL;
		pMMFunc = unevenpeaks;
		chromlen = 30;
		break;
		case 5 :
		pScale = NULL;
		pMMFunc = unevendecpeaks;
		chromlen = 30;
		break;
		case 6 :
		pScale = NULL;
		pMMFunc = twopeaktrap;
		chromlen = 20;
		break;
		case 7 :
		pScale = NULL;
		pMMFunc = unevencos;
		chromlen = 30;
		break;

                case 0 :
		pScale = NULL;
                pMMFunc = dummy;
                chromlen = 10;
                break;

                default:
		pScale = NULL;
		pMMFunc = twopeaks;
		chromlen = 20;
		break;
	}
	
	PUniformRandomBits pBits;

        switch(optfunc) {
                case 2:
                pBits = new UniformRandomBits(uPopSize,40);
                for (nI=0; nI < uPopSize; nI++)
                     cltn.add((PTObject) new HmbInd(pBits->elem(nI)));
                break;

                case 3:
                case 4:
                case 5:
		case 7:
		pBits = new UniformRandomBits(uPopSize,30);
                cout << *pBits << endl;
                for (nI=0; nI < uPopSize; nI++)
                     cltn.add((PTObject) new SinInd(pBits->elem(nI)));
                cout << cltn << endl;
                break;

                default:
		pBits = new UniformRandomBits(uPopSize,chromlen);
                for (nI=0; nI < uPopSize; nI++)
                     cltn.add((PTObject) new BinHaploid(pBits->elem(nI)));
                break;
        }
	delete pBits;

        DeterministicCrowding ga(optfunc,
                                 cltn,
				 pMMFunc,
                   		 pSch, 
				 pXvr,
                                 flMRate,pScale);

	cout << "Generation " << ga.trials() << endl;
	cout << ga.pop();
	cout << "Best Ind" << endl << "-----------" << endl;
	cout << ga.bestInd() << endl;
	cout << "Worst Ind" << endl << "-----------" << endl;
	cout << ga.worstInd() << endl;

	while (ga.trials() < nGen) {
	     ga.generation();
             if (ga.trials() % 50 == 0) {
                 cout << ga.pop() << endl;
                 ga.resetMonitor();
             }

	     cout << "Best Ind  : " << ga.bestInd() << " (" << ga.bestInd().objValue() << ')' << endl;
	     cout << "Worst Ind : " << ga.worstInd() << " (" << ga.worstInd().objValue() << ')' << endl;
	}

	TOArray	final;
	final.addAll(ga.pop().operator RCCollection());
	final.sort(ascFitness);
	CollectionIterator iter(final);
        cout << endl << "Final population sorted in ascending order of fitness" << endl;
	while ( iter )  {
	    cout << *iter() << ' ' << ((PTGAObj) iter())->objValue() << endl;
	    iter++;
	}
}

