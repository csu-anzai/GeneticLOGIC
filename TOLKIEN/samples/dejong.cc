//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      A GA program developed using TOLKIEN to solve
//      some De Jong's functions
//
#include <time.h>
#include <math.h>
#include <iostream.h>
#include "Normal.h"
#include "ga.h"
#include "popcrd.h"
#include "popsmpl.h"
#include "dpldbin.h"
#include "slctrw.h"
#include "schemata.h"
#include "scale.h"

#if !defined(PI)
#define PI      ((double) 3.141592654)
#endif

#define _5PI     (5 * PI)

unsigned nGen = 500 , uPopSize = 50, uTest = 1;
float    flMRate = 0.003;
float    flXRate = 1;
float    flGap   = 0.5;
Normal  gauss_0_1(0,1,&mlcg);

_CLASSDEF(Parameters)
class   Parameters : public BinHaploid
{
public:
        Parameters(unsigned parms, unsigned parmLen, double flOff) : 
            BinHaploid(parms * parmLen, RANDOMIZE), 
            uParmLen(parmLen), uParms(parms), flOffSet(flOff)
	{
		flPrecs = (flOff * 2) / (pow(2, parmLen) - 1);
	}

	_shallowCopy(Parameters)

	void    printOn(ostream & out) const
	{
		int i, j, k;
		for (i = 0, j = uParms - 1, k = 0; i < j; i++, k += uParmLen) 
                     out << asLong(k,uParmLen) * flPrecs - flOffSet << ","; 
                out << asLong(k,uParmLen) * flPrecs - flOffSet; 
	}

	double	var(int i) const
	{
		return asLong(uParmLen * i, uParmLen) * flPrecs - flOffSet;
	}

protected :
	double flPrecs;
	double flOffSet;
	unsigned uParms, uParmLen;
};

//
//	constants used by F5
//
int a[2][25] ={
	{
		-32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
		-32, -16, 0, 16, 32, -32, -16, 0, 16, 32	},
	{
		-32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
                0,0,0,0,0, 
		16, 16, 16, 16, 16, 32, 32, 32, 32, 32	}
};

//
//      F1 : f1(x1, x2, x3) = x1 ^ 2 + x2 ^ 2 + x3 ^ 2
//
//              -5.12 <= xi <= 5.12
//
//      use 10 bits for each parameter
//
//      xi = (10.24 / 1023) * binary rep of parameter
//
//
double   djF1(RCTGAObj ind)
{
    double   flX1, flX2, flX3;

    flX1 =  ((RCParameters) ind).var(0);
    flX2 =  ((RCParameters) ind).var(1);
    flX3 =  ((RCParameters) ind).var(2);

    return flX1 * flX1 + flX2 * flX2 + flX3 * flX3;
}

//                              2                 2
//      F2 : f2(x1, x2) = 100(x1  - x2) + (1 - x1)
//
//              -2.048 <= xi <= 2.048
//
//      use 12 bits for each parameter
//
//      xi = (4.096 / 4095) * binary rep of parameter
//
//
double   djF2(RCTGAObj ind)
{
    double   flX1, flX2;

    flX1 =  ((RCParameters) ind).var(0);
    flX2 =  ((RCParameters) ind).var(1);

    return 100 * pow(flX1 * flX1 - flX2, 2.0) + pow(1 - flX1, 2.0);
}

//
//      F3 : f3(x1, x2, x3, x4, x5) =
//              30 + floor(x1) + floor(x2) + floor(x3) + floor(x4) + floor(x5)
//
//              -5.12 <= xi <= 5.12
//
//      use 10 bits for each parameter
//
//      xi = (10.24 / 1023) * binary rep of parameter
//
double   djF3(RCTGAObj ind)
{
    double      flValues[5];
    int         nValues[5], i, j, nReturn;

    for (nReturn = 0, i = 0, j = 0; i < 5; i++, j += 10) {
        flValues[i] =  ((RCParameters) ind).var(i);
        nValues[i] = flValues[i];
        if (nValues[i] > flValues[i])
            nValues[i] -= 1;
	nReturn += nValues[i];
    }
    return 30 + nReturn;
}

//                             30
//                           ------
//      F4 : f4(xi) =        \       [ i * (xi)**4  + Gauss(0,1) ]
//                           /
//                           ------
//                             1
//
//              -1.28 <= xi <= 1.28
//
//      use 8 bits for each parameter
//
//      xi = (2.56 / 255) * binary rep of parameter
//
double   djF4(RCTGAObj ind)
{
    double   flTemp, flTotal = 0;

    for(int i=0, j=0; i < 30; i++, j+=8) {
            flTemp =  ((RCParameters) ind).var(j);
	    flTemp = i * pow(flTemp, 4.0);
            flTemp += gauss_0_1();
            flTotal += flTemp;
    }

    return flTemp;
}

//
//      f5(x1,x2) [Shekel's foxholes] =
//
//
//                             25
//                           ------                1
//             0.002  +      \       ------------------------------
//                           /                     2
//                           ------               ----              6
//                             j=1        j  +    \     (x   - a   )
//                                                /___    i     ij
//                                                i=1
//
//              -65.536 <= xi <= 65.536
//
//      use 16 bits for each parameter
//
//      xi = (131.072 / 65535) * binary rep of parameter
//
//
double   djF5(RCTGAObj ind)
{
	register int i, j;
	double ans = 0;
	double fj; 
        double x[2];

        x[0] = ((RCParameters) ind).var(0);
        x[1] = ((RCParameters) ind).var(1);

	for (j = 0; j < 25; j++)
	{
		fj = j + 1;
		for (i = 0; i < 2; i++)
		{
                     fj += pow(x[i] - a[i][j], 6.0);
		}
		ans += 1.0 / fj;
	}
        return(0.002 + ans);
}



void    runTest(PPopulation pPop, RCollection outpop, PScalingScheme pScale = NULL)
{
	GeneticAlgorithm   ga(pPop,
                              (PSelectionScheme) new RW_Select(),
			      (PCrossover) new MultiPtCrossover(1, flXRate),
			      flMRate,
			      flGap,
			      pScale);

	cout << "Generation " << ga.trials() << endl;
	cout << ga.pop();
	cout << "Best Ind" << endl << "-----------" << endl;
	cout << ga.bestInd() << endl;
	cout << "Worst Ind" << endl << "-----------" << endl;
	cout << ga.worstInd() << endl;

	while (ga.trials() < nGen) {
	     ga.generation();
	     cout << "Generation " << ga.trials() << endl;
             if (ga.trials() % 50 == 0) {
                 cout << ga.pop() << endl;
		 ga.resetMonitor();
             }

             cout << "Best Ind   : ";
             cout << ((RCBinIndividual) ga.bestInd()).operator RCBitString();
             cout << " (" << ga.bestInd().objValue() << ')' << endl;

             cout << "Worst Ind  : ";
             cout << ((RCBinIndividual) ga.worstInd()).operator RCBitString();
             cout << " (" << ga.worstInd().objValue() << ')' << endl;
	}

	outpop.addAll(ga.pop());
}

void djF1test()
{
	PPopulation pPop;
	TOArray pop;
	pop.ownsElements(owner);

	pPop = new CrowdingPopulation(djF1);
	
	for (register nI=0; nI < uPopSize; nI++)
	     pPop->add(new Parameters(3, 10, 5.12));

	((PCrowdingPopulation) pPop)->crowdingSubPop(pPop->size() / 10.0);

	runTest(pPop, pop);

	SchemataStatistics st;

        // count the total number of each of the following
        // four schemata in the final population
        //
	st.addSchema("1############################1");
	st.addSchema("1#############################");
	st.addSchema("0#############################");
	st.addSchema("##############################");

	CollectionIterator iter(pop);
	while (iter) {
	      ((PBinHaploid) iter())->BinHaploid::printOn(cout);
	      cout << endl;
	      iter++;
	}

	st(pop);
	cout << endl;
	for (nI = 0; nI < 4; nI++)
	     cout << st.getSchema(nI) << " : " << st.getCount(nI) << endl;

	cout << endl;
}

void djF2test()
{
	PPopulation pPop;
	pPop = new CrowdingPopulation(djF2);
        TOArray pop;
	pop.ownsElements(owner);

	for (register nI=0; nI < uPopSize; nI++)
	     pPop->add(new Parameters(2, 12, 2.048));

	((PCrowdingPopulation) pPop)->crowdingSubPop(pPop->size() / 10.0);

	runTest(pPop, pop);
}

void djF3test()
{
	PPopulation pPop;
	pPop = new CrowdingPopulation(djF3);
        TOArray pop;
	pop.ownsElements(owner);

	for (register nI=0; nI < uPopSize; nI++)
	     pPop->add(new Parameters(3, 10, 5.12));

	((PCrowdingPopulation) pPop)->crowdingSubPop(pPop->size() / 10.0);

	runTest(pPop,pop);
}

void djF4test()
{
	PPopulation pPop;
	pPop = new CrowdingPopulation(djF4);
	TOArray pop;
	pop.ownsElements(owner);

	for (register nI=0; nI < uPopSize; nI++)
	     pPop->add(new Parameters(30, 8, 1.28));

	((PCrowdingPopulation) pPop)->crowdingSubPop(pPop->size() / 10.0);

	runTest(pPop, pop , new NonNegScaling());
}

void djF5test()
{
	PPopulation pPop;
        pPop = new CrowdingPopulation(djF5);
	TOArray pop;
	pop.ownsElements(owner);

	for (register nI=0; nI < uPopSize; nI++)
	     pPop->add(new Parameters(2, 16, 65.536));

	((PCrowdingPopulation) pPop)->crowdingSubPop(pPop->size() / 10.0);

        runTest(pPop, pop);
}

void    main(int argv, char *argc[])
{
	if (argv > 1)
	    uTest = abs(atoi(argc[1]));

	if (argv > 2)
	    nGen = abs(atoi(argc[2]));

	if (argv > 3)
	    uPopSize = atoi(argc[3]);

	if (argv > 4)
	    flXRate = atof(argc[4]);

	if (argv > 5)
	    flMRate = atof(argc[5]);

	if (argv > 6)
	    flGap = atof(argc[6]);

	switch (uTest) {
		case 2:
		    djF2test();
		    break;
		case 3:
		    djF3test();
		    break;
		case 4:
		    djF4test();
		    break;
		case 5:
                    djF5test();
                    break;
		case 6:
		break;
                case 7:
                break;
                default:
                    djF1test();
        }
}
