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
//
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "gatsp.h"
#include "scale.h"
#include "slctrw.h"
#include "tsp.h"
#include "tour.h"

PScalingScheme	pScale = NULL;
unsigned nGen = 100 , uPopSize = 200;
float    flMRate = 0.1;
float    flXRate = 1;
float    flGap   = 0.7;

TourInfo *pTourInfo;

fstream msgout;
fstream statfile;
fstream	profile;
fstream indfile;
fstream bestfile;
fstream bestcostfile;
fstream	bestinfile;

#define	OSTREAM statfile

void	main(int argv, char *argc[])
{
	int  i, j;
	int  indtoRead = 0;
	ShortVec ints;
	PTSPind	pChild;
	int	retries = 0;

        if (argv == 1) {
           cout << "No tour information supplied" << endl;
           cout << "Command syntax : " << argc[0] << " TOUR [CONFIG_FILE_NAME]" << endl;
           cout << "For example : " << argc[0] << " att48 config.fil" << endl;
           exit(-1);
        }

	char filename[100];

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
	      cfgfile >> flGap;
	   }
	}
	else
	   exit(-1);

	msgout.open("tour.out", ios::out);
	statfile.open("stat.out", ios::out);
	bestfile.open("best.out", ios::out);
	bestcostfile.open("bestcost.out", ios::app);
	profile.open("profile.out",ios::out);

	pTourInfo->canonical();

	OSTREAM << "Number of Generations : " << nGen << endl;
	OSTREAM << "Population Size : " << uPopSize << endl;
	OSTREAM << "Crossover  Rate : " << flXRate << endl;
	OSTREAM << "Mutation   Rate : " << flMRate << endl;
	OSTREAM << "Generation Gap  : " << flGap << endl;

	PPOP_GA pPop = new POP_GA(cost_function);

	ints.resize(pTourInfo->cities());

        for (i=0; i<uPopSize; i++)
	     pPop->add((PTGAObj) new TSPind(pTourInfo->cities(), TRUE));
        for (i=0; i<uPopSize; i++)
             pPop->elem(i)->fitness() = 1.0 / pPop->elem(i)->objValue();

	edgeMap.resize(pTourInfo->cities());

	TSP_GA		   ga(pPop,
			      (PSelectionScheme) new RW_Select(),
			      (PCrossover) new OddPtCrossover(1, flXRate),
			      flMRate,
			      flGap,
			      pScale);

	i = 5;
	while (i > 0)
	     if (ga.run(nGen) == FALSE)
		 i--;
	     else
		 i = 5;

	OSTREAM << ga.bestInd() << endl << endl << ga.worstInd() << endl << endl;

	OSTREAM << "Number of crossover performed : " << TSPind::crossTotal << endl;

	ints = ((RCTSPind) ga.bestInd());
	for (i = 0; i < ints.capacity(); i++)
	    bestfile << ints[i] << endl;
	bestcostfile << ga.bestInd().objValue() << endl;

	pTourInfo->printLocations(msgout, ints);
	delete pTourInfo;
}

