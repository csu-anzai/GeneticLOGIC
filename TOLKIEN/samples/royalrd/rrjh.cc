//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//
//      This program trys to optimize Holland's "Royal Road Functions"
//      posted in GA-List v7n22.
//
//      Reference :
//      Forrest, S.; and Mitchell, M.  (1992).
//      Relative Building-Block Fitness and the Building-Block Hypothesis
//      in Foundations of Genetic Algorithms 2, Morgan Kaufmann.
//
#include <iostream.h>
#include <fstream.h>
#include "schemata.h"
#include "trand.h"
#include "ga.h"
#include "hpldbin.h"
#include "popelite.h"
#include "popcrd.h"
#include "slctsus.h"
#include "urndbits.h"
#include "IntVec.h"

const   int     strLen = 240;
const   int     defLen = 8;             // length of each target schema
const   int     intervene = 7;          // the number of intervening bits
                                        // of each target schema

const   int     incr = defLen + intervene;

const   int     k = 4;                  // the number of 1st level
					// target schemata equals 2 ** 4

const	int     twoPwrK = pow(2.0, k);
const   double  v = 0.02;
const   double  u = 0.3;
const   int     u_star = 1;
const   int     m_star = 4;

class   RRSchemataStatistics : public SchemataStatistics
{
public :
	RRSchemataStatistics() : SchemataStatistics()
	{
                zeros.resize(strLen);
		zeros.clear();
	}
	virtual int             matchingBits(unsigned index,
					     const BitString & src) const;
protected :
	BitString zeros;
        // given a schema and a bitstring, e.g. 1*** and 1011
        // the matching bit procedure first create
        // a bit string by filling the don't cares by zeroes
        // and then perform a bitwise and.
        // e.g.
        // 1*** => 1000, 1000 & 1011 => 1000
        // the number of ones remain is the bits matched
};

int     RRSchemataStatistics::matchingBits(unsigned index,
					   const BitString & src) const
{
	BitString       bits =
		((PCTritString) schemata.at(index))->filter(zeros);
        BitString       cmp = bits & src;
	return cmp.count();
}

RRSchemataStatistics st;

void    genTemplates(RRSchemataStatistics & st)
//
//      generate all the schemata
//
{
	char strBuf[strLen + 1];
	char ones[defLen + 1];
	int i = tRand(0, strLen - twoPwrK * defLen - (twoPwrK - 1) * intervene);
	int j,m,n,h;
	char *p;
	TritString tstr;
	BitString zeros, tbits;
	int sp[100];

        zeros.resize(strLen);
	zeros.clear();
	memset(ones, '1', defLen);	// define the basic block
	memset(strBuf,0, strLen + 1);

	// calculate the positions of the 1st level targets
	for (n = 0, j = twoPwrK - 1; j >= 0; j--,n++,i+=incr)
	    sp[n] = i;

	for (n=0; n <= k; n++) { // for all levels
	     m = pow(2.0,n); // the number of 1st level schemata used
	     i = 0;
	     for (h = pow(2.0,k-n) - 1; h >= 0; h--) {
		memset(strBuf, '#', strLen);
		for (j=0;j<m;j++,i++) { // construct a schema
                     p = strBuf + sp[i];
		     memset(p, '1', defLen);
		}
		st.addSchema(strBuf);
	     }
	}
/*
	for (i=0;i<twoPwrK;i++)
	     cout << sp[i] << ' ';
	cout << endl;

	cout << (RCTOArray) st << endl;
*/
}

double  RRJH(RCTGAObj ind)
{
        double retval = 0;
        register i,n,m,h;
        int  matched;   // matched alleles
        double bonus;
	BitString bits = (RCBinHaploid) ind;

        // calculate PART(i)
        // m(i) equals the value returns by matchingBits
        //
        for (i=0; i < twoPwrK; i++) {
             // count how many target schemata contained
             // by this chromosome
             matched = st.matchingBits(i, bits);
             if (matched <= m_star)
                 retval += v * matched;
             else
                 if (matched < defLen)
                     retval += v * (m_star - matched);
        }

        // calculate BONUS(j)
        // BONUS(j) = u_star + (n(j) - 1) * u , j > 0
        //          = 0, j = 0
        // where j is the number of targets found at level j
        for (i=0,n=0; n <= k; n++) { // for all levels
             bonus = 0;
             for (h = pow(2.0,k-n) - 1; h >= 0; h--,i++)
                  // for all schemata in this level
                  if (st.getSchema(i).matches(bits))
                      bonus += 1;
             if (bonus > 0)
                 bonus = u_star + u * (bonus - 1);
             retval += bonus;
	}
        return retval;
}

unsigned nGen = 10000 , uPopSize = 512, uTest = 1, nXpts = 5;
float    flMRate = 0.003;
float    flXRate = 1.0;
float    flGap   = 0.5;

int main(int argv, char* argc[])
{

	unsigned  childrenPerGen = uPopSize * flGap;
	unsigned  evals = uPopSize;
        unsigned  nI, nJ;
        fstream   statfile("stat.out", ios::out);

	if (argv > 1)
            nGen = abs(atoi(argc[1]));

	if (argv > 2)
            uPopSize = atoi(argc[2]);

	if (argv > 3)
	    nXpts = atoi(argc[3]);

        if (argv > 4)
            flXRate = atof(argc[4]);

	if (argv > 5)
            flMRate = atof(argc[5]);

	if (argv > 6)
	    flGap = atof(argc[6]);

	genTemplates(st);

	PPopulation pPop;
	pPop = new CrowdingPopulation(RRJH);
	((PCrowdingPopulation) pPop)->crowdingSubPop(uPopSize * 0.01);

	UniformRandomBits *pBits = new UniformRandomBits(uPopSize, strLen);
        for (nI=0; nI < uPopSize; nI++)
	     pPop->add(new BinHaploid((*pBits)[nI]));
	delete pBits;

        IntVec  counts(pPop->size(), 0);
        // tally the schemata in the original population
        for (nI = 0; nI < st.numSchemata(); nI++)
            for (nJ = 0; nJ < pPop->size(); nJ++)
                if (st.getSchema(nI).
                    matches(((PCBinHaploid) pPop->elem(nJ))->operator RCBitString()))
                    counts[nI]++;

        for (nI = 0; nI < st.numSchemata(); nI++)
            statfile << st.getSchema(nI) << " : " << counts[nI] << endl;
        statfile << "---------------------------------" << endl;

	GeneticAlgorithm   ga(pPop,
			      (PSelectionScheme) new SUS_Select(),
			      (PCrossover) new MultiPtCrossover(nXpts, flXRate),
			      flMRate,
			      flGap,
			      NULL);

	cout << "Generation " << ga.trials() << endl;
	cout << ga.pop();
	cout << "Best Ind" << endl << "-----------" << endl;
	cout << ga.bestInd() << endl;
	cout << "Worst Ind" << endl << "-----------" << endl;
	cout << ga.worstInd() << endl;

	while (ga.trials() < nGen) {
	     ga.generation();
	     evals += childrenPerGen;
	     // cout << "Generation " << ga.trials() << endl;
             // cout << ga.pop() << endl;
             cout << "Evaluations " << evals << endl;
             cout << "Best Ind  : " << ga.bestInd() << " (" << ga.bestInd().objValue() << ')' << endl;
             cout << "Worst Ind : " << ga.worstInd() << " (" << ga.worstInd().objValue() << ')' << endl;
	     cout << "----------------------" << endl;
	}

        statfile << ga.bestInd() << endl;
	for (int i = 0; i < st.numSchemata(); i++) {
             statfile << st.getSchema(i);
             if (st.getSchema(i).matches(((RCBinHaploid) ga.bestInd()).operator RCBitString()))
                 statfile << " matched" << endl;
             else
                 statfile << " not matched" << endl;
        }
}
