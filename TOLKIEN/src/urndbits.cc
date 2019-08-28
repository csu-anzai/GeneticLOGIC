//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "urndbits.h"
#include "trand.h"

UniformRandomBits::UniformRandomBits(sizeType size, int strlen) : popSize(size)
{
	PBitString pBits;

	pBitStrings = (PBitString *) new PBitString[popSize];
	for (register i = popSize - 1; i >= 0; i--) {
	     pBits = new BitString();
	     pBits->resize(strlen);
	     pBitStrings[i] = pBits;
	}

	randomize();
}

UniformRandomBits::~UniformRandomBits()
{
	for (register i = popSize - 1; i >= 0; i--)
	     delete pBitStrings[i];
	delete pBitStrings;
}

void	UniformRandomBits::randomize()
{
	int val;
	sizeType i;
	int j,k,p,r;

	for (i = 0, j = popSize / 2.0; i < j; i++)
	     pBitStrings[i]->set();
	for (; i < popSize; i++)
	     pBitStrings[i]->clear();

	for (k = 0, p = pBitStrings[0]->length() - 1, r = popSize - 1; k <= p; k++)
	     for (i = 0; i < popSize; i++) {
		  j = tRand(i,r);
		  val = pBitStrings[j]->test(k);
		  pBitStrings[j]->assign(k,
					 pBitStrings[i]->test(k));
		  pBitStrings[i]->assign(k, val);
	     }
}

ostream & operator << ( ostream & out, RCUniformRandomBits bitstrs)
{
	for (register i = 0; i < bitstrs.popSize; i++)
	     out << *bitstrs.pBitStrings[i] << endl;

	return out;
}
