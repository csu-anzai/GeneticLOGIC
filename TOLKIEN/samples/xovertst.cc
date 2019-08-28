//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      A demo to test crossover on various types of chromosomes
//
#include <time.h>
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "atom.h"
#include "diploid.h"
#include "dpldbin.h"
#include "tclsf.h"
#include "ttclsf.h"
#include "tvalues.h"
#include "xover.h"

_CLASSDEF(IntHaploid)

class   IntHaploid : public Haploid
{
public:

	DECLARE_RTTI()

	IntHaploid() : Haploid()
	{
	}

	IntHaploid(RCIntHaploid src) : Haploid(src)
	{
	}

	IntHaploid(unsigned uLen, int fill)
	{
	     PAtomBase pAtom;
	     genes.capacity(uLen);
	     for (register i = 0; i < uLen; i++) {
		  pAtom = new TInteger(fill);
		  genes.add((PTObject) pAtom);
	     }
	     genes.ownsElements(owner);
	}

	~IntHaploid()
	{
	}

	_shallowCopy(IntHaploid)

	virtual void deepenShallowCopy()
	{
		Haploid::deepenShallowCopy();
	}
};

static  PCTypeInfo inthaploidBases[] = { &Haploid::infoObj, 0 };
const TypeInfo  IntHaploid::infoObj("IntHaploid", inthaploidBases);

void	main(int argc, char *argv[])
{
	int      n = 1, i, indlen=20, j, trials=100, xpt = 2;
	PTGAObj  pFather, pMother, pChild;
	BitString       ones, zeroes;

	if (argc > n)
	    i = atoi(argv[n++]);
	if (argc > n)
	    indlen = atoi(argv[n++]);
	if (argc > n)
	    trials = atoi(argv[n++]);
	if (argc > n)
	    xpt = atoi(argv[n++]);

	ones.resize(indlen);
	zeroes.resize(indlen);
	ones.set();
	zeroes.clear();

	TritString      t1(ones,zeroes), t2(zeroes,zeroes), t3(ones,ones);

	MultiPtCrossover xover(xpt,1.0);

	switch (i) {
		case 2:
			pFather = new BinDiploid(ones,zeroes);
                        pMother = new BinDiploid(zeroes,ones);
			break;
		case 3:
			pFather = new TClassifier(t1,t2);
			pMother = new TClassifier(t2,t1);
			break;
		case 4:
			pFather = new TTClassifier(t1,t2,t3);
                        pMother = new TTClassifier(t3,t1,t2);
			break;
		case 5:
			pFather = new IntHaploid(indlen,1);
			pMother = new IntHaploid(indlen,0);
			break;

		default:
			pFather = new BinHaploid(ones);
			pMother = new BinHaploid(zeroes);
			break;
	}

	xover.setMate(pFather,pMother);
	cout << "Father : " << *pFather << endl;
	cout << "Mother : " << *pMother << endl;
	for (i=0;i<trials;i++) {
		pChild = xover.create();
		cout << "Child  : " << *pChild << endl;
		delete pChild;
	}

	xover.xpts(xover.xpts() + 1);
	xover.setMate(pFather,pMother);
	cout << "Father : " << *pFather << endl;
	cout << "Mother : " << *pMother << endl;
	for (i=0;i<trials;i++) {
		pChild = xover.create();
		cout << "Child  : " << *pChild << endl;
		delete pChild;
	}

	delete pFather;
	delete pMother;
}
