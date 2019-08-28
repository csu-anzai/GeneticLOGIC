//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//

#include "xover.h"
#include "trand.h"
#include "bitstr.h"
#include "tritstr.h"
#include "IntVec.h"
#include "chromo.h"

static  PCTypeInfo xoverBases[] = { &TObject::infoObj, 0 };
const TypeInfo  Crossover::infoObj("Crossover", xoverBases);

static  PCTypeInfo uxoverBases[] = { &Crossover::infoObj, 0 };
const TypeInfo  UniformCrossover::infoObj("UniformCrossover", uxoverBases);

static  PCTypeInfo mpxoverBases[] = { &Crossover::infoObj, 0 };
const TypeInfo  MultiPtCrossover::infoObj("MultiCrossover", mpxoverBases);

static  PCTypeInfo ptxoverBases[] = { &Crossover::infoObj, 0 };
const TypeInfo  PtCrossover::infoObj("Point Crossover", ptxoverBases);

static  PCTypeInfo oddxoverBases[] = { &PtCrossover::infoObj, 0 };
const TypeInfo  OddPtCrossover::infoObj("OddPtCrossover", oddxoverBases);

static  PCTypeInfo evenxoverBases[] = { &PtCrossover::infoObj, 0 };
const TypeInfo  EvenPtCrossover::infoObj("EvenPtCrossover", evenxoverBases);

int   Crossover::compare( RCTObject obj ) const
{
	if (isSame(obj))
	    return 0;
	else
	    return -1;
}

PTGAObj UniformCrossover::create()
{
	PCTGAObj pTmp = pFather;
	PTGAObj  pChild;

        pChild = pMother->uniformCrossover(*pFather, flXRate);
	// swap parents
	pFather = pMother;
	pMother = pTmp;

        return pChild;
}

void    UniformCrossover::transform(RTGAObj child)
{
	PCTGAObj pTmp = pFather;
        pMother->uniformCrossover(*pFather, flXRate, child);
	// swap parents
	pFather = pMother;
        pMother = pTmp;
}

MultiPtCrossover::MultiPtCrossover(unsigned uPoints, double flRate)
{
        if (uPoints % 2 == 0)
            pXOver = (PPtCrossover) new EvenPtCrossover(uPoints, flRate);
        else
            pXOver = (PPtCrossover) new OddPtCrossover(uPoints, flRate);
}

unsigned        MultiPtCrossover::xpts(unsigned uNewPts)
{
	unsigned nOldXpts = pXOver->xpts();
	float	 flRate = pXOver->rate();

        if (pXOver->xpts() % 2 == 0) {
                if (uNewPts % 2 == 0)
                    pXOver->xpts(uNewPts);
                else {
                        delete pXOver;
                        pXOver = (PPtCrossover) new OddPtCrossover(uNewPts, flRate);
                }
        }
        else {
                if (uNewPts % 2 != 0)
                    pXOver->xpts(uNewPts);
                else {
                        delete pXOver;
                        pXOver = (PPtCrossover) new EvenPtCrossover(uNewPts, flRate);
                }
        }
        return nOldXpts;
}

PTGAObj EvenPtCrossover::create()
{
	PCTGAObj pTmp = pFather;
	PTGAObj  pChild;

        if (flip())
            pChild = pMother->evenPtCrossover(*pFather, nXpts);
        else
	    pChild = (PTGAObj) pMother->deepCopy();
	// swap parents
	pFather = pMother;
	pMother = pTmp;

        return pChild;
}

void    EvenPtCrossover::transform(RTGAObj child)
{
	PCTGAObj pTmp = pFather;
        if (flip())
            pMother->evenPtCrossover(*pFather, nXpts, child);
        else
            child = *pMother;
	// swap parents
	pFather = pMother;
        pMother = pTmp;
}

PTGAObj OddPtCrossover::create()
{
	PCTGAObj pTmp = pFather;
	PTGAObj	pChild;

        if (flip())
            pChild = pMother->oddPtCrossover(*pFather, nXpts);
        else
	    pChild = (PTGAObj) pMother->deepCopy();
	// swap parents
	pFather = pMother;
	pMother = pTmp;

	return pChild;
}

void    OddPtCrossover::transform(RTGAObj child)
{
	PCTGAObj pTmp = pFather;
        if (flip())
            pMother->oddPtCrossover(*pFather, nXpts, child);
        else
            child = *pMother;
	// swap parents
	pFather = pMother;
        pMother = pTmp;
}


