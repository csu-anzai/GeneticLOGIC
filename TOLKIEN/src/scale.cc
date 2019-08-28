//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include    "scale.h"
#include    "ranksb.h"

static  PCTypeInfo scaleBases[] = { &TObject::infoObj, 0 };
const TypeInfo  ScalingScheme::infoObj("Scaling Scheme", scaleBases);

static  PCTypeInfo sigmascaleBases[] = { &ScalingScheme::infoObj, 0 };
const TypeInfo  SigmaScaling::infoObj("Sigma Scaling", sigmascaleBases);

static  PCTypeInfo linearscaleBases[] = { &ScalingScheme::infoObj, 0 };
const TypeInfo  LinearScaling::infoObj("Linear Scaling", linearscaleBases);

static  PCTypeInfo min2maxscaleBases[] = { &SigmaScaling::infoObj, 0 };
const TypeInfo  Min2MaxScaling::infoObj("Min to Max Scaling", min2maxscaleBases);

static  PCTypeInfo nonnegscaleBases[] = { &NonNegScaling::infoObj, 0 };
const TypeInfo  NonNegScaling::infoObj("Non-Negative Scaling", nonnegscaleBases);

void    ScalingScheme::operator()(RGeneticAlgorithm ga,
                                  RPTGAObj pBest, RPTGAObj pWorst)
//
//      scale by assigning random fitness to individuals
//      this function is useful when the derived operator()
//      scaled all fitnesses to the same value (or zero)
//      this is possible at least for SigmaScaling
//
{
        CollectionIterator popIter(ga.pop().operator RCCollection());
        PTGAObj pObj;
        IntVec ints;
        int i = 0;

        ranksb.random(ints, ga.pop().size(), ga.pop().size());

        // the fitness values are in the set [1 ... ga.pop.size()]
        while ( popIter ) {
               pObj = (PTGAObj) popIter();
               pObj->fitness() = ints[i] + 1;
               if (pObj->fitness() == 1)
                   pWorst = pObj;
               else
                   if (pObj->fitness() == ga.pop().size())
                       pBest = pObj;
               popIter++;
        }
}

void    SigmaScaling::operator()(RGeneticAlgorithm ga,
                                 RPTGAObj pBest, RPTGAObj pWorst)
{
        CollectionIterator popIter(ga.pop().operator RCCollection());
        PTGAObj pObj;

        const double flDiff = ((SampleStatistic &) ga.objValueStat()).mean() -
                 flSFactor * ((SampleStatistic &) ga.objValueStat()).stdDev();

        if ( popIter ) {
             pBest = pWorst = (PTGAObj) popIter();
             ((PTGAObj) popIter())->fitness() =
                ((PTGAObj) popIter())->objValue() - flDiff;
             if (((PTGAObj) popIter())->fitness() < 0)
                 ((PTGAObj) popIter())->fitness() = 0;
             ++popIter;
        }

	while ( popIter ) {
               pObj = (PTGAObj) popIter();
               pObj->fitness() =
                  pObj->objValue() - flDiff;
               if (pObj->fitness() < 0) {
                   pObj->fitness() = 0;
                   pWorst = pObj;
               }
               else
                   if (pObj->fitness() > pBest->fitness())
		       pBest = pObj;
	       else if (pObj->fitness() < pWorst->fitness())
		       pWorst = pObj;
               ++popIter;
	}

        if (pBest->fitness() == pWorst->fitness())
            ScalingScheme::operator()(ga,pBest,pWorst);
}

void    LinearScaling::operator()(RGeneticAlgorithm ga,
                                  RPTGAObj pBestInd, RPTGAObj pWorstInd)
{
        float    flDelta, flA, flB, flAvg;

        ga.pop().findBestandWorst(pBestInd,pWorstInd);

	if (pWorstInd->objValue() >= 0) {
	    flAvg = (pBestInd->objValue() + pWorstInd->objValue()) / 2;
	    flDelta = pBestInd->objValue() - pWorstInd->objValue();
	    flA = flAvg / flDelta;
	    flB = flAvg * ( pBestInd->objValue() - 2 * flAvg) / flDelta;
	}
	else {
	    flDelta = flAvg - pWorstInd->objValue();
	    flA = flAvg / flDelta;
	    flB = pWorstInd->objValue() * -1 * flAvg / flDelta;
	}

        CollectionIterator popIter(ga.pop().operator RCCollection());

	while ( popIter ) {
		((PTGAObj) popIter())->fitness() =
			flA * ((PTGAObj) popIter())->objValue() + flB;
               ++popIter;
	}
}

void NonNegScaling::operator()(RGeneticAlgorithm ga,
                               RPTGAObj pBestInd, RPTGAObj pWorstInd)
{
        PTGAObj pObj;
        double flMin;

        ga.pop().findBestandWorst(pBestInd,pWorstInd);

        if (pWorstInd->objValue() >= 0)
            return; // no need to scale
        else {
            CollectionIterator popIter(ga.pop().operator RCCollection());
            //
            // flMin is slightly larger than fabs(pWorstInd->objValue())
            // ensure the fitness of the worst individual
            // is not zero after scaling
            //
            flMin = fabs(pWorstInd->objValue()) +
                    (pBestInd->objValue() - pWorstInd->objValue()) / ga.pop().size();

            while ( popIter ) {
                    pObj = (PTGAObj) popIter();
                    pObj->fitness() = pObj->objValue() + flMin;
                    ++popIter;
            }
        }
}

void    Min2MaxScaling::operator()(RGeneticAlgorithm ga,
                                   RPTGAObj pBestInd, RPTGAObj pWorstInd)
{
        CollectionIterator popIter(ga.pop().operator RCCollection());
        PTGAObj pObj;

        const double flDiff = ((SampleStatistic &) ga.objValueStat()).mean() +
                 flSFactor * ((SampleStatistic &) ga.objValueStat()).stdDev();

        if (popIter) {
               pBestInd = pWorstInd = pObj = (PTGAObj) popIter();
               pObj->fitness() =
                  flDiff - pObj->objValue();
               if (pObj->fitness() < 0)
                   pObj->fitness() = 0;
               ++popIter;

               while ( popIter ) {
                      pObj = (PTGAObj) popIter();
                      pObj->fitness() =
                         flDiff - pObj->objValue();
                      if (pObj->fitness() < 0) {
                          pObj->fitness() = 0;
                          pWorstInd = pObj;
                      }
                      else if (pObj->fitness() > pBestInd->fitness())
			       pBestInd = pObj;
			else if (pObj->fitness() < pWorstInd->fitness())
				pWorstInd = pObj;
		      ++popIter;
               }
        }

        if (pBestInd->fitness() == pWorstInd->fitness())
            ScalingScheme::operator()(ga,pBestInd,pWorstInd);
}

