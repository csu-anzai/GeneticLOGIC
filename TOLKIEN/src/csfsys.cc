//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "csfsys.h"
#include "tclsf.h"
#include "ttclsf.h"
#include "message.h"
#include "assoc.h"
#include "trand.h"
#include "dict.h"
#include "csfmatch.h"
#include "IntVec.h"
#include "slctrwwr.h"

Normal  noise(0.0, 0.075, &mlcg);

int  decEBid(const void *pMem1, const void *pMem2)
{
	//
	// sort elements in descending order of ebid values
	//
	if ((*((PClassifier *) pMem2))->ebid() >
	    (*((PClassifier *) pMem1))->ebid())
	return 1;
	else if ((*((PClassifier *) pMem2))->ebid() <
		 (*((PClassifier *) pMem1))->ebid())
	     return -1;
	else
	     return 0;
}

int  ascEBid(const void *pMem1, const void *pMem2)
{
	//
	// sort elements in ascending order of ebid values
	//
	if ((*((PClassifier *) pMem1))->ebid() >
	    (*((PClassifier *) pMem2))->ebid())
	return 1;
	else if ((*((PClassifier *) pMem1))->ebid() <
		 (*((PClassifier *) pMem2))->ebid())
	     return -1;
	else
	     return 0;
}

sizeType ClassifierSystem::worstofn() const
//
//      select worst individual from random subpopulation of size n
//      this function assumes the size of the classifier list is nonzero
//
//      error if the value return is greater than or equal to
//      the size of the list (may be no classifier is available)
//
//      this function assumes that the number of classifiers
//      available for replacement is greater than n
//
{
        sizeType  i, j = rangeCheck(uCrowdingSubPop, 1, csfList.size(), 1), k,
                  uWorst, uCandidate;
	float     flWorstStrength, flVal;

        uWorst = csfList.size();    // set to invalid value first

        if (j > 1) {
           for (k=0; k < csfList.size(); k++) {
                // find an initial candidate
                if (((PCClassifier) csfList.elem(k))->canReplace()) {
                    flWorstStrength = ((PCTGAObj) csfList.elem(k))->fitness();
		    uWorst = k;
                    break;
                }
           }

           if (uWorst == csfList.size())
              // no candidate left
              return uWorst;

           for (k = j - 1; k >= 0; k--) {
                i = tRand(1, j);
                do {
                    i--;
                    uCandidate = tRand(0, csfList.size() - 1);
                } while ((! ((PCClassifier) csfList.elem(uCandidate))->canReplace())
                          && (i > 0));
                if (((PCClassifier) csfList.elem(uCandidate))->canReplace()) {
                    flVal = ((PCTGAObj) csfList.elem(uCandidate))->fitness();
                    if (flWorstStrength > flVal) {
                        uWorst = uCandidate;
                        flWorstStrength = flVal;
                    }
                }
	   }
	}
        return uWorst;
}

void    ClassifierSystem::replaceBy(PCClassifier pChild)
{
//
//      replacement using modified De Jong crowding
//      this function assumes that the number of classifiers
//      available for replacement is greater than uCrowdingFactor
//

	int             nMatch, nMatchMax = -1;
	sizeType        uPopMember, uMostSimilar = 0;

	for (register sizeType j = uCrowdingFactor - 1; j >=0 ; j--) {
                uPopMember = worstofn();
		nMatch =
		  pChild->matchCount(* (PClassifier) csfList.elem(uPopMember));
		if (nMatch > nMatchMax) {
		    nMatchMax = nMatch;
		    uMostSimilar = uPopMember;
		}
	}

	csfList.setAt(uMostSimilar, (PTObject) pChild);
}

ClassifierSystem::ClassifierSystem(PMsgList	    pList,
				   PSelectionScheme pSelect,
				   PCrossover       pCross) :
	     nTrials(0),
	     pSSch(pSelect),
	     pXover(pCross),
             flBidRt(0.1),
             flBidSp(1),
             flEBidSp(1),
             flBidTaxRate(0.01),
             flLifeTaxRate(0.005),
	     flMRate(DEFAULT_MUTATRATE),
             flCsfStrMax(CSF_STRENGTH_MAX),
             flCsfStrMin(CSF_STRENGTH_MIN),
             nMatches(0),
             supportFlag(TRUE),
             uCrowdingFactor(3),
             uCrowdingSubPop(3),
	     oneMsgPCsf(FALSE),
             bbFlag(TRUE),
             flPrdTaxMax(PRD_TAX_MAX),
             flPrdTaxPow(1.5),
             nNewCsfStr(0)
{
             csfList.ownsElements(owner);
	     pCurMsgList = pList;
	     pOldMsgList = (PMsgList) pList->deepCopy();
	     pNewMsgList = (PMsgList) pList->deepCopy();
}

ClassifierSystem::~ClassifierSystem()
{
	delete pSSch;
	delete pXover;
	delete pOldMsgList;
	delete pCurMsgList;
	delete pNewMsgList;
}


void    ClassifierSystem::findMatch()
//
//      find the match for all classifiers and all messages
//      each match is maintained by a match record
//      so if there are C classifiers and M messages
//      the maximum number of matches will be C * M
//
{
        CollectionIterator citer(csfList);
        nMatches = 0;
        matchSet.removeAll();
        while (citer) {
               nMatches += ((PClassifier) citer())->matchMsgs(msgs());
	       matchSet.add((PTObject)
			    & ((PClassifier) citer())->matchList());
               ++citer;
        }
}

void    ClassifierSystem::preProcessing()
//
//      for each classifier do
//              calculate the support
//              clear the change-in-strength
//      enddo
{
	CollectionIterator citer(csfList);
	flTotalSupport = 0;
        if (supportFlag)
            while (citer) {
                    flTotalSupport += ((PClassifier) citer())->setSupport();
                    ((PClassifier) citer())->strChg() = 0;
                    citer++;
            }
        else // no need to calculate support
            while (citer) {
                    ((PClassifier) citer())->strChg() = 0;
                    citer++;
            }
        if (flTotalSupport == 0)
            // if flTotalSupport is zero a divide-by-zero will
            // occur when setEBid is called
            flTotalSupport = noise();
}

void    ClassifierSystem::setBid(PClassifier pCsf)
{
	pCsf->bid()
	 = flBidRt * pow(pCsf->specificity(), flBidSp) * pCsf->fitness();

	if (pCsf->bid() < 0)
	    pCsf->bid() = 0;
}

void    ClassifierSystem::setEBid(PClassifier pCsf)
//
//      the effective bid is used for bidding purpose
//      the winning classifier will pay the bid rather than
//      the effective bid
//
//      (c.f. Goldberg's Genetic Algorithms in Search, Optimization and
//            Machine Learning )
//
{
	pCsf->ebid()
	 = flBidRt * pow(pCsf->specificity(), flEBidSp) *
	   pCsf->fitness() *
           (1 + supportFlag * pCsf->support() / flTotalSupport) +
	   noise();

	if (pCsf->ebid() < 0)
	    pCsf->ebid() = 0;
}

PTClassifier ClassifierSystem::auction(RCollection availCsfs,
				       RCMessage msg)
//
//      for the given message find a winner from
//      all the matching classifiers according to
//      the effective bids
//
{
	CollectionIterator citer(availCsfs);
	PTClassifier  pCsf, pWinner = NULL;
	TOArray	      matched;

	while (citer) {
             pCsf = (PTClassifier) citer();
             if (pCsf->taxon(0).matches(msg.operator RCBitString()))
                 matched.add(pCsf);
             ++citer;
	}

	if (matched.size() > 0) {
	   // there are classifiers that matched this message
	   // choose a winner
	   CollectionIterator titer(matched);
	   pWinner = (PTClassifier) titer();
           ++titer;
	   while (titer) {
		if (((PTClassifier) titer())->ebid() >
		    pWinner->ebid())
		    pWinner = (PTClassifier) titer();
                ++titer;
	   }
	}

	return pWinner;
}

void    ClassifierSystem::produceMsgs()
//
//	Prepare the message list for the next cycle
//
{
	if (oneMsgPCsf) {
           //
           //      for each message find a winning classifier
           //      each classifer can post only one message
	   //
	   pNewMsgList->removeAll();
           CollectionIterator miter(msgs());
           CollectionIterator citer(csfs());
           PTObject pObj;
           PTClassifier pWinner;

	   TObjSLList availCsfs;

	   availCsfs.addAll(csfs());
	   while (citer) {
                   ((PClassifier) citer())->reset();
                   setBid((PClassifier) citer());
                   setEBid((PClassifier) citer());
                   ++citer;
	   }

           while (miter) {
                  pWinner = auction(availCsfs, * (PCMessage) miter());
                  if (pWinner) {
                      ((PClassifier) pWinner)->post(*pNewMsgList,
                                                    (PCMessage) miter(),
                                                    NULL);
                      if (pWinner->msgsProduced() > 0)
                          availCsfs.remove(* (PTObject) pWinner);

                      // if oneMsgPCsf is false
                      // the bid tax is collected by checking
                      // the match set which is empty if oneMsgPCsf is true
                      pWinner->strChg() -=
                           flBidTaxRate * pWinner->fitness();
                   }
                   ++miter;
           }
	   return;
	}

        //
        //  if a classifier can post more than one message
        //  perform the following segment of code
        //

        findMatch();                    // find all matches
        pNewMsgList->removeAll();       // empty the new message list
        if (nMatches == 0)
	    return;  // no matches

        CollectionIterator miter(matchSet);
	PClassifier pCls;
	TOArray tmpMsgs;
	PTObject pObj;
        PCMatchList pMchList;
	sizeType msgsPosted = 0;

	if (nMatches > pNewMsgList->capacity())
	    // see whether message list is resizeable
            pNewMsgList->capacity(nMatches);

        if (nMatches > pNewMsgList->capacity()) {
            //
            // number of matches is larger than the
            // size of the message list
            //
	    // perform an auction amount the classifiers
	    // for message posting

	    while (miter) {
		   pCls = & ((RClassifier) ((PMatchList) miter())->csf());
		   setBid(pCls);
		   setEBid(pCls);
                   ++miter;
	    }

	    RWwoR_Select  rs(matchSet);

            while (msgsPosted < nMatches) {
		   pMchList = (PCMatchList) rs.select();
                   if (pMchList)
		       msgsPosted += pMchList->post(*pNewMsgList);
		   else
		       break;
	    }
	}
	else {
	    // all classifiers can post messages
	    while (miter) {
		   setBid(& ((RClassifier) ((PMatchList) miter())->csf()));
		   ((PCMatchList) miter())->post(*pNewMsgList);
                   ++miter;
	    }
	}
}

void    ClassifierSystem::updateCsfStr()
//
//      update strength of classifiers
//
{
	CollectionIterator citer;
	PClassifier pCsf;

        if (bbFlag) // run bucket brigade
	    bucketBrigade();

        citer.restart(csfList);
	while (citer)  {
           pCsf = (PClassifier) citer();

           // collect existence tax
	    pCsf->strChg() -=
                flLifeTaxRate * pCsf->fitness();

            // collect producer tax
            pCsf->strChg() -=  flPrdTaxMax *
                               pow(pCsf->msgsProduced() / msgs().capacity(),
                                   flPrdTaxPow);
            ++citer;
	}

        citer.restart(matchSet);
        while (citer) {
               // collect bid tax
	       pCsf = (PClassifier) & ((PCMatchList) citer())->csf();
	       pCsf->strChg() -=
                    flBidTaxRate * pCsf->fitness();
               ++citer;
	}

        citer.restart(csfList);
        flAvgCsfStr = 0;
        while (citer)  {
	   pCsf = (PClassifier) citer();
	   pCsf->fitness() += pCsf->strChg();
	   if (pCsf->fitness() > flCsfStrMax)
	       pCsf->fitness() = flCsfStrMax;
	   else
           if (pCsf->fitness() < flCsfStrMin)
               pCsf->fitness() = flCsfStrMin;
           flAvgCsfStr += pCsf->fitness();
           ++citer;
	}
        flAvgCsfStr /= csfList.size();
}

void	ClassifierSystem::run()
{
	     nTrials++;
             preProcessing();
	     detectors();
             produceMsgs();
             updateCsfStr();
	     effectors();
	     swapMsgList();
}

void    ClassifierSystem::newClassifiers(float flFracReplace)
{
	TObjSLList    newCSList(owner);
        TObjSLList    availCsfs;
        sizeType    uMortal, uAvailCsfs, i, uNumNewCsfs;
        CollectionIterator iter;

	uAvailCsfs = setRplFlags();  // mark classfiers' availability status
	ga((RCollection) newCSList, flFracReplace, *pSSch, *pXover);

        uNumNewCsfs = newCSList.size();
        if (uAvailCsfs > uNumNewCsfs) {
	    while ((uNumNewCsfs > 0) &&
                   (uAvailCsfs > uNumNewCsfs)) {
		   replaceBy((PClassifier) newCSList.remove_front());
		 uAvailCsfs--;
		 uNumNewCsfs--;
	    }
	    if (uNumNewCsfs == 0)
		return;
	}

	//
	// the number of classifiers available for replacement is
	// smaller than the number of new classifiers
	//
	for (i=0; i < csfs().size(); i++)
	     if (((PClassifier) csfs().elem(i))->canReplace())
                 csfs().setAt(i, newCSList.remove_front());
}

void    ClassifierSystem::ga(RCollection        newCSList,
			     float		flFracReplace,
			     RSelectionScheme   ssch,
			     RCrossover         xover)
{
	sizeType clsTotal = csfList.size(), numReplace;
	PTGAObj pChild;
	PCTGAObj pMate1, pMate2;

	unsigned uReplace = flFracReplace * clsTotal;
	uReplace = rangeCheck(uReplace, 2, clsTotal, (int) (clsTotal * 0.5));

	if (csfList.size() == 0)
	    error("No classifiers left!");

	ssch.reset(csfList);
        for (numReplace = uReplace; numReplace > 0; numReplace--) {
	     pMate1 = ssch.select();
	     pMate2 = ssch.select();
             xover.setMate(pMate1, pMate2);
	     pChild = xover.create();

             switch (nNewCsfStr) {
                  case 1 :
                     pChild->fitness() = flAvgCsfStr;
                  break;
                  default :
                     pChild->fitness() =
                      (pMate1->fitness() + pMate2->fitness()) * 0.5;
             }

	     pChild->mutate(flMRate);
	     newCSList.add(pChild);
	}
}

sizeType  ClassifierSystem::setRplFlags()
{
        CollectionIterator      iter(csfs());
        PMessage                pMsg;
        sizeType                availCsfs = 0;

        while (iter) {
		((PClassifier) iter())->canReplace(TRUE);
                ++iter;
	}

        iter.restart(*pNewMsgList);
	//
	// classifiers responsible for messages posting
	// in this cycle cannot be replaced
	//
	while (iter) {
		pMsg = (PMessage) iter();
		if (pMsg->pProducer)
		    pMsg->pProducer->canReplace(FALSE);
                if (pMsg->pMtchMsg1)
		    pMsg->pMtchMsg1->pProducer = NULL;
		if (pMsg->pMtchMsg2)
		    pMsg->pMtchMsg2->pProducer = NULL;
		++iter;
	}

        iter.restart(csfs());

	while (iter) {
		if (((PClassifier) iter())->canReplace() )
		    availCsfs++;
                ++iter;
	}

	return availCsfs;
}

void    ClassifierSystem::bucketBrigade()
{
	CollectionIterator miter(*pNewMsgList);
	PMessage        pMsg;

	while (miter) {
		pMsg = (PMessage) miter();
		if (pMsg->pProducer)
		    pMsg->pProducer->strChg() -= pMsg->intensity();
		pMsg->payReward();
                ++miter;
	}
}

float   ClassifierSystem::findAvgCsfStr()
{
	CollectionIterator iter(csfList);
	flAvgCsfStr = 0;
	if (csfList.size() > 0) {
	    while ( iter ) {
		    flAvgCsfStr += ((PCTGAObj) iter())->fitness();
		    ++iter;
	    }
	    flAvgCsfStr /= csfList.size();
	}
	return flAvgCsfStr;
}

