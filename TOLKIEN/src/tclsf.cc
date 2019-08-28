//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "tclsf.h"
#include "message.h"
#include "trand.h"
#include "xover.h"
#include "csfmatch.h"
#include "msglist.h"

static  PCTypeInfo tclassiferBases[] = { &Classifier::infoObj, 0 };
const TypeInfo	TClassifier::infoObj("TClassifier", tclassiferBases);

TClassifier::TClassifier(RTritString srcTaxon, RTritString srcAction) :
	Classifier(), txn(srcTaxon), act(srcAction), mlist(*this)
{
	findSpecificity();
	findLength();
}

TClassifier::TClassifier(RCTClassifier src) : Classifier(), mlist(*this)
{
	TGAObj::operator=((RCTGAObj) src);
	txn = src.txn;
	act = src.act;
	findSpecificity();
	findLength();
}

BOOL TClassifier::isEqual( RCTObject   Obj ) const
{
	PTClassifier pObj;
	if (Obj.isA() == this->isA()) {
	    pObj = (PTClassifier) & Obj;
	    if (txn.isEqual(pObj->txn) && act.isEqual(pObj->act ))
	       return TRUE;
	}
	return FALSE;
}

RTClassifier TClassifier::operator=(RCTClassifier src)
{
        if ( !isSame(src) ) {
	    TGAObj::operator=(src);
	    txn = src.txn;
	    act = src.act;
	    mlist = src.mlist;
	    findSpecificity();
	    findLength();
	}
	return *this;
}

void    TClassifier::oddPtCrossover(RCTGAObj mate, int nXpts, RTGAObj child) const
{
	PRECONDITION(mate.isKindOf(isA()) &&
		     child.isKindOf(isA()));

	RCTClassifier materef = (RCTClassifier) mate;
	RTClassifier cref = (RTClassifier) child;
	TritString      tstr1 = txn + act;
	TritString      tstr2 = materef.txn + materef.act;
	TritString      tstr3;

	tstr3.oddPtCrossover(tstr1, tstr2, nXpts);
	cref.txn = tstr3.at(0,txn.length());
	cref.act = tstr3.at(txn.length(), tstr3.length() - txn.length());
	cref.findSpecificity();
	cref.findLength();
}

void    TClassifier::evenPtCrossover(RCTGAObj mate, int nXpts,
				     RTGAObj child) const
{
	PRECONDITION(mate.isKindOf(isA()) &&
		     child.isKindOf(isA()));

	RCTClassifier materef = (RCTClassifier) mate;
	RTClassifier cref = (RTClassifier) child;
        TritString      tstr1 = txn + act;
        TritString      tstr2 = materef.txn + materef.act;
	TritString      tstr3;

        tstr3.evenPtCrossover(tstr1, tstr2, nXpts);
        cref.txn = tstr3.at(0,txn.length());
        cref.act = tstr3.at(txn.length(), tstr3.length() - txn.length());
	cref.findSpecificity();
	cref.findLength();
}

void     TClassifier::printOn( ostream  & out) const
{
        out << txn << " : " << act << " [" << fitness() << "]";
}


void     TClassifier::uniformCrossover(RCTGAObj mate, float flXRate,
				       RTGAObj child) const
{
   ((RTClassifier) child).txn.uniformCrossover(txn,
                                (RTritString) ((RTClassifier) mate).txn,
					      flXRate);
   ((RTClassifier) child).act.uniformCrossover(act,
                                (RTritString) ((RTClassifier) mate).act,
					     flXRate);
   ((RTClassifier) child).findSpecificity();
   ((RTClassifier) child).findLength();
}

void    TClassifier::findSpecificity()
{
        unsigned uLenTotal = txn.length(),
                 uDCTotal = txn.findDontCare();
	if (uLenTotal)
	    flSpecificity = ((double) (uLenTotal - uDCTotal)) / uLenTotal;
	else
	    flSpecificity = 0;
}

BOOL    TClassifier::post(RMsgList msgList, PCMessage pSrcMsg,
			  PCMessage pDummy, float flBid)
//
//	post messages to msglist
//
{
	PMessage pMsg = msgList.newMsg();
        pMsg->filter(act, pSrcMsg);
	pMsg->producer(this);
	pMsg->intensity(flBid);
	pMsg->matchedMsgs(pSrcMsg, NULL);

	if (msgList.add(pMsg)) {
	    uMsgProd++;
            uTMsgProd++;
	    return TRUE;
	}
        else
	   return FALSE;
}

#if !defined ( __postfix_inc__ )
int	TClassifier::matchMsgs(RCCollection msgList)
{
	CollectionIterator miter(msgList);
	PCMessage       pMsg;

	mlist.removeAll();
	while (miter) {
	      pMsg = ((PCMessage) miter());
              if (txn.matches(pMsg->operator RCBitString()))
		  mlist.add(pMsg);
              ++miter;
	}
	uTMtch += mlist.size();
	return mlist.size();
}
#else
int	TClassifier::matchMsgs(RCCollection msgList)
{
	CollectionIterator miter(msgList);
	PCMessage       pMsg;

	mlist.removeAll();
	while (miter) {
              pMsg = ((PCMessage) miter++);
              if (txn.matches(pMsg->operator RCBitString()))
		  mlist.add(pMsg);
	}
	uTMtch += mlist.size();
	return mlist.size();
}
#endif
