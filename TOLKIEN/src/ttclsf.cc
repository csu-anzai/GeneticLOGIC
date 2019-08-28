//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "ttclsf.h"
#include "message.h"
#include "trand.h"
#include "xover.h"
#include "csfmatch.h"
#include "msglist.h"

static  PCTypeInfo ttclsfBases[] = { &TTClassifier::infoObj, 0 };
const TypeInfo  TTClassifier::infoObj("TTClassifier", ttclsfBases);

TTClassifier::TTClassifier(RTritString srcTaxon1,
			   RTritString srcTaxon2,
			   RTritString srcAction) :
		Classifier(),
		taxon1(srcTaxon1),
		taxon2(srcTaxon2),
		act(srcAction),
		mlist(*this)
{
	// ensure same length
	taxon2.resize(taxon1.length() + 1);
	act.resize(taxon1.length());
	setNegationBit();
	findSpecificity();
	findLength();
}

TTClassifier::TTClassifier(RCTTClassifier src) : Classifier(), mlist(*this)
{
	TGAObj::operator=((RCTGAObj) src);
	taxon1 = src.taxon1;
	taxon2 = src.taxon2;
	act = src.act;
	findSpecificity();
	findLength();
}

void    TTClassifier::printOn( ostream  & out) const
{
	TritString t2 = taxon2.at(1, taxon1.length());

	out << taxon1 << " , ";
	if (taxon2.valueAt(0) == 1)
	    out << '~';
	out << t2;
	out << " : " << act << " [" << fitness() << "]";
}

BOOL	TTClassifier::post(RMsgList msgList, PCMessage pMsg1,
			   PCMessage pMsg2, float flBid)
{
        PMessage pMsg = msgList.newMsg();
	pMsg->filter(act, pMsg1);
	pMsg->producer(this);
	pMsg->intensity(flBid);
	pMsg->matchedMsgs(pMsg1, pMsg2);

        if (msgList.add(pMsg)) {
            uMsgProd++;
	    return TRUE;
        }
        else
           return FALSE;
}

int     TTClassifier::matchMsgs(RCCollection msgList)
{
	CollectionIterator  miter1(msgList);
	CollectionIterator  miter2(msgList);
        PCMessage       pMsg1;
	PTTMatchList    pInfo;
        BOOL            flag;

        mlist.removeAll();
	// check all matches for pairs of different messages
	while (miter1) {
	      pMsg1 = (PCMessage) miter1();
	      if (taxon1.matches(pMsg1->operator RCBitString())) {
		  miter2.restart();
                  if ( taxon2.valueAt(0) != 1 ) {
                       while (miter2) {
			     if (taxon2.matches(((PCMessage) miter2())->operator RCBitString()))
				 mlist.add(pMsg1,
					   (PCMessage) miter2());
			     miter2++;
		       }
		  }
		  else { // negated condition
		       flag = TRUE;
		       while (miter2) {
			     if (taxon2.matches(((PCMessage) miter2())->operator RCBitString())) {
				flag = FALSE;
				break;
			     }
			     miter2++;
		       }

		       if (flag)
			   mlist.add(pMsg1, NULL);
		  }
	      }
	      miter1++;
	}

	uTMtch += mlist.size();
	return mlist.size();
}

void    TTClassifier::findSpecificity()
{
	unsigned uLenTotal = 2 * taxon1.length(),
		 uDCTotal  = taxon1.findDontCare() + taxon2.findDontCare();
	if (uLenTotal)
	    flSpecificity = ((double) (uLenTotal - uDCTotal)) / uLenTotal;
	else
	    flSpecificity = 0;
}

void TTClassifier::oddPtCrossover(RCTGAObj mate,
				  int xpts, RTGAObj child) const
{
	PRECONDITION(mate.isKindOf(isA()) &&
		     child.isKindOf(isA()));

	TritString	tstr1 = taxon1 + taxon2 + act,
			tstr2 = ((RTTClassifier) mate).taxon1 +
				((RTTClassifier) mate).taxon2 +
				((RTTClassifier) mate).act,
			tstr3;

	tstr3.oddPtCrossover(tstr1, tstr2, xpts);

	((RTTClassifier) child).taxon1 = tstr3.at(0, taxon1.length());

	((RTTClassifier) child).taxon2 =
			tstr3.at(taxon1.length(),
				 taxon2.length());

	((RTTClassifier) child).act =
			tstr3.at(taxon1.length() + taxon2.length(),
				 act.length());
}

void TTClassifier::evenPtCrossover(RCTGAObj mate, int segments,
				   RTGAObj child) const
{
	PRECONDITION(mate.isKindOf(isA()) &&
		     child.isKindOf(isA()));

	TritString	tstr1 = taxon1 + taxon2 + act,
			tstr2 = ((RTTClassifier) mate).taxon1 +
				((RTTClassifier) mate).taxon2 +
				((RTTClassifier) mate).act,
			tstr3;

	tstr3.evenPtCrossover(tstr1, tstr2, segments);

	((RTTClassifier) child).taxon1 = tstr3.at(0, taxon1.length());

	((RTTClassifier) child).taxon2 = tstr3.at(taxon1.length(),
						  taxon2.length());

	((RTTClassifier) child).act = tstr3.at(taxon1.length() +
						  taxon2.length(),
						  act.length());

}

void TTClassifier::uniformCrossover(RCTGAObj mate, float xrate,
				    RTGAObj child) const
{
	PRECONDITION(mate.isKindOf(isA()) &&
		     child.isKindOf(isA()));

	((RTTClassifier) child).taxon1.uniformCrossover(taxon1,
					      ((RTTClassifier) mate).taxon1,
					      xrate);
	((RTTClassifier) child).taxon2.uniformCrossover(taxon2,
					      ((RTTClassifier) mate).taxon2,
					      xrate);
	((RTTClassifier) child).act.uniformCrossover(act,
					      ((RTTClassifier) mate).act,
					      xrate);

	((RTTClassifier) child).setNegationBit();
}

BOOL TTClassifier::isEqual( RCTObject   Obj ) const
{
	PTTClassifier pObj;

	if (Obj.isA() == this->isA()) {
	    pObj = (PTTClassifier) & Obj;
	    if (taxon1.isEqual(pObj->taxon1))
		if (taxon2.isEqual(pObj->taxon2))
		    if (act.isEqual(pObj->act))
			return TRUE;
	}

	return FALSE;
}

RTTClassifier TTClassifier::operator=(RCTTClassifier src)
{
	if ( !isSame(src) ) {
	    TGAObj::operator=(src);
	    taxon1 = src.taxon1;
	    taxon2 = src.taxon2;
	    act = src.act;
	    mlist = src.mlist;
	    findSpecificity();
	    findLength();
	}
	return *this;
}
