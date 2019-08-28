//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "clsifier.h"
#include "message.h"
#include "clsifier.h"

static  PCTypeInfo messageBases[] = { &TObject::infoObj, 0 };
const TypeInfo	Message::infoObj("Message", messageBases);

void 	Message::filter(RCTritString cp, PCMessage pMsg)
//
//	creates a message using cp as a template and fills
//	the don't cares with the corresponding bits from msg
//
//      the negation bit is not included in the message
{
	if (pMsg)
            bits = cp.filter(pMsg->operator RCBitString());
	else
	    bits = (RCBitString) cp;
}

void     Message::printOn( ostream  & out ) const
{
	 out << bits;
         if (pProducer == NULL)
	     out << "(NULL)";
         else
             out << "(" << *pProducer << ")";
}

void Message::payReward(float flBid)
{
        PClassifier pSupplier1 = NULL, pSupplier2 = NULL;

        if (pMtchMsg1)
            pSupplier1 = pMtchMsg1->pProducer;
        if (pMtchMsg2)
            pSupplier2 = pMtchMsg2->pProducer;

        if (pSupplier1 && pSupplier2) {
	    pSupplier1->strChg() += flBid / 2;
	    pSupplier2->strChg() += flBid / 2;
        }
        else
            if (pSupplier1)
		pSupplier1->strChg() += flBid;
            else
                if (pSupplier2)
		    pSupplier2->strChg() += flBid;
}

