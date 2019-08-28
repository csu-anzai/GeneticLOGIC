//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "msglist.h"
#include "message.h"
#include "hashset.h"

static  PCTypeInfo msglistBases[] = { &TObject::infoObj, 0 };
const TypeInfo  MsgList::infoObj("MsgList", msglistBases);

MsgList::MsgList(unsigned mLen,
		 BOOL dupMsgs,
		 sizeType size) :
		 msgLength(mLen),
		 dmMax(0), emMax(0), imMax(0), maxMsgs(size)
{
	ASSERT(mLen > 0);

	if (dupMsgs == TRUE)
	    pMsgs = (PCollection) new TOArray(reference, size, 0);
	else
	    pMsgs = (PCollection) new HashSet(reference);

	freeMsgs.ownsElements(owner);
	for (sizeType i = 0; i < maxMsgs ; i++)
	     freeMsgs.add((PTObject) new Message(msgLength));
}

MsgList::~MsgList()
{
	if (pMsgs) {
	    pMsgs->ownsElements(owner);
	    // pMsgs did not own its elements
	    // but its elements must be destroyed now
	    pMsgs->removeAll();
	    delete pMsgs;
	}
}

void MsgList::deepenShallowCopy()
{
	// create a shallow copy
	pMsgs = (PCollection) pMsgs->copy();
	pMsgs->deepenShallowCopy();
	// pMsgs does not own its elements
	pMsgs->ownsElements(reference);
	freeMsgs.deepenShallowCopy();
}

PMessage MsgList::newMsg()
{
	if (freeMsgs.size() > 0)
	    return (PMessage) freeMsgs.removeAt(freeMsgs.size() - 1);
	else
	    return new Message(msgLength);
}


BOOL MsgList::add(PMessage pMsg)
{
	if ( pMsg && (pMsgs->size() < maxMsgs) ) {
	    if ((pMsg->producer() != NULL) && isDetectorMsg(*pMsg)) {
		// hallucination message
		deleteMsg(pMsg);
		return FALSE;
	    }
	    else
		if (! pMsgs->add((PTObject) pMsg)) {
		    deleteMsg(pMsg);
		    return FALSE;
		}

	    if (isEffectorMsg(*pMsg)) {
		if (emMax > 0)
		    if (eMsgs.size() >= emMax)
			return FALSE;
		eMsgs.add((PTObject) pMsg);
	    }
	    else
		if (isDetectorMsg(*pMsg)) {
		    if (dmMax > 0)
			if (dMsgs.size() >= dmMax)
			    return FALSE;
		    dMsgs.add((PTObject) pMsg);
		}
	    else {
		if (imMax > 0)
		    if (iMsgs.size() >= imMax)
                        return FALSE;
		iMsgs.add((PTObject) pMsg);
	    }
	    return TRUE;

	}
	else
            return FALSE;
}

void	MsgList::printOn(ostream & out) const
{
	pMsgs->printOn(out);
}

BOOL    MsgList::isEqual(RCTObject obj) const
{
        if (isA() == obj.isA()) {
	    RCMsgList src = (RCMsgList) obj;
            if (dmMax != src.dmMax)
                return FALSE;
            if (emMax != src.emMax)
                return FALSE;
            if (imMax != src.imMax)
                return FALSE;
            if (maxMsgs != src.maxMsgs)
                return FALSE;
	    if (msgLength != src.msgLength)
                return FALSE;
            return pMsgs->isEqual(*src.pMsgs);
        }
        else
            return FALSE;
}

int     MsgList::compare(RCTObject obj) const
{
        if (isA() == obj.isA()) {
            RCMsgList src = (RCMsgList) obj;
            if (msgLength != src.msgLength)
                return msgLength > src.msgLength ? 1 : -1;
            else
                return pMsgs->compare(*src.pMsgs);
	}
        else
            return -1;
}

