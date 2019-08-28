//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __MSGLIST_H )
#define   __MSGLIST_H

#include "toarray.h"
#include "message.h"

class   MsgList : public TObject
{
public:

	DECLARE_RTTI()

					MsgList(unsigned,
						BOOL = TRUE,
						sizeType =
						  DEFAULT_COLLECTION_SIZE);
					~MsgList();

        virtual         BOOL            isEqual(RCTObject) const;
        virtual         int             compare(RCTObject) const;
	virtual		void		printOn(ostream &) const;
					_shallowCopy(MsgList)
	virtual 	void		deepenShallowCopy();
	virtual		void		removeAll();

	virtual 	void		capacity(sizeType);
	virtual		sizeType	capacity() const;
	virtual 	BOOL		add(PMessage);
	virtual 	BOOL		isEffectorMsg(RCMessage) const;
	virtual		BOOL		isDetectorMsg(RCMessage) const;
	virtual 	PMessage	newMsg();
	virtual 	void		deleteMsg(PMessage);

			PTObject	at(sizeType) const;
			PTObject	elem(sizeType) const;

			sizeType	size() const;

			RCTOArray	effectorMsgs() const;
			RCTOArray	internalMsgs() const;
			RCTOArray	detectorMsgs() const;

			unsigned	dMsgMax() const;
			unsigned	iMsgMax() const;
			unsigned	eMsgMax() const;

			void		dMsgMax(unsigned);
			void		iMsgMax(unsigned);
			void		eMsgMax(unsigned);

			operator RCCollection () const;

protected :

	sizeType	dmMax;		// maximum number of detector
					// messages allowed in one cycle

	sizeType	emMax;		// maximum number of effector
					// messages allowed in one cycle

	sizeType	imMax;		// maximum number of internal
					// messages allowed in one cycle

	unsigned	msgLength;

	PCollection	pMsgs;
	TOArray  	freeMsgs;

	TOArray         eMsgs, iMsgs, dMsgs;
	sizeType        maxMsgs;
};

inline 	sizeType MsgList::size() const
{
	return pMsgs->size();
}

inline 	BOOL MsgList::isEffectorMsg(RCMessage msg) const
//
//	by default the tag "01" is assumed as effector msg
//
{
	return msg.test(1) && (!msg.test(0));
}

inline	BOOL MsgList::isDetectorMsg(RCMessage msg) const
//
//	by default the tag "00" is assumed as effector msg
//
{
	return ! (msg.test(0) || msg.test(1));
}

inline  void MsgList::removeAll()
{
	pMsgs->addContentsTo(freeMsgs);
	pMsgs->removeAll();
	eMsgs.removeAll();
	iMsgs.removeAll();
	dMsgs.removeAll();
}

inline	RCTOArray MsgList::effectorMsgs() const
{
	return eMsgs;
}

inline	RCTOArray MsgList::internalMsgs() const
{
	return iMsgs;
}

inline	RCTOArray MsgList::detectorMsgs() const
{
	return dMsgs;
}

inline unsigned   MsgList::dMsgMax() const
{
	return dmMax;
}

inline unsigned   MsgList::iMsgMax() const
{
	return imMax;
}

inline unsigned   MsgList::eMsgMax() const
{
	return emMax;
}

inline void    MsgList::dMsgMax(unsigned val)
{
        dmMax = val;
}

inline void    MsgList::iMsgMax(unsigned val)
{
        imMax = val;
}

inline void    MsgList::eMsgMax(unsigned val)
{
        emMax = val;
}

inline  void    MsgList::deleteMsg(PMessage pMsg)
//
//	add the message to the free store
//	it is the user's responsibility to
//	ensure all the messages are of the
//	same type
//
{
	if (pMsg)
	    if (! freeMsgs.add((PTObject) pMsg))
		error("freeMessage error");
}

inline  void    MsgList::capacity(sizeType newCap)
{
	if (freeMsgs.size() > newCap)
	    // reduce the size of the free store
	    freeMsgs.capacity(newCap);

	maxMsgs = newCap;
}

inline  PTObject MsgList::at(sizeType index) const
{
	return pMsgs->at(index);
}

inline  PTObject MsgList::elem(sizeType index) const
{
	return pMsgs->elem(index);
}

inline	MsgList::operator RCCollection () const
{
	return *pMsgs;
}

inline	sizeType MsgList::capacity() const
{
	return maxMsgs;
}

#endif

