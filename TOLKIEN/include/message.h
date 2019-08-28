//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __MESSAGE_H)
#define __MESSAGE_H

#include <iostream.h>
#include "tobject.h"
#include "bitstr.h"
#include "trand.h"
#include "toarray.h"
#include "clsifier.h"

class   Message : public TObject
{
public:

	DECLARE_RTTI()
				Message();
				Message(unsigned);
				Message(const BitString &, float = 0);
				Message(const char *, float = 0);
	virtual		    	~Message();

				_shallowCopy(Message)
	virtual  void           deepenShallowCopy();

	virtual  BOOL 	    	isEqual( RCTObject ) const;
	virtual  int 	    	compare(RCTObject ) const;
	virtual  unsigned   	length() const;

	virtual  void 	    	printOn( ostream  &) const;

	virtual  RCMessage  	operator=(RCMessage);
	virtual  RCMessage      operator=(RCBitString);
	virtual  BitStrBit  	operator[](int);
	virtual  BYTE       	operator[](int) const;
        virtual                 operator RCBitString() const;

	virtual  int 		test(int) const;
	virtual  PCClassifier   producer() const;
	virtual  void           producer(PCClassifier);
	virtual  float      	intensity() const;
	virtual  void	    	intensity(float);
	virtual  void	    	payReward(float);
	virtual  void	    	payReward();
	virtual	 void		filter(RCTritString, PCMessage);
	virtual  void           matchedMsgs(PCMessage, PCMessage);

	friend  class   ClassifierSystem;

protected:

	BitString  bits;

	float	   flIntensity;	// the bid paid by the classifier
				// for posting this message

	PClassifier pProducer;  // pointer to producer
				// this pointer may be invalid
				// if the classifier is destroyed
				// by GA

	PMessage    pMtchMsg1, pMtchMsg2;
				//
				// messages matching matched classifiers
				// pMtchMsg2 is NULL for one-taxon-one-action
				// classifiers
				//
};

inline  RCMessage Message::operator=(RCBitString src)
{
	bits = src;
	pProducer = NULL;
	flIntensity = 0;
	return *this;
}

inline  RCMessage Message::operator=(RCMessage src)
{
	bits = src.bits;
        pProducer = src.pProducer;
        flIntensity = src.flIntensity;
	return *this;
}

inline  Message::Message() : pProducer(NULL),
                             flIntensity(0),
                             pMtchMsg1(NULL),
                             pMtchMsg2(NULL)
{
}

inline  Message::Message(unsigned length) : pProducer(NULL),
                                            flIntensity(0),
                                            pMtchMsg1(NULL),
                                            pMtchMsg2(NULL)

{
	bits.resize(length);
}

inline  Message::Message(const BitString & src, float flVal) :
			bits(src),
                        pProducer(NULL),
                        pMtchMsg1(NULL),
			pMtchMsg2(NULL),
			flIntensity(flVal)
{
}

inline  Message::Message(const char *szData, float flVal) :
                        pProducer(NULL),
                        pMtchMsg1(NULL),
                        pMtchMsg2(NULL),
			flIntensity(flVal)
{
	bits = atoBitString(szData);
}

inline  Message::~Message()
{
}

inline  BOOL Message::isEqual( RCTObject obj ) const
{
	if (obj.isA() == this->isA())
	    return bits == ((RCMessage) obj).bits;
	return FALSE;
}

inline  void    Message::deepenShallowCopy()
{
}

inline int Message::compare(RCTObject obj) const
{
        if (isSame(obj))
            return 0;

	if (isA() == obj.isA()) {
	    long val1 = bits.asLong(),
		 val2 = ((RCMessage) obj).bits.asLong();

	    if (val1 == val2)
		return 0;
	    else
		if (val1 > val2)
		    return 1;
	}

	return -1;
}

inline  unsigned Message::length() const
{
	return bits.length();
}

inline  Message::operator   RCBitString() const
{
	return bits;
}

inline	int Message::test(int i) const
{
	return bits.test(i);
}

inline  BitStrBit  Message::operator[](int i)
{
	if ((unsigned)(i) >= bits.length()) error("illegal bit index");
	    return BitStrBit(bits, i);
}

inline  BYTE       Message::operator[](int i) const
{
       if ((unsigned)(i) >= bits.length()) error("illegal bit index");
	   return bits.test(i);
}

inline void Message::producer(PCClassifier pCls)
{
	pProducer = (PClassifier) pCls;
}

inline PCClassifier Message::producer() const
{
	return pProducer;
}

inline float Message::intensity() const
{
	return flIntensity;
}

inline void Message::intensity(float flVal)
{
	flIntensity = flVal;
}

inline	void Message::payReward()
{
	payReward(flIntensity);
}

inline void Message::matchedMsgs(PCMessage pMsg1, PCMessage pMsg2)
{
	pMtchMsg1 = (PMessage) pMsg1;
	pMtchMsg2 = (PMessage) pMsg2;
}

#endif

