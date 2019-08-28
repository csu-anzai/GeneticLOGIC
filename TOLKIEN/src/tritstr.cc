//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include   "tritstr.h"
#include   "tvalues.h"
#include   "message.h"
#include   "trand.h"
#include   "ranksb.h"
#include   <stdlib.h>

static  PCTypeInfo tritstrBases[] = { &TObject::infoObj, 0 };
const TypeInfo	TritString::infoObj("TritString", tritstrBases);

Trit& Trit::operator=(int nVal)
{
	tstring.setAt(uOffset, nVal);
	return *this;
}

TritString::TritString(const TritSubString & substr) : flMutDC(0)
{
	bits = substr.tstr.bits.at(substr.pos,substr.len);
	dcbits = substr.tstr.dcbits.at(substr.pos,substr.len);
}

TritString::TritString(unsigned nLength, BOOL randFlag) :
                flMutDC(0)
{
	bits.resize(nLength);
	dcbits.resize(nLength);

        if (randFlag)
            randomize();
	else {
	    bits.clear();
	    dcbits.clear();
	}
}

TritString::TritString(const char *szData) : flMutDC(0)
{
	register int nI = 0, nLength = strlen(szData);
	if (szData != NULL) {
	    bits.resize(nLength);
	    dcbits.resize(nLength);
	    dcbits.clear();
	    for (; nI < bits.length(); nI++)
		 switch (szData[nI]) {
		     case '0':
			bits.clear(nI);
			break;
		     case '1':
			bits.set(nI);
			break;
		     default:
			bits.set(nI);
			dcbits.set(nI);
			break;
		 }
	}
}

void    TritString::printOn(ostream & out) const
{
	for (register int nI = 0; nI < length(); nI++)
	     if (dcbits[nI])
		 out << (char) dontCare;
	     else
		 out << (unsigned) bits[nI];
}

unsigned  TritString::mutate(float flMRate)
{
	for (register int nI=length()-1,nJ=0;nI>=0;nI--)
	     if (tRand.flip(flMRate)) {
		nJ++;
		setRand(nI);
	     }
	return nJ;
}

void     TritString::randomize()
{
        for (register int nI=length()-1;nI>=0;nI--) 
             if (tRand.flip(flMutDC)) // set to #
                 setAt(nI,dontCare);
             else
                 setAt(nI,tRand(0,1));
}

void    TritString::setAt(unsigned uAt, int nVal)
{
	switch(nVal) {
		case 0:
		   dcbits.clear(uAt);
		   bits.clear(uAt);
		   break;
		case 1:
		   bits.set(uAt);
		   dcbits.clear(uAt);
		   break;
		default:
		   bits.set(uAt);
		   dcbits.set(uAt);
		   break;
	}
}

void    TritString::setRand(unsigned uAt)
{
        if (flMutDC == 0)
	    setAt(uAt, ((dcbits[uAt] ? -1 : bits.test(uAt)) +
                       (tRand(1,2) + 1)) % 3 - 1);
        else
            if (tRand.flip(flMutDC)) {
                // set to #
                dcbits.set(uAt);
                bits.set(uAt);
            }
            else if (tRand.flip(0.5)) {
               // set to 0
               dcbits.clear(uAt);
               bits.clear(uAt);
            }
            else {
               // set to 1
               bits.set(uAt);
               dcbits.clear(uAt);
            }
}

RTritString TritString::operator=(const char * szData)
{
	register int nI = 0, nLength = strlen(szData);
	if (szData != NULL) {
	    bits.resize(nLength);
	    dcbits.resize(nLength);
	    dcbits.clear();
	    for (; nI < bits.length(); nI++)
		 switch (szData[nI]) {
		     case '0':
			bits.clear(nI);
			break;
		     case '1':
			bits.set(nI);
			break;
		     default:
			bits.set(nI);
			dcbits.set(nI);
			break;
		 }
	}
        return *this;
}

int     TritString::compare(RCTObject obj) const
{
	if (!obj.isKindOf(isA()))
	    return -1;
	else {
	    const BitString& bstr1 = bits, bstr2 = ((TritString&) obj).bits;
	    if (bstr1 == bstr2)
		return 0;
	    else if (bstr1 < bstr2)
		     return -1;
	}
	return 1;
}

void    TritSubString::operator = (RCTritString str)
{
	tstr.bits.at(pos,len) = str.bits;
	tstr.dcbits.at(pos,len) = str.dcbits;
}

void    TritSubString::operator = (const TritSubString& src)
{
	tstr.bits.at(pos,len) = src.tstr.bits.at(pos,len);
	tstr.dcbits.at(pos,len) = src.tstr.dcbits.at(pos,len);
}

RTritString TritString::operator=(const TritSubString & y)
{
	bits = y.tstr.bits.at(y.pos,y.len);
	dcbits = y.tstr.dcbits.at(y.pos,y.len);
	return *this;
}

TritSubString TritString::at(int pos, int len) const
{
	return TritSubString(*this, pos, len);
}

TritSubString TritString::before(int pos) const
{
	return TritSubString(*this, 0, pos);
}

TritString  operator + (RCTritString x, RCTritString y)
{
	TritString r(x.bits + y.bits, x.dcbits + y.dcbits);
	return r;
}

BOOL    TritString::matches(RCBitString test) const
{
        return bits == (test | dcbits) ? TRUE : FALSE;
}

BOOL    TritString::contains(RCBitString test) const
{
	//
	// if the message is shorter than the TritString,
	// the function will return true if the bits of the
	// message match the corresponding bits of the classifier
	// part, e.g.
	//
	//      1000##000 will match the message 100
	//      1##0001   will not match the message 10110
	//
        return bits.contains(test | dcbits) ? TRUE : FALSE;
}

BitString    TritString::filter(RCBitString src) const
//
//      creates a bitstring using this tritstring as a template
//      and fills the don't cares with the corresponding bits
//      from src
//
{
	return bits & ~dcbits | src & dcbits;
}

void    TritString::operator += (RCTritString src)
{
	bits += src.bits;
	dcbits += src.dcbits;
}

void    TritString::evenPtCrossover(RCTritString mother,
                                    RCTritString father, int nXpts)
//
//      no checking on the length of the strings is performed
//      because this is assumed to be the task of the class Crossover
//      it is assumed that nXpts is even
//
{
        PTritString pShorter, pLonger;
	int i, j, k, len, nXpt;
	BOOL   flag;

	if (mother.length() >= father.length()) {
	    pLonger = (PTritString) &mother;
	    pShorter = (PTritString) &father;
	}
	else {
	    pLonger =(PTritString) &father;
	    pShorter = (PTritString) &mother;
	}

	len = pShorter->length() - 1;
	*this = *pLonger;

        IntVec xpts(nXpts);

	//
	// generate a sorted sequence of random numbers
	// in the range 0 to pShorter->length() - 1
	//
        ranksb.ordered(xpts, nXpts, pShorter->length());

	if (xpts[0] == 0) {
	    i = xpts.capacity() - 1;
	    if (xpts[i] == len)
	       xpts[i]--;
	}

        for (i = 0; i < nXpts; i+= 2) {
                j = xpts[i];
		k = xpts[i+1] - j;
                bits.at(j, k) = pShorter->bits.at(j, k);
                dcbits.at(j, k) = pShorter->dcbits.at(j, k);
        }
}

void    TritString::oddPtCrossover(RCTritString mother,
                                   RCTritString father, int nXpt)
//
//      no checking on the length of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
//      so if length of father = length of mother = 1
//      a runtime error will occur
//
{
        PTritString pCurMate, pNextMate, pTemp;
	int cur, i, j, k, m, len;
        BOOL flag;

        if (mother.length() >= father.length()) {
            pNextMate = (PTritString) &mother;
	    pCurMate = (PTritString) &father;
        }
        else {
            pNextMate = (PTritString) &father;
            pCurMate = (PTritString) &mother;
        }

	resize(0);  // empty child first

	//
	// generate a sorted sequence of random numbers
	// in the range 0 to pCurMate->length() - 1
	//
        IntVec xpts(nXpt);
	ranksb.ordered(xpts, nXpt, pCurMate->length());

	for (i=0, cur=0; i < nXpt; i++) {
               m = xpts[i] - cur + 1;
               bits += pCurMate->bits.at(cur, m);
               dcbits += pCurMate->dcbits.at(cur, m);
               // swap parents
               pTemp = pCurMate;
               pCurMate = pNextMate;
	       pNextMate = pTemp;
               cur = xpts[i] + 1;
        }

        if (cur <= pCurMate->length()) {
            // copy last segment
	    m = pCurMate->length() - cur;
            bits += pCurMate->bits.at(cur, m);
            dcbits += pCurMate->dcbits.at(cur, m);
        }
}

void	TritString::uniformCrossover(RCTritString mother,
				     RCTritString father,
                                     float flXRate)
{
	resize(mother.length());

	for (register int nI = 0 ; nI < mother.length(); nI++)
		if (tRand.flip(flXRate))
		    assign(nI, mother);
		else
		    assign(nI, father);
}

TritString::operator BitString() const
{
        BitString bstr(bits);
        bstr |= dcbits;
        return bstr;
}

unsigned TritString::matchCount(RCTritString src) const
{
//
//      count number of positions of similarity
//
        for (register int j = length() - 1, nCount = 0; j >= 0; j--)
                if (valueAt(j) == src.valueAt(j))
                    nCount++;
        return nCount;
}

#if defined(_MSC_VER) || defined(__BORLANDC__)
TritString  shorttoTritString(unsigned short value)
//
//      create a TritString with the first position
//      as the least significant value
{
	div_t	   divr;
        BitString  bits, dcbits;
	unsigned short s = value;

	divr = div(s, 3);
	for (; divr.quot > 3;s = divr.quot) {
	    switch( divr.rem ) {
		case 0 :
		    bits += 0;
		    dcbits += 0;
		    break;
		case 1 :
		    bits += 1;
		    dcbits += 0;
		    break;
		case 2 :
		    bits += 1;
		    dcbits += 1;
		    break;
	    }
	    divr = div(s, 3);
        }
        return TritString(bits, dcbits);
}

TritString  longtoTritString(unsigned long value)
{
        ldiv_t      ldivr;
        BitString  bits, dcbits;
	unsigned long l = value;

        ldivr = ldiv(l, 3);
        for (; ldivr.quot > 3;l = ldivr.quot) {
            switch( ldivr.rem ) {
		case 0 :
		    bits += 0;
		    dcbits += 0;
		    break;
		case 1 :
		    bits += 1;
		    dcbits += 0;
		    break;
		case 2 :
		    bits += 1;
		    dcbits += 1;
		    break;
	    }
	}

        return TritString(bits, dcbits);
}
#else
TritString  shorttoTritString(unsigned short value)
//
//      create a TritString with the first position
//      as the least significant value
{
        BitString  bits, dcbits;
	int	quot, rem, s;

	quot = value / 3;
	rem  = value % 3;

        for (; quot > 3;) {
            switch( rem ) {
                case 0 :
                    bits += 0;
                    dcbits += 0;
                    break;
                case 1 :
		    bits += 1;
                    dcbits += 0;
                    break;
                case 2 :
                    bits += 1;
                    dcbits += 1;
                    break;
            }
	    s = quot;
	    quot =  s / 3;
	    rem  =  s % 3;
        }
        return TritString(bits, dcbits);
}

TritString  longtoTritString(unsigned long value)
{
        BitString  bits, dcbits;
        unsigned long quot, rem, s;

	quot = value / 3;
	rem  = value % 3;
	for (; quot > 3;) {
            switch( rem ) {
                case 0 :
                    bits += 0;
                    dcbits += 0;
                    break;
                case 1 :
                    bits += 1;
                    dcbits += 0;
                    break;
		case 2 :
                    bits += 1;
                    dcbits += 1;
                    break;
            }
	    s = quot;
	    quot = s / 3;
	    rem  = s % 3;
        }

        return TritString(bits, dcbits);
}
#endif

int         operator == (RCTritString t1, RCTritString t2)
{
        return (t1.bits == t2.bits && t1.dcbits == t2.dcbits) ? 1 : 0;
}

int         operator != (RCTritString t1, RCTritString t2)
{
	if (t1.bits == t2.bits) {
		if (t1.dcbits == t2.dcbits)
		   return 0;
		else
		   return 1;
	}
	else
	   return 1;
}

RTritString  TritString::operator=(RCTritString source)
{
	bits = source.bits;
	dcbits = source.dcbits;
	flMutDC = source.flMutDC;
	return *this;
}

RTritString TritString::operator=(RCBitString source)
{
	bits = source;
	dcbits.resize(source.length());
	dcbits.clear();
	return *this;
}


float TritString::asFloat(int nFrom, int nLength) const
//
//      bits[i]         dcbits[i]       integer value
//      ---------------------------------------------
//        0                0                 0
//        1                0                 1
//        1                1                 2
//
{
    if (nLength <= 0)
	return 0;

    register int i, j, k;
    register float accum, power3 = 1;

    if (nFrom < 0)
	i = 0;
    else
        i = (nFrom > length() - 1) ? 0 : nFrom;

    if (nLength < 0)
        j = 1;
    else
        j = (nLength > (j = length() - i)) ? j : nLength;

    for (k = i; k < j; k++) {
	if ( bits.test(k) ) {
           if ( dcbits.test(k) ) // 11 -> 2
               accum += 2 * power3;
	   else // 10 -> 1
	       accum += power3;
	}
	power3 *= 3;
    }

    return accum;
}

BOOL TritString::isEqual( RCTObject obj ) const
{
	if (obj.isKindOf(isA())) {
		if ( ! ( bits == ((RCTritString) obj).bits ) )
		     return FALSE;
		else
		     return dcbits == ((RCTritString) obj).dcbits;
	}
	else
	    return FALSE;
}

