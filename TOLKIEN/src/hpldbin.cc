//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//	Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "trand.h"
#include "tvalues.h"
#include "xover.h"
#include "dpldbin.h"
#include "graycode.h"

static  PCTypeInfo binhaploidBases[] = { &BinIndividual::infoObj, 0 }; const
TypeInfo  BinHaploid::infoObj("BinHaploid", binhaploidBases);

BinHaploid::BinHaploid(unsigned uLen, unsigned uFlag, BOOL bFlag) :
                       graycodeFlag(bFlag) {
                       bits.resize(rangeCheck(uLen,1,MAX_CHROMLEN,DEFAULT_CHROMLEN));
                       if (uFlag == RANDOMIZE) randomize(); }

BinHaploid::BinHaploid(const BitString & src, BOOL bFlag) : bits(src),
			graycodeFlag(bFlag) { }

BinHaploid::BinHaploid(const char  *pString, BOOL bFlag) :
	bits(atoBitString(pString)), graycodeFlag(bFlag) { }

float    BinHaploid::asFloat(int nFrom, int nLength) const
{
        if (graycodeFlag == TRUE) {
            BitString bstr = bits.at(nFrom, nLength);
            if (bstr.length() <= LONGBITS) // use tGrayCode for long values
                return tGrayCode.asBin(bstr.asLong());
            else {
                bstr.deGray();
                bstr.right_trim(0);
                return bstr.bin2float(0, bstr.length());
            }
        }
	else
            return bits.bin2float(nFrom, nLength);
}

unsigned long   BinHaploid::asLong(int nFrom, int nLength) const
{
        BitString bstr;
        if (graycodeFlag == TRUE) {
            bstr = bits.at(nFrom, nLength);
            return tGrayCode.asBin(bstr.asLong());
        }
        else
	    return bits.bin2long(nFrom, nLength);
}

RBinIndividual BinHaploid::operator=(RCBinHaploid source)
{
	if (! isSame(source) ) {
	    TGAObj::operator=(source);
	    bits = source.bits;
	    graycodeFlag = source.graycodeFlag;
	}
	return *this;
}

RBinIndividual BinHaploid::operator=(RCBinDiploid source)
{
        TGAObj::operator=(source);
        bits = (RCBitString) source;
        isGrayCode(source.isGrayCode());
	return *this;
}

RBinHaploid  BinHaploid::operator=(unsigned long lVal)
{
	unsigned lastLength = bits.length();
	if (graycodeFlag == TRUE)
	    bits = longtoBitString(tGrayCode.asGray(lVal));
	else
	    bits = longtoBitString(lVal);
	bits.resize(lastLength);
	return *this;
}

void    BinHaploid::randomize()
{
        for (register int nI = bits.length() - 1;nI >= 0; nI--)
             if (tRand.flip(0.5))
		 bits.set(nI);
	     else
		 bits.clear(nI);
}

void BinHaploid::printOn( ostream & out) const
{
	for (register int nI = 0; nI < bits.length() ; nI++)
	     out << bits.test(nI);
}

int     BinHaploid::compare(RCTObject obj) const
{
	if ( obj.isA() != isA() )
	    return -1;
	else {
	    float flVal1 = asFloat(), flVal2 = ((RBinHaploid) obj).asFloat();
	    if (flVal1 == flVal2)
		return 0;
	    else if (flVal1 < flVal2)
		    return -1;
	    else
		return 1;
	}
}

void     BinHaploid::mutate(float flRate)
{
        for (register int nI=length() - 1; nI >=0; nI--)
             if (tRand.flip(flRate))
		 invert(nI);
}

PTGAObj  BinHaploid::oddPtCrossover(RCTGAObj mate, int nXpt) const
{
	if ((mate.isKindOf(BinHaploid::typeInfo())) ||
	    (mate.isKindOf(BinDiploid::typeInfo()))) {
            PBinHaploid  pChild = (PBinHaploid) deepCopy();
            pChild->bits.oddPtCrossover(bits,
                                        ((RCBinHaploid) mate).bits,
					nXpt);
	    return pChild;
	}
	else
            return (PTGAObj) NOOBJECT;
}

void    BinHaploid::oddPtCrossover(RCTGAObj mate, int nXpt, RTGAObj child) const
{
	if (child.isKindOf(isA()))
            ((RBinHaploid) child).bits.oddPtCrossover(bits,
                                                   ((RCBinHaploid) mate).bits,
                                                   nXpt);
	else
            mate.oddPtCrossover(*this, nXpt, child);
}

PTGAObj  BinHaploid::evenPtCrossover(RCTGAObj mate, int nXpts) const
{
	if ((mate.isKindOf(BinHaploid::typeInfo())) ||
	    (mate.isKindOf(BinDiploid::typeInfo()))) {
            PBinHaploid  pChild = (PBinHaploid) deepCopy();
            pChild->bits.evenPtCrossover(bits,
                                        ((RCBinHaploid) mate).bits,
                                        nXpts);
	    return pChild;
	}
	else
            return (PTGAObj) NOOBJECT;
}

void    BinHaploid::evenPtCrossover(RCTGAObj mate, int nXpts,
				    RTGAObj child) const
{
	if (child.isKindOf(isA()))
            ((RBinHaploid) child).bits.evenPtCrossover(bits,
                                                   ((RCBinHaploid) mate).bits,
                                                   nXpts);
	else
            mate.evenPtCrossover(*this, nXpts, child);
}

PTGAObj  BinHaploid::uniformCrossover(RCTGAObj mate, float flXRate) const
{
	if ((mate.isKindOf(BinHaploid::typeInfo())) ||
	    (mate.isKindOf(BinDiploid::typeInfo()))) {
            PBinHaploid  pChild = (PBinHaploid) deepCopy();
            pChild->bits.uniformCrossover(bits,
					 ((RCBinHaploid) mate).bits,
					 flXRate);
	    return pChild;
	}
	else
            return (PTGAObj) NOOBJECT;
}

void    BinHaploid::uniformCrossover(RCTGAObj mate, float flXRate,
				     RTGAObj child) const
{
	if (child.isKindOf(isA()))
            ((RBinHaploid) child).bits.uniformCrossover(bits,
                                                ((RCBinHaploid) mate).bits,
                                                flXRate);
	else
	    mate.uniformCrossover(*this, flXRate, child);
}

unsigned    BinHaploid::hammingDistance(RCBitString src) const
{
	BitString  tmp;
	xor(bits, src, tmp);
	return tmp.bitsum();
}

double      BinHaploid::distance(RCTGAObj obj) const
{
	BitString  tmp;
        BOOL    flag = obj.isKindOf(isA());

        if (flag) {
	    xor(bits, ((RCBinHaploid) obj).bits, tmp);
	    return tmp.bitsum();
	}
	else
	    if (obj.isKindOf(BinDiploid::typeInfo())) {
		xor(bits, (((RCBinDiploid) obj).
				operator RCBinHaploid()).bits, tmp);
		return tmp.bitsum();
	    }
            else
		return MAXFLOAT;
}

