//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __HPLDBIN_H )
#define     __HPLDBIN_H

#include <math.h>
#include "tobject.h"
#include "bitstr.h"
#include "errhndl.h"
#include "tvalues.h"
#include "binind.h"

//
//
//      Class BinHaploid is an enhancement of
//      class BitString of libg++ for GA applications.
//      The genetic operators on the BinHaploid class
//      are optimized using the specific functions provided
//      by class BitString.
//
//

class BinGene;
class BinHaploid : public BinIndividual
{
public:

    friend class BinGene;

    DECLARE_RTTI()

                                BinHaploid();
                                BinHaploid(unsigned,
                                           unsigned = ~RANDOMIZE,
                                           BOOL = FALSE);
                                BinHaploid(const BitString & ,
                                           BOOL = FALSE);
                                BinHaploid(const char *,
                                           BOOL = FALSE);
                                BinHaploid(RCBinHaploid);
				~BinHaploid();

                                _shallowCopy(BinHaploid)
    virtual void                deepenShallowCopy();

    virtual int                 compare(RCTObject ) const;
    virtual BOOL                isEqual( RCTObject ) const;
    virtual void                printOn( ostream  & ) const;

    virtual void                randomize();
    virtual void                mutate(float);

    virtual PTGAObj             oddPtCrossover(RCTGAObj, int) const;
    virtual void                oddPtCrossover(RCTGAObj, int, RTGAObj) const;
    virtual PTGAObj             evenPtCrossover(RCTGAObj, int) const;
    virtual void                evenPtCrossover(RCTGAObj, int, RTGAObj) const;
    virtual PTGAObj             uniformCrossover(RCTGAObj, float) const;
    virtual void                uniformCrossover(RCTGAObj, float,
                                                 RTGAObj) const;

    virtual BinGene             operator[](unsigned);
    virtual unsigned            operator[](unsigned) const;

    virtual RBinHaploid         operator=(const char  *);
    virtual RBinIndividual      operator=(RCBinDiploid);
    virtual RBinIndividual      operator=(RCBinHaploid);
    virtual RBinIndividual      operator=(RCBitString);

    virtual RBinHaploid         operator=(unsigned long);
    virtual                     operator RCBitString() const;

    virtual void                assign(unsigned, unsigned);
    virtual void                invert();
    virtual void                invert(int);
    virtual void                set();
    virtual void                set(int);
    virtual void                set(int, int);
    virtual void                clear();
    virtual void                clear(int);
    virtual void                clear(int, int);
    virtual int                 length() const;
    virtual void                resize(unsigned);
    virtual float               asFloat(int,int) const;
    virtual unsigned long       asLong(int,int) const;
    virtual float               asFloat() const;
    virtual unsigned long       asLong() const;

    virtual BOOL                isGrayCode() const;
    virtual void                isGrayCode(BOOL);

    virtual unsigned            hammingDistance(RCBitString) const;
    virtual double              distance(RCTGAObj) const;

protected:

    BitString   bits;
    BOOL        graycodeFlag;     // TRUE if the bits represent Gray Code
};

class BinGene
{
public:
        BinGene(RBinHaploid src, unsigned uIndex);
        BinGene(const BinGene& src);
        BinGene& operator=(unsigned uVal);
        operator    unsigned();

private:
    RBinHaploid    ind;
    const unsigned uOffset;
};

inline  BinHaploid::BinHaploid() : graycodeFlag(FALSE)
{
}

inline  BinHaploid::BinHaploid(RCBinHaploid src) :
		    BinIndividual(src),
                    graycodeFlag(src.graycodeFlag),
                    bits(src.bits)
{
        //
        //  an internal compiler error was found for my g++
        //  when compiling dpldbin.cc if this constructor
        //  is not defined
}

inline  BinHaploid::~BinHaploid()
{
}

inline  void BinHaploid::deepenShallowCopy()
{
}

inline  unsigned BinHaploid::operator[](unsigned uIndex) const
{
	return bits.test(uIndex);
}

inline	BinGene BinHaploid::operator[](unsigned uIndex)
{
	return BinGene(*this, uIndex);
}

inline  RBinHaploid  BinHaploid::operator=(const char  *pString)
{
	bits = atoBitString(pString);
	return *this;
}

inline  RBinIndividual BinHaploid::operator=(RCBitString src)
{
        bits = src;
	return *this;
}

inline  void    BinHaploid::assign(unsigned uIndex, unsigned uVal)
{
	if (uVal)
	    bits.set(uIndex);
	else
	    bits.clear(uIndex);
}

inline  void    BinHaploid::invert()
{
	~bits;
}

inline  void    BinHaploid::invert(int nIndex)
{
        bits.invert(nIndex);
}

inline  void    BinHaploid::set()
{
        bits.set();
}

inline  void    BinHaploid::set(int nIndex)
{
        bits.set(nIndex);
}

inline  void    BinHaploid::set(int nFrom, int nTo)
{
        bits.set(nFrom, nTo);
}

inline  void    BinHaploid::clear()
{
        bits.clear();
}

inline  void    BinHaploid::clear(int nIndex)
{
        bits.clear(nIndex);
}

inline  void    BinHaploid::clear(int nFrom, int nTo)
{
	bits.clear(nFrom, nTo);
}

inline  int     BinHaploid::length() const
{
        return bits.length();
}

inline  void    BinHaploid::resize(unsigned uNewLen)
{
        bits.resize(rangeCheck(uNewLen,1,MAX_INDLEN,DEFAULT_INDLEN));
}

inline  float  BinHaploid::asFloat() const
{
        return asFloat(0, bits.length());
}

inline  unsigned long  BinHaploid::asLong() const
{
        if (bits.length() <= LONGBITS)
            return asLong(0, bits.length());
        else
            return asLong(0, LONGBITS);
}

inline  BinHaploid::operator RCBitString() const
{
	return bits;
}

inline  BOOL  BinHaploid::isGrayCode() const
{
	return graycodeFlag;
}

inline  void  BinHaploid::isGrayCode(BOOL flag)
{
        graycodeFlag = flag;
}

inline  BinGene::BinGene(RBinHaploid src, unsigned uIndex) :
        ind(src), uOffset(uIndex)
{
}

inline  BinGene::BinGene(const BinGene& src) :
        ind(src.ind), uOffset(src.uOffset)
{
}

inline  BinGene&  BinGene::operator=(unsigned uVal)
{
        ind.assign(uOffset, uVal);
	return *this;
}

inline  BinGene::operator  unsigned()
{
	return ind.bits.test(uOffset);
}

inline BOOL BinHaploid::isEqual( RCTObject obj ) const
{
       if ( obj.isKindOf(this->isA()) ) {
           if ( ((RCBinHaploid) obj).graycodeFlag == graycodeFlag )
                return ((RCBinHaploid) obj).bits == bits;
           else
                return ((RCBinHaploid) obj).asLong() == asLong();
      }
       else
           return FALSE;
}

#endif

