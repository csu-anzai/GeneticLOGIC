//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __DPLDBIN_H )
#define     __DPLDBIN_H

#include "tritstr.h"
#include "hpldbin.h"

//
//
//      Class BinDiploid provides facilities for experiments on
//      dominance and diploidy of binary chromosomes.
//
//

class BinDiploid : public BinIndividual
{
public:

    DECLARE_RTTI()
                                BinDiploid();
                                BinDiploid(unsigned,
                                           unsigned,
                                           BOOL = FALSE);
				BinDiploid(RCTritString,
					   RCTritString,
                                           BOOL = FALSE);
				~BinDiploid();

                                _shallowCopy(BinDiploid)
    virtual void                deepenShallowCopy();

    virtual BOOL                isEqual( RCTObject ) const;
    virtual int                 compare(RCTObject ) const;
    virtual int                 length() const;
    virtual void                printOn( ostream  & ) const;

    virtual void                resize(unsigned);

    virtual RBinIndividual      operator=(RCBinDiploid);
    virtual RBinIndividual      operator=(RCBinHaploid);
    virtual RBinIndividual      operator=(RCBitString);

    virtual                     operator RCBinHaploid() const;
    virtual                     operator RCBitString() const;

    virtual RCTritString        hmlgs1() const;
    virtual RCTritString        hmlgs2() const;
    virtual RCTritString        hmlgs1(RCTritString);
    virtual RCTritString        hmlgs2(RCTritString);

    virtual void                mutate(float);
    virtual void                randomize();
    virtual void                setRand(unsigned);

    virtual void                invert(int);
    virtual void                set(int);
    virtual void                clear(int);

    virtual PTGAObj             oddPtCrossover(RCTGAObj, int) const;
    virtual void                oddPtCrossover(RCTGAObj, int, RTGAObj) const;
    virtual PTGAObj             evenPtCrossover(RCTGAObj, int) const;
    virtual void                evenPtCrossover(RCTGAObj, int, RTGAObj) const;
    virtual PTGAObj             uniformCrossover(RCTGAObj, float) const;
    virtual void                uniformCrossover(RCTGAObj,float,RTGAObj) const;

    virtual float               asFloat() const;
    virtual unsigned long       asLong() const;
    virtual float               asFloat(int,int) const;
    virtual unsigned long       asLong(int,int) const;

    virtual BOOL                isGrayCode() const;
    virtual void                isGrayCode(BOOL flag);

    virtual unsigned            hammingDistance(RCBinDiploid) const;
    virtual unsigned            hammingDistance(RCBitString) const;
    virtual double              distance(RCTGAObj) const;

protected:

    TritString  hmlg1, hmlg2;   // the homologous chromosomes
    BinHaploid  expressed;      // the expressed phenotype of this object

    virtual void                mapDominanceAt(unsigned uIndex);
    virtual void                setDominance();
    virtual void                normalize();

    virtual PTGAObj             _oddPtCrossover(RCBinDiploid, int) const;
    virtual PTGAObj             _oddPtCrossover(RCBinHaploid, int) const;
    virtual void                _oddPtCrossover(RCBinDiploid, int,
						RBinDiploid) const;
    virtual void                _oddPtCrossover(RCBinHaploid, int,
						RBinDiploid) const;

    virtual PTGAObj             _evenPtCrossover(RCBinDiploid, int) const;
    virtual PTGAObj             _evenPtCrossover(RCBinHaploid, int) const;
    virtual void                _evenPtCrossover(RCBinDiploid, int,
                                                 RBinDiploid) const;
    virtual void                _evenPtCrossover(RCBinHaploid, int,
                                                 RBinDiploid) const;

    virtual PTGAObj             _uniformCrossover(RCBinDiploid, float) const;
    virtual PTGAObj             _uniformCrossover(RCBinHaploid, float) const;
    virtual void                _uniformCrossover(RCBinDiploid, float,
						  RBinDiploid) const;
    virtual void                _uniformCrossover(RCBinHaploid, float,
						  RBinDiploid) const;
};

inline  BinDiploid::BinDiploid()
{
}

inline  BinDiploid::~BinDiploid()
{
}

inline  void BinDiploid::deepenShallowCopy()
{
}

inline  int  BinDiploid::compare(RCTObject obj) const
{
	return expressed.compare(obj);
}

inline  RCTritString BinDiploid::hmlgs1() const
{
	return hmlg1;
}

inline  RCTritString BinDiploid::hmlgs2() const
{
	return hmlg2;
}

inline  RCTritString BinDiploid::hmlgs1(RCTritString src)
{
	hmlg1 = src;
	normalize();
	setDominance();
	return hmlg1;
}

inline  RCTritString BinDiploid::hmlgs2(RCTritString src)
{
	hmlg2 = src;
	normalize();
	setDominance();
	return hmlg2;
}

inline  void BinDiploid::setRand(unsigned uI)
{
	hmlg1.setRand(uI);
	hmlg2.setRand(uI);
	mapDominanceAt(uI);
}

inline  int BinDiploid::length() const
{
        return expressed.length();
}

inline  BinDiploid::operator RCBinHaploid() const
{
	return expressed;
}

inline  BinDiploid::operator RCBitString() const
{
        return expressed.operator RCBitString();
}

inline  float         BinDiploid::asFloat() const
{
	return expressed.asFloat();
}

inline  unsigned long  BinDiploid::asLong() const
{
	return expressed.asLong();
}

inline  float         BinDiploid::asFloat(int nFrom, int nLength) const
{
	return expressed.asFloat(nFrom, nLength);
}

inline  unsigned long  BinDiploid::asLong(int nFrom, int nLength) const
{
        return expressed.asLong(nFrom, nLength);
}

inline void BinDiploid::mapDominanceAt(unsigned uIndex)
{
        if (hmlg1.valueAt(uIndex) >= hmlg2.valueAt(uIndex))
            expressed.assign(uIndex, abs(hmlg1.valueAt(uIndex)));
        else
            expressed.assign(uIndex, abs(hmlg2.valueAt(uIndex)));
}

inline  BOOL  BinDiploid::isGrayCode() const
{
        return expressed.isGrayCode();
}

inline  void  BinDiploid::isGrayCode(BOOL flag)
{
	expressed.isGrayCode(flag);
}


#endif

