//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __URANDBITS_H )
#define        __URANDBITS_H

#include	"tdefs.h"
#include        "bitstr.h"

_CLASSDEF(UniformRandomBits)

class   UniformRandomBits
{
public:
	UniformRandomBits(sizeType popSize, int strlen);
	~UniformRandomBits();
	void randomize();
	const BitString & operator[](sizeType index) const;
	const BitString & elem(sizeType index) const;

	friend ostream & operator << ( ostream &, RCUniformRandomBits);
protected:
	sizeType popSize;
	PBitString* pBitStrings;

};

inline const BitString & UniformRandomBits::operator[](sizeType index) const
{
	return *pBitStrings[index >= popSize ? 0 : index];
}

inline const BitString & UniformRandomBits::elem(sizeType index) const
{
	return *pBitStrings[index];
}

#endif

