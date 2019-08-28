//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#include "scale.h"
#include "hpldbin.h"

#ifdef M_PI
#define PI M_PI
#endif

#ifndef PI
#define PI 3.14159265358979323846
#endif

const   double FOUR_PI = 4 * PI;
const   double prec1 = 1 / 1023.0;
const   double prec2 = 1.0 / 1048575L;    // 1 / (2 ** 20 - 1)
const   double prec3 = 1.0 / 1073741823L; // 1 / (2 ** 30 - 1)

double  twopeaks(RCTGAObj);
double  hmb(RCTGAObj);
double  evenpeaks(RCTGAObj);
double  unevenpeaks(RCTGAObj);
double  unevendecpeaks(RCTGAObj);
double  twopeaktrap(RCTGAObj);
double  unevencos(RCTGAObj);

_CLASSDEF(HmbInd)
_CLASSDEF(SinInd)

class	SinInd : public BinHaploid
//
// 	thirty bits to represent values in [0.0, 1.0]
//
{
public:
	SinInd() : BinHaploid(30) {}
	SinInd(RCSinInd src) : BinHaploid(src)
	{
	}
	SinInd(RCBitString bits) : BinHaploid(bits)
	{
		resize(30);
	}

	_shallowCopy(SinInd) 

        virtual void printOn(ostream& out) const
	{
		out << asLong() * prec3;
	}
};

class   HmbInd : public BinHaploid
{
public :
	HmbInd() : BinHaploid(40) {}
	HmbInd(RCHmbInd src) : BinHaploid(src)
	{
	}
	HmbInd(RCBitString bits) : BinHaploid(bits)
	{
		resize(40);
	}

	_shallowCopy(HmbInd) 

	virtual hashValueType hashValue() const
	{
		(hashValueType) this;
	}

        double x1() const { return 5 - asFloat(0,20) * prec2 * 10; }
	double x2() const { return 5 - asFloat(20,20) * prec2 * 10; }
        virtual void printOn(ostream& out) const
          {
                out << x1() << ' ' << x2();
          }
};

