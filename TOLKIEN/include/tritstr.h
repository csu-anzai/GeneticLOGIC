//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __TRITSTR_H )
#define   __TRITSTR_H

#include "tobject.h"
#include "bitstr.h"

class	Trit;

class TritSubString
{
  friend class      TritString;

protected:

	TritString & tstr;
	int	     pos;
	int	     len;

		    TritSubString(RTritString x, int p, int l);
		    TritSubString(RCTritString x, int p, int l);
		    TritSubString(const TritSubString& x);

public:
                    ~TritSubString();

  void              operator =  (RCTritString);
  void              operator =  (const TritSubString&);

  int               length() const;
  int               empty() const;
  int               OK() const;
};

class   TritString : public TObject
{
//      a string of ternary digit
public:

    DECLARE_RTTI()

				TritString();
				TritString(RCBitString);
                                TritString(RCTritString);
				TritString(RCBitString, RCBitString);
				TritString(const TritSubString &);
				TritString(const char *);
                                TritString(unsigned, BOOL=~RANDOMIZE);
                                ~TritString();

    virtual BOOL                isEqual( RCTObject ) const;
                                _shallowCopy(TritString)
    virtual void                deepenShallowCopy();

    virtual void                printOn( ostream  & ) const;
    virtual unsigned            length() const;

    virtual void                setRand(unsigned);
    virtual void                randomize();
    virtual unsigned            mutate(float);

    virtual int                 compare(const TObject&) const;
    virtual RTritString         operator=(RCTritString);
    virtual RTritString         operator=(const TritSubString &);
    virtual RTritString         operator=(RCBitString);
    virtual RTritString         operator=(const char *);
    virtual RTritString         operator=(unsigned int);
    virtual operator            BitString() const;
    virtual void                operator += (RCTritString);
    virtual BOOL                matches(RCBitString) const;
    virtual BOOL                contains(RCBitString) const;
    virtual unsigned            findDontCare() const;
    virtual void                clear();
    virtual void                set();
    virtual Trit                operator[](unsigned);
    virtual void                resize(unsigned);
    virtual void                assign(unsigned, RCTritString);
    virtual void                setAt(unsigned,int);
    virtual int                 valueAt(unsigned) const;
    virtual TritSubString       at(int, int) const;
    virtual TritSubString       before(int) const;
    virtual BitString           filter(RCBitString) const;
    virtual void                oddPtCrossover(RCTritString, RCTritString, int);
    virtual void                evenPtCrossover(RCTritString, RCTritString, int);
    virtual void                uniformCrossover(RCTritString, RCTritString,
						 float);

    virtual float               asFloat(int,int) const;
    virtual unsigned long       asLong(int,int) const;
    virtual float               asFloat() const;
    virtual unsigned long       asLong() const;

    virtual unsigned            matchCount(RCTritString src) const;

	    float               mutateDC() const;
	    void                mutateDC(float);

    // friends
    friend TritString           shorttoTritString(unsigned short);
    friend TritString           longtoTritString(unsigned long);

    friend int                  operator == (RCTritString, RCTritString);
    friend int                  operator != (RCTritString, RCTritString);

protected:

    BitString   bits;
    BitString   dcbits;

    float	flMutDC;

    friend class TritSubString;
    friend class Trit;

    friend TritString  operator + (RCTritString, RCTritString);
};

class   Trit 	// a ternary digit; 0=0, 1=1, -1=#
{
public:
	Trit(RTritString, unsigned);
	Trit(const Trit & );
	Trit& operator=(int);
	operator int();

private:
	RTritString    tstring;
	unsigned       uOffset;
};


inline  Trit::Trit(RTritString tstr, unsigned uAt) :
	tstring(tstr), uOffset(uAt)
{
}

inline  Trit::Trit(const Trit & from) :
	tstring(from.tstring), uOffset(from.uOffset)
{
}

inline  Trit::operator int()
{
	return tstring.valueAt(uOffset);
}

inline TritSubString::TritSubString(const TritSubString& x) :
	tstr(x.tstr), pos(x.pos), len(x.len)
{
}

inline TritSubString::TritSubString(RTritString x, int p, int l) :
	tstr(x), pos(p), len(l)
{
}

inline TritSubString::TritSubString(RCTritString x, int p, int l) :
	tstr((RTritString) x), pos(p), len(l)
{
}

inline  TritSubString::~TritSubString()
{
}

inline  int  TritSubString::length() const
{
	return len;
}

inline  int  TritSubString::empty() const
{
	return len == 0;
}

inline TritString::TritString() : flMutDC(DEFAULT_MUTDC)
{
}

inline TritString::TritString(RCBitString bstr) :
                bits(bstr), flMutDC(DEFAULT_MUTDC)
{
	dcbits.resize(bits.length());
	dcbits.clear();
}

inline TritString::TritString(RCTritString src) :
                bits(src.bits), dcbits(src.dcbits), flMutDC(src.flMutDC)
{
        //
        //  an internal compiler error was found for my g++
        //  when compiling dpldbin.cc if this constructor
        //  is not defined
}

inline TritString::TritString(RCBitString x, RCBitString y) :
        bits(x), dcbits(y), flMutDC(DEFAULT_MUTDC)
{
}

inline  TritString::~TritString()
{
}

inline  Trit    TritString::operator[](unsigned uAt)
{
	return Trit(*this, uAt);
}

inline void TritString::deepenShallowCopy()
{
}

inline unsigned TritString::length() const
{
    return bits.length();
}

inline RTritString TritString::operator=(unsigned int value)
{
    *this = shorttoTritString(value);
    return *this;
}

inline unsigned TritString::findDontCare() const
{
	return dcbits.count();
}

inline void TritString::clear()
{
    bits.clear();
    dcbits.clear();
}

inline void TritString::set()
{
    bits.set();
    dcbits.set();
}

inline void TritString::resize(unsigned uNewSize)
{
        bits.resize(uNewSize);
        dcbits.resize(uNewSize);
}

inline void TritString::assign(unsigned nAt, RCTritString src)
{
    bits.assign(nAt, src.bits.test(nAt));
    dcbits.assign(nAt, src.dcbits.test(nAt));
}

inline int TritString::valueAt(unsigned uAt) const
{
	return dcbits.test(uAt) ?
		(int) dontCare : bits.test(uAt);
}

inline float TritString::mutateDC() const
{
	return flMutDC;
}

inline void TritString::mutateDC(float newMutDC)
{
	flMutDC = (float) rangeCheck(newMutDC, 0.0, 1.0, DEFAULT_MUTDC);
}

inline unsigned long TritString::asLong(int nFrom, int nLength) const
{
        return (unsigned long) asFloat(nFrom, nLength);
}

inline float TritString::asFloat() const
{
        return asFloat(0, length());
}

inline unsigned long TritString::asLong() const
{
        return asFloat(0, length());
}

#endif

