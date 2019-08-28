//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __ATOM_H )
#define        __ATOM_H

#include        "tobject.h"

class   AtomBase : public TObject
{
public:
                                DECLARE_RTTI()

        virtual                 ~AtomBase();

        virtual PTObject        shallowCopy() const = 0;
        virtual void            deepenShallowCopy() = 0;

        virtual int             compare( RCTObject ) const;
        virtual BOOL            isEqual( RCTObject ) const = 0;

        virtual void            mutate(const float) = 0;

        virtual RAtomBase       operator= (RCTChar) = 0;
        virtual RAtomBase       operator= (RCTLong) = 0;
        virtual RAtomBase       operator= (RCTInteger) = 0;
        virtual RAtomBase       operator= (RCTDouble) = 0;

        virtual void            printOn( ostream & ) const = 0;

protected:

	AtomBase();
};

class   Atom : public AtomBase
{
public:

    static const TypeInfo infoObj;

    static PCTypeInfo typeinfo()
    {
	   return &infoObj;
    }

    virtual RCTypeInfo getTypeInfo() const;
    virtual PCTypeInfo          isA() const;

				Atom();
				Atom(RCAtom);
				Atom(RCAtomBase);
				Atom(const long);
				Atom(const double);
				Atom(const char);
				~Atom();

    virtual const char*         nameOf() const;
    virtual BOOL                isEqual( RCTObject ) const;

				_shallowCopy(Atom)
    virtual void                deepenShallowCopy();

    virtual void                mutate(const float);

    virtual RAtomBase           operator= (RCAtom);
    virtual RAtomBase           operator= (RCTChar);
    virtual RAtomBase           operator= (RCTLong);
    virtual RAtomBase           operator= (RCTInteger);
    virtual RAtomBase           operator= (RCTDouble);

    virtual void                printOn( ostream & ) const;

protected:
	PAtomBase   pRep;
};

class   TChar : public AtomBase
{
public:

				DECLARE_RTTI()

				TChar(char=0);

    virtual BOOL                isEqual( RCTObject ) const;
    virtual void                printOn( ostream & ) const;

				_shallowCopy(TChar)

    virtual void                deepenShallowCopy();
    virtual void                mutate(const float);

    virtual RAtomBase           operator= (RCTChar);
    virtual RAtomBase           operator= (RCTLong);
    virtual RAtomBase           operator= (RCTInteger);
    virtual RAtomBase           operator= (RCTDouble);

    virtual RAtomBase           operator= (char);
    virtual                     operator char() const;

protected:
    char        chData;
};

class   TInteger : public AtomBase
{
public:

				DECLARE_RTTI()

				TInteger(int=0);

    virtual BOOL                isEqual( RCTObject ) const;
    virtual void                printOn( ostream& ) const;
    virtual void                deepenShallowCopy();
				_shallowCopy(TInteger)

    virtual void                mutate(const float);

    virtual RAtomBase           operator= (int);

    virtual RAtomBase           operator= (RCTChar);
    virtual RAtomBase           operator= (RCTLong);
    virtual RAtomBase           operator= (RCTInteger);
    virtual RAtomBase           operator= (RCTDouble);

    virtual                     operator int() const;

protected:
    int nData;
};

class   TLong : public AtomBase
{
public:

				DECLARE_RTTI()

				TLong(long=0);

    virtual BOOL                isEqual( RCTObject ) const;
    virtual void                printOn( ostream& ) const;
    virtual void                deepenShallowCopy();
				_shallowCopy(TLong)

    virtual void                mutate(const float);

    virtual RAtomBase           operator= (RCTChar);
    virtual RAtomBase           operator= (RCTLong);
    virtual RAtomBase           operator= (RCTInteger);
    virtual RAtomBase           operator= (RCTDouble);
    virtual RAtomBase           operator= (long);

    virtual                     operator long() const;

protected:
    long lData;
};

class   TDouble : public AtomBase
{
public:

				DECLARE_RTTI()

				TDouble(double=0);

    virtual BOOL                isEqual( RCTObject ) const;
    virtual void                printOn( ostream& ) const;
    virtual void                deepenShallowCopy();
				_shallowCopy(TDouble)
    virtual void                mutate(const float);

    virtual RAtomBase           operator= (RCTChar);
    virtual RAtomBase           operator= (RCTLong);
    virtual RAtomBase           operator= (RCTInteger);
    virtual RAtomBase           operator= (RCTDouble);
    virtual RAtomBase           operator= (double);

    virtual                     operator double() const;

protected:
    double  flData;
};

inline  AtomBase::AtomBase()
{
}

inline  AtomBase::~AtomBase()
{
}

inline  int     AtomBase::compare( RCTObject obj) const
{
	PRECONDITION(isKindOf(obj.isA()));
        return (hashValue() - obj.hashValue());
}

inline	Atom::Atom()
{
	pRep = (PAtomBase) new TInteger();
}

inline  Atom::Atom(const double flVal)
{
        pRep = (PAtomBase) new TDouble(flVal);
}

inline  Atom::Atom(const long lVal)
{
	if (lVal < MAXINT)
	    pRep = (PAtomBase) new TInteger(lVal);
	else
	    pRep = (PAtomBase) new TLong(lVal);
}

inline  Atom::Atom(const char chVal)
{
        pRep = (PAtomBase) new TChar(chVal);
}

inline  Atom::Atom(RCAtomBase src)
{
        pRep = (PAtomBase) src.deepCopy();
}

inline  Atom::Atom(RCAtom src)
{
        pRep = (PAtomBase) src.pRep->deepCopy();
}

inline  RCTypeInfo Atom::getTypeInfo() const
{
	return infoObj;
}

inline  Atom::~Atom()
{
        delete pRep;
}

inline  PCTypeInfo Atom::isA() const
{
	return pRep->isA();
}

inline  const char* Atom::nameOf() const
{
	return pRep->nameOf();
}

inline  BOOL Atom::isEqual( RCTObject  obj ) const
{
	return pRep->isEqual(obj);
}

inline  void Atom::printOn( ostream  & out) const
{
	pRep->printOn(out);
}

inline  void Atom::deepenShallowCopy()
{
}

inline  void Atom::mutate(const float flRate)
{
	pRep->mutate(flRate);
}

inline  RAtomBase Atom::operator= (RCTChar ch)
{
	pRep->operator=(ch);
	return *this;
}

inline  RAtomBase Atom::operator= (RCTInteger val)
{
	pRep->operator=(val);
	return *this;
}

inline  RAtomBase Atom::operator= (RCTLong val)
{
	pRep->operator=(val);
	return *this;
}

inline  RAtomBase Atom::operator= (RCTDouble val)
{
	pRep->operator=(val);
	return *this;
}

inline  RAtomBase Atom::operator= (RCAtom src)
{
	if (! isSame(src)) {
	    delete pRep;
	    pRep = (PAtomBase) src.pRep->deepCopy();
	}
	return *this;
}

inline  TChar::TChar(char val) : chData(val)
{
}

inline  void TChar::printOn( ostream  & out) const
{
	 out << chData;
}

inline  void  TChar::deepenShallowCopy()
{
}


inline  RAtomBase TChar::operator= (char ch)
{
	chData = ch;
	return *this;
}


inline  RAtomBase TChar::operator= (RCTChar src)
{
	chData = src.chData;
	return *this;
}

inline  RAtomBase TChar::operator= (RCTLong val)
{
	chData = val.operator long();
	return *this;
}

inline  RAtomBase TChar::operator= (RCTDouble val)
{
	chData = val.operator double();
	return *this;
}

inline  RAtomBase TChar::operator= (RCTInteger val)
{
	chData = val.operator int();
	return *this;
}

inline  TChar::operator char() const
{
	return chData;
}

inline  TInteger::TInteger(int val) : nData(val)
{
}

inline  void TInteger::printOn( ostream  & out) const
{
	out << nData;
}

inline  void TInteger::deepenShallowCopy()
{
}

inline  RAtomBase TInteger::operator= (RCTInteger src)
{
	nData = src.nData;
	return *this;
}

inline  RAtomBase TInteger::operator= (RCTLong val)
{
	nData = val.operator long();
	return *this;
}

inline  RAtomBase TInteger::operator= (RCTDouble val)
{
	nData = val.operator double();
	return *this;
}

inline  RAtomBase TInteger::operator= (RCTChar val)
{
	nData = val.operator char();
	return *this;
}

inline  RAtomBase TInteger::operator= (int val)
{
	nData = val;
	return *this;
}

inline  TInteger::operator int() const
{
	return nData;
}

inline  TDouble::TDouble(double val) : flData(val)
{
}

inline  void TDouble::printOn( ostream  & out) const
{
	out << flData;
}

inline  void TDouble::deepenShallowCopy()
{
}

inline  RAtomBase TDouble::operator= (RCTLong val)
{
	flData = val.operator long();
	return *this;
}

inline  RAtomBase TDouble::operator= (RCTInteger val)
{
	flData = val.operator int();
	return *this;
}

inline  RAtomBase TDouble::operator= (RCTDouble src)
{
	flData = src.flData;
	return *this;
}

inline  RAtomBase TDouble::operator= (RCTChar val)
{
	flData = val.operator char();
	return *this;
}


inline  RAtomBase TDouble::operator= (double val)
{
	flData = val;
	return *this;
}

inline  TDouble::operator double() const
{
	return flData;
}

inline  TLong::TLong(long val) : lData(val)
{
}

inline  void TLong::printOn( ostream  & out) const
{
	out << lData;
}

inline  void TLong::deepenShallowCopy()
{
}

inline  RAtomBase TLong::operator= (long val)
{
	lData = val;
	return *this;
}

inline  TLong::operator long() const
{
	return lData;
}

inline  RAtomBase TLong::operator= (RCTLong src)
{
	lData = src.lData;
	return *this;
}

inline  RAtomBase TLong::operator= (RCTInteger val)
{
	lData = val.operator int();
	return *this;
}

inline  RAtomBase TLong::operator= (RCTDouble val)
{
	lData = val.operator double();
	return *this;
}

inline  RAtomBase TLong::operator= (RCTChar val)
{
	lData = val.operator char();
	return *this;
}

#endif

