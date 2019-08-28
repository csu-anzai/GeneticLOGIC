//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __XOVER_H )
#define __XOVER_H

#include <iostream.h>
#include "tobject.h"
#include "errhndl.h"
#include "IntVec.h"
#include "trand.h"

int             bitCompare(int , int );

class   Crossover : public TObject
{
public:
				DECLARE_RTTI()

        virtual                 ~Crossover();
        virtual BOOL            isEqual( RCTObject ) const;
        virtual int             compare( RCTObject ) const;
        virtual void            printOn( ostream  & out) const;

        virtual PTObject        shallowCopy() const = 0;
        virtual void            deepenShallowCopy();

        virtual double          rate() const;
        virtual double          rate(double);
        virtual void            setMate(PCTGAObj, PCTGAObj);
        virtual PTGAObj         create() = 0;
        virtual void            transform(RTGAObj) = 0;

protected:

        double          flXRate;
	PCTGAObj        pMother;
	PCTGAObj        pFather;

                                Crossover(double flRate =DEFAULT_XOVERATE);
                                Crossover(RCCrossover);
        virtual BOOL            flip() const;
};

//
//      In case when pMother is not a kind of pFather, e.g. (pMother is Diploid
//      and pFather is Haploid, function create will always return a child
//      of the type of pMother
//

class  PtCrossover : public Crossover
{
public:
				DECLARE_RTTI()

        virtual void            printOn( ostream  & out) const;
        virtual PTObject        shallowCopy() const = 0;

        virtual PTGAObj         create() = 0;
        virtual void            transform(RTGAObj) = 0;
        virtual unsigned        xpts(unsigned) = 0;
        virtual unsigned        xpts() const = 0;
};

class  OddPtCrossover : public Crossover
{
public:
				DECLARE_RTTI()

                                OddPtCrossover(unsigned nPoints=1,
                                               double flRate=DEFAULT_XOVERATE);
                                OddPtCrossover(RCOddPtCrossover);
                                _shallowCopy(OddPtCrossover)

        virtual PTGAObj         create();
        virtual void            transform(RTGAObj);
        virtual unsigned        xpts() const;
        virtual unsigned        xpts(unsigned);

protected :
	int nXpts;
};

class  EvenPtCrossover : public Crossover
{
public:
				DECLARE_RTTI()

				EvenPtCrossover(int nPoints = 2,
					       double flRate=DEFAULT_XOVERATE);
				EvenPtCrossover(RCEvenPtCrossover);
                                _shallowCopy(EvenPtCrossover)

        virtual PTGAObj         create();
        virtual void            transform(RTGAObj);
        virtual unsigned        xpts(unsigned);
        virtual unsigned        xpts() const;
        virtual void            setMate(PCTGAObj, PCTGAObj);

protected:
	int nXpts;
};

class  MultiPtCrossover : public Crossover
{
public:
				DECLARE_RTTI()

				MultiPtCrossover(unsigned uPoints = 2,
					       double flRate=DEFAULT_XOVERATE);
				MultiPtCrossover(RCMultiPtCrossover);
				~MultiPtCrossover();

	virtual BOOL            isEqual( RCTObject ) const;
	virtual int             compare( RCTObject ) const;
	virtual void            printOn( ostream  & out) const;

				_shallowCopy(MultiPtCrossover)
	virtual void            deepenShallowCopy();

	virtual unsigned        xpts() const;
        virtual unsigned        xpts(unsigned);

        virtual PTGAObj         create();
        virtual void            transform(RTGAObj);
        virtual void            setMate(PCTGAObj , PCTGAObj );

protected:
        PPtCrossover      pXOver;
};

class   UniformCrossover : public Crossover
{
public:
				DECLARE_RTTI()

				UniformCrossover(double flRate =
							DEFAULT_XOVERATE);
				UniformCrossover(RCUniformCrossover);

				_shallowCopy(UniformCrossover)

	virtual PTGAObj         create();
	virtual void            transform(RTGAObj);
};

inline  Crossover::Crossover(double flRate) :
			     pMother(NULL), pFather(NULL)
{
	rate(flRate);
}

inline  Crossover::Crossover(RCCrossover src) :
			     pMother(src.pMother), pFather(src.pFather),
			     flXRate(src.flXRate)
{
}

inline  Crossover::~Crossover()
{
}

inline BOOL Crossover::isEqual( RCTObject obj ) const
{
	return compare(obj) == 0;
}

inline void  Crossover::deepenShallowCopy()
{
}

inline  void    Crossover::printOn( ostream  & out) const
{
	out << nameOf() << " : " << rate();
}

inline  double  Crossover::rate() const
{
	return flXRate;
}

inline  double Crossover::rate(double flNewRate)
{
	double  flOldRate = flXRate;
	flXRate = rangeCheck(flNewRate,0.0,1.0,DEFAULT_XOVERATE);
	return flOldRate;
}

inline  void   Crossover::setMate(PCTGAObj mother, PCTGAObj father)
{
	if ((mother->length() < 1) || (father->length() < 1))
	   lib_error_handler("Crossover", "setMate , length < 1");
	else {
           pMother = mother;
           pFather = father;
	}
}

inline  BOOL   Crossover::flip() const
{
	return tRand.flip(flXRate);
}

inline  void    PtCrossover::printOn( ostream  & out) const
{
	Crossover::printOn(out);
	out << " (" << xpts() << ')';
}

inline  OddPtCrossover::OddPtCrossover(unsigned nPoints,
				       double flRate) :
				       Crossover(flRate)
{
	xpts(nPoints);
}

inline  OddPtCrossover::OddPtCrossover(RCOddPtCrossover src) :
		Crossover(src), nXpts(src.nXpts)
{
}

inline  unsigned OddPtCrossover::xpts() const
{
	return nXpts;
}

inline  unsigned OddPtCrossover::xpts(unsigned uNewPts)
{
	unsigned uOldPts = nXpts;
	nXpts = uNewPts;
	if (nXpts % 2 == 0)
	    nXpts--;
	if (nXpts < 1)
	    nXpts = 1;
	return uOldPts;
}

inline  EvenPtCrossover::EvenPtCrossover(int nPoints,
					 double flRate) :
					 Crossover(flRate)
{
	xpts(nPoints);
}

inline  EvenPtCrossover::EvenPtCrossover(RCEvenPtCrossover src) :
           Crossover(src), nXpts(src.nXpts)
{
}

inline  unsigned EvenPtCrossover::xpts(unsigned uNewPts)
{
	unsigned uOldPts = nXpts;
	nXpts = uNewPts;
	if (nXpts % 2 != 0)
	    nXpts--;
	if (nXpts < 2)
	    nXpts = 2;
	return uOldPts;
}

inline  unsigned EvenPtCrossover::xpts() const
{
	return nXpts;
}

inline  void EvenPtCrossover::setMate(PCTGAObj mother, PCTGAObj father)
{
	if ((mother->length() < nXpts) || (father->length() < nXpts))
	   lib_error_handler("EvenCrossover", "setMate , length < nXpts");
	else {
           pMother = mother;
           pFather = father;
	}
}

inline  MultiPtCrossover::MultiPtCrossover(RCMultiPtCrossover src) :
	pXOver(src.pXOver)
{
}

inline  MultiPtCrossover::~MultiPtCrossover()
{
	delete pXOver;
}

inline  BOOL     MultiPtCrossover::isEqual( RCTObject obj ) const
{
	return pXOver->isEqual(obj);
}

inline  int      MultiPtCrossover::compare( RCTObject obj ) const
{
	return pXOver->compare(obj);
}

inline  void     MultiPtCrossover::printOn( ostream  & out) const
{
	pXOver->printOn(out);
}

inline  void     MultiPtCrossover::deepenShallowCopy()
{
	pXOver->deepenShallowCopy();
}

inline  unsigned MultiPtCrossover::xpts() const
{
	return pXOver->xpts();
}

inline  PTGAObj  MultiPtCrossover::create()
{
	return pXOver->create();
}

inline  void    MultiPtCrossover::transform(RTGAObj obj)
{
	pXOver->transform(obj);
}

inline  void    MultiPtCrossover::setMate(PCTGAObj mother, PCTGAObj father)
{
	pXOver->setMate(mother, father);
}

inline  UniformCrossover::UniformCrossover(double flRate) :
					   Crossover(flRate)
{
}

inline  UniformCrossover::UniformCrossover(RCUniformCrossover src) :
	Crossover(src)
{
}

#endif

