//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#if !defined ( __TSP_H )
#define __TSP_H

#include "tobject.h"
_CLASSDEF(TSPind)

#include "SIntVec.h"
#include "edge.h"

extern char *xoverNames[];

class TSPind : public TGAObj
{
public:

	DECLARE_RTTI()

	TSPind(unsigned uLen,
	       BOOL uFlag = FALSE);

	TSPind(ShortVec & src) : ints(src)
	{
	}

	TSPind(RCTSPind src) :
	    ints(src.ints)
	{
	    objValue() = src.objValue();
	    fitness() = src.fitness();
	}

	virtual ~TSPind() {}

	virtual  PTObject shallowCopy() const
	{
		return (PTObject) new TSPind(*this);
	}

	virtual void    deepenShallowCopy() {}
	virtual void    printOn( ostream  & out) const;
	virtual int     compare(RCTObject obj) const;

    //
	virtual void     randomize();
	virtual void     mutate(float);
	virtual void     phenotype(PHENOTYPEFUNC pFunc)
	{
		objValue() = pFunc(*this);
		fitness() = 1 / objValue();
	}

	virtual    short&  operator[](unsigned uIndex)
	{
		return ints.elem(uIndex);
	}

	virtual    short&  operator[](unsigned uIndex) const
	{
		return ints[uIndex];
	}

    //      assignment operators
	virtual    RTGAObj  operator=(RCTGAObj);
	virtual    RTGAObj  operator=(const ShortVec& src)
	{
	    ints = src;
	    return *this;
	}
	virtual     int     length() const { return ints.capacity(); }
	virtual     void    resize(unsigned uNewLen)
	    {
		    ints.resize(rangeCheck(uNewLen,1,MAX_INDLEN,DEFAULT_INDLEN));
	    }

	virtual     operator        const ShortVec & () const { return ints; }
	//
	virtual PTGAObj  oddPtCrossover(RCTGAObj, int) const;
	virtual void     oddPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual PTGAObj  evenPtCrossover(RCTGAObj, int) const;
	virtual void     evenPtCrossover(RCTGAObj, int, RTGAObj) const;
	virtual PTGAObj  uniformCrossover(RCTGAObj, float) const;
	virtual void     uniformCrossover(RCTGAObj, float, RTGAObj) const;

	virtual BOOL	isEqual( RCTObject ) const;

	virtual	BOOL	legalTour();

	static	long	 crossTotal;
	static  BOOL	 printFlag;
	static  double   fraction;

	static	long	orOptTotal;
	static	long	twoOptTotal;

	void	edge(RCTGAObj, RTGAObj) const;

	virtual	double distance(RCTGAObj obj) const
	{
		return objValue() - obj.objValue();
	}

	void	optimizeLocal(double = 0);

	void	orOpt();
	void	twoOpt();
        void    swapOpt();

	void	debug(const char *);

protected:

	TSPind() {}

	ShortVec ints;

	void	reverse(int,int);
	int	testSwap(int,int);
	int     _orOpt(int,int=3);
};

extern	EdgeMap	edgeMap;

#endif

