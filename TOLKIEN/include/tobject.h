//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TOBJECT_H )
#define __TOBJECT_H

#include "tdefs.h"
#include <string.h>
#include <iostream.h>
#include <assert.h>
#include <values.h>
#include "trand.h"
#include "errhndl.h"

typedef int	(* COMPAREFUNC)(const void *, const void *);
typedef double  (*PHENOTYPEFUNC)(RCTGAObj); // typedef for phenotype function

extern  int     compare_ob(const void *, const void *);
extern  int     decFitness(const void *pMem1, const void *pMem2);
extern  int     ascFitness(const void *pMem1, const void *pMem2);
extern  int     decObjValue(const void *pMem1, const void *pMem2);
extern  int     ascObjValue(const void *pMem1, const void *pMem2);

#define DECLARE_RTTI() \
	static const TypeInfo infoObj; \
        static PCTypeInfo typeInfo() { return &infoObj; } \
        virtual RCTypeInfo getTypeInfo() const { return infoObj; }

#define staticTypeInfo(T)       T::info()
#define ptrTypeInfo(p)          ((p)->getInfo())
#define refTypeInfo(p)          ((r).getInfo())
#define ptrCast(T,p) \
	(T::info()->canCast((p)->getInfo()) ? (T*)(p) : 0)
#define refCast(T,r) \
	(T::info()->canCast((r).getInfo()) ? (T&)(r) : 0)

static struct DontCare
{
public:
	operator char() const;
	operator int()  const;
} dontCare;

class   TypeInfo
{
	const char* n;       // class name
	PCTypeInfo *b;       // list of bases

public:
	TypeInfo(const char *name, PCTypeInfo bases[]) :
		n(name), b(bases)
	{
	}

        const char * name() const
	{
		return n;
	}

	int same (PCTypeInfo p) const
	{
		return this == p || strcmp(n, p->n) == 0;
	}

	int canCast(PCTypeInfo p) const
	{
		return same(p) || p->hasBase(this);
	}

	int hasBase(PCTypeInfo) const;
};

class   TObject
{
public:

    static const TypeInfo 	infoObj;
    static  PCTypeInfo 		typeInfo();
    virtual RCTypeInfo 		getTypeInfo() const;

    virtual                     ~TObject();

    static  PTObject            nil;    // pointer to error object

    virtual PCTypeInfo 		isA() const;
    virtual BOOL 		isKindOf(PCTypeInfo pTypeInfo) const;
    virtual const char  *	nameOf() const;

    virtual hashValueType 	hashValue() const;
    virtual void 		printOn( ostream  & out) const = 0;
    virtual int 		compare( RCTObject ) const = 0;
    virtual BOOL                isEqual( RCTObject ) const;
    virtual BOOL 		isSame( RCTObject ) const;
    virtual void 		error(const char  *msg) const;
				PTObject deepCopy() const;
				// copy with distinct instance variables
    virtual PTObject 		copy() const;
				// copy defaulted as shallowCopy
    virtual PTObject 		shallowCopy() const = 0;
    virtual void 		deepenShallowCopy() = 0;
				// convert shallow copy to deep copy

    friend ostream & operator << ( ostream &, RCTObject );
};

#define NOOBJECT  (TObject::nil)

class TError : public TObject
{

public:

    static const TypeInfo 	infoObj;
    static  PCTypeInfo 		typeInfo();
    virtual RCTypeInfo 		getTypeInfo() const;

    virtual hashValueType hashValue() const
    {
	return MAXLONG;
    }

    virtual int isEqual( RCTObject ) const
    {
        return 1;
    }

    virtual void printOn( ostream & ) const;

    void operator delete( void * );

    virtual int compare( RCTObject ) const
    {
	return 1;
    }

    virtual PTObject shallowCopy() const
    {
	return NULL;
    }

    virtual void deepenShallowCopy()
    {
    }

};

inline void TError::printOn( ostream & out ) const
{
    out << nameOf() << '\n';
}

inline void TError::operator delete( void * )
{
    lib_error_handler("TError", "invalid call for operator delete");
}

class TGAObj : public TObject {
public:

    static  const TypeInfo 	infoObj;
    static  PCTypeInfo 		typeInfo();
    virtual RCTypeInfo          getTypeInfo() const;

    virtual int 		compare( RCTObject ) const;
    virtual void 		printOn( ostream  & out) const = 0;

    virtual PTObject 		shallowCopy() const = 0;
    virtual void 		deepenShallowCopy() = 0;

    virtual double& 		fitness();
    virtual double& 		objValue();
    virtual double  		fitness() const;
    virtual double  		objValue() const;

	    RTGAObj             operator=(RCTGAObj obj);

    virtual int 		length() const = 0;

    virtual PTGAObj  		oddPtCrossover(RCTGAObj, int) const = 0;
    virtual void     		oddPtCrossover(RCTGAObj, int,
					       RTGAObj) const = 0;
    virtual PTGAObj  		evenPtCrossover(RCTGAObj, int) const = 0;
    virtual void     		evenPtCrossover(RCTGAObj, int,
						RTGAObj) const = 0;
    virtual PTGAObj  		uniformCrossover(RCTGAObj, float) const = 0;
    virtual void     		uniformCrossover(RCTGAObj, float,
						 RTGAObj) const = 0;
    virtual void     		mutate(float) = 0;
    virtual void     		randomize() = 0;
    virtual void     		phenotype(PHENOTYPEFUNC pFunc);

    // a user supplied metric defining the distance between two objects
    virtual double              distance(RCTGAObj obj) const;


//      constructors
//      allowed to be called by derived classes only

protected:
    TGAObj();

private:
    double      flFitness;
    double      flObjValue;

};

inline  TObject::~TObject()
{
}

inline	PCTypeInfo TObject::typeInfo()
{
	return &infoObj;
}

inline RCTypeInfo TObject::getTypeInfo() const
{
	return infoObj;
}

inline BOOL TObject::isSame(RCTObject ob) const
{
	return this==&ob;
}

inline BOOL TObject::isKindOf(PCTypeInfo pTypeInfo) const
{
	return isA()->hasBase(pTypeInfo);
}

inline PCTypeInfo TObject::isA() const
{
        return & getTypeInfo();
}

inline const char* TObject::nameOf() const
{
	return isA()->name();
}

inline void TObject::error(const char  *msg) const
{
	(*lib_error_handler)( nameOf(), msg);
}

inline BOOL TObject::isEqual( RCTObject obj ) const
{
	return compare(obj) == 0;
}

inline  PCTypeInfo TError::typeInfo()
{
	return &infoObj;
}

inline RCTypeInfo TError::getTypeInfo() const
{
	return infoObj;
}

inline TGAObj::TGAObj() : flFitness(0), flObjValue(0)
{
}

inline	PCTypeInfo TGAObj::typeInfo()
{
	return &infoObj;
}

inline RCTypeInfo TGAObj::getTypeInfo() const
{
	return infoObj;
}

inline double& TGAObj::fitness()
{
	return flFitness;
}

inline double& TGAObj::objValue()
{
	return flObjValue;
}

inline double  TGAObj::fitness() const
{
	return flFitness;
}

inline double  TGAObj::objValue() const
{
	return flObjValue;
}

inline RTGAObj TGAObj::operator=(RCTGAObj obj)
{
	fitness() = obj.fitness();
	objValue() = obj.objValue();
	return *this;
}

inline void     TGAObj::phenotype(PHENOTYPEFUNC pFunc)
{
	flFitness = flObjValue = (*pFunc)(*this);
}

inline double   TGAObj::distance(RCTGAObj obj) const
{
	return MAXFLOAT;
}

inline	DontCare::operator char() const
{
	return '#';
}

inline	DontCare::operator int()  const
{
	return -1;
}

inline ostream & operator << ( ostream & out, RCTObject obj )
{
    obj.printOn( out );
    return out;
}

inline BOOL operator == ( RCTObject obj1, RCTObject obj2 )
{
    return obj1.isEqual( obj2 );
}

inline BOOL operator !=( RCTObject obj1, RCTObject obj2 )
{
    return ! ( obj1.isEqual(obj2) );
}

// if nValue is not between nMin and nMax (inclusive)
//    return nDefval
// else
//    return nValue

inline	double	rangeCheck(double flValue, const double flMin,
			   const double flMax, const double flDefval)
{
     return (((flValue) < (flMin)) ?
		(flDefval) : (((flValue) > (flMax)) ? (flDefval) : (flValue)));
}

inline  unsigned  flip(double flBias)
{
	if (((double) rand() / RAND_MAX) < flBias)
	    return 1;
	else
	    return 0;
}

#endif

