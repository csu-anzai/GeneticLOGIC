//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __BININD_H )
#define     __BININD_H

#include "tobject.h"
#include "bitstr.h"

class BinIndividual : public TGAObj
{
public:

				DECLARE_RTTI()

    virtual void 		printOn( ostream  & out) const = 0;

    virtual PTObject 		shallowCopy() const = 0;
    virtual void 		deepenShallowCopy() = 0;

    virtual int 		length() const = 0;

    virtual void                invert(int) = 0;
    virtual void                set(int) = 0;
    virtual void                clear(int) = 0;

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

    virtual float               asFloat(int,int) const = 0;
    virtual unsigned long       asLong(int,int) const = 0;
    virtual float               asFloat() const = 0;
    virtual unsigned long       asLong() const = 0;

    virtual RBinIndividual      operator=(RCBinDiploid) = 0;
    virtual RBinIndividual      operator=(RCBinHaploid) = 0;
    virtual RBinIndividual      operator=(RCBitString) = 0;

    virtual BOOL                isGrayCode() const = 0;
    virtual void                isGrayCode(BOOL flag) = 0;

    virtual                     operator RCBitString() const = 0;

    virtual unsigned            hammingDistance(RCBitString) const = 0;
};

#endif

