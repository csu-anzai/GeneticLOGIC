//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __POPULATN_H )
#define     __POPULATN_H

#include "errhndl.h"
#include "toarray.h"
#include "tobject.h"
#include "tvalues.h"


class   Population : public TGAObj
{
public:
                                DECLARE_RTTI()

	virtual			~Population();

        virtual PTObject        shallowCopy() const = 0;
        virtual void            deepenShallowCopy();

        virtual BOOL            isEqual( RCTObject ) const;
	virtual void            printOn( ostream  & out) const;

        virtual void            randomize();
        virtual void            mutate(float);

        virtual PTGAObj         oddPtCrossover(RCTGAObj, int) const;
        virtual void            oddPtCrossover(RCTGAObj, int, RTGAObj) const;
        virtual PTGAObj         evenPtCrossover(RCTGAObj, int) const;
        virtual void            evenPtCrossover(RCTGAObj, int, RTGAObj) const;
        virtual PTGAObj         uniformCrossover(RCTGAObj, float) const;
	virtual void            uniformCrossover(RCTGAObj, float, RTGAObj) const;

        virtual PTGAObj         at(sizeType index) const;
        virtual PTGAObj         elem(sizeType index) const;

	virtual void            setAt(sizeType index, PTGAObj pObj, BOOL = TRUE);
	virtual BOOL            add(PTGAObj, BOOL = TRUE);
	virtual void            addAll(RCCollection from, BOOL = TRUE);
	virtual void     	removeAll();

	virtual void            findBestandWorst(RPTGAObj, RPTGAObj) const;

	virtual int      	length() const;
	virtual int      	size() const;

                                operator RCCollection() const;
        virtual RPopulation     operator=(RCPopulation);

                void            phenotypeFunc(PHENOTYPEFUNC);
                PHENOTYPEFUNC   phenotypeFunc() const;

        virtual void            replaceBy(RCollection, BOOL = FALSE);

protected:

        PCollection      pInds;
        PHENOTYPEFUNC    pfuncPhenotype;        // pointer to function
						// for phenotype evaluation

        Population(PHENOTYPEFUNC, PCollection=NULL);
        Population(RCPopulation);
};

inline Population::Population(PHENOTYPEFUNC pFunc, PCollection pCltn) :
        TGAObj(), pfuncPhenotype(pFunc)
{
        if (pCltn == NULL)
            pInds = (PCollection) new TOArray();
        else
            pInds = pCltn;

        pInds->ownsElements(owner);
}

inline Population::Population(RCPopulation src) :
	TGAObj((RCTGAObj) src),
        pfuncPhenotype(src.pfuncPhenotype),
        pInds((PCollection) src.pInds->shallowCopy())
{
        pInds->deepenShallowCopy();
}

inline Population::~Population()
{
        delete pInds;
}

inline BOOL Population::isEqual( RCTObject obj ) const
{
	if ( obj.isA() == isA() &&
	     TGAObj::isEqual(obj) )
	    return pInds->isEqual(* ((RCPopulation) obj).pInds);
	else
	    return FALSE;
}

inline void     Population::removeAll()
{
        pInds->removeAll();
}

inline int     Population::length() const
{
        return pInds->size();
}

inline int     Population::size() const
{
        return pInds->size();
}

inline  Population::operator RCCollection() const
{
        return * (PCCollection) pInds;
}

inline PHENOTYPEFUNC Population::phenotypeFunc() const
{
        return pfuncPhenotype;
}

inline void Population::phenotypeFunc(PHENOTYPEFUNC newfunc)
{
	pfuncPhenotype = newfunc;
}

inline PTGAObj Population::at(sizeType index) const
{
        return (PTGAObj) pInds->at(index);
}

inline PTGAObj Population::elem(sizeType index) const
{
        return (PTGAObj) pInds->elem(index);
}

inline void    Population::deepenShallowCopy()
{
        // performs nothing, since pInds is already a deep copy
}

inline BOOL    Population::add(PTGAObj pObj, BOOL evaluate)
{
        if (evaluate)
            pObj->phenotype(pfuncPhenotype);
        return pInds->add((PTObject) pObj);
}

inline void    Population::setAt(sizeType index, PTGAObj pObj, BOOL evaluate)
{
        if (evaluate)
            pObj->phenotype(pfuncPhenotype);
        pInds->setAt(index, (PTObject) pObj);
}

inline PTGAObj  Population::oddPtCrossover(RCTGAObj mate, int nXpt) const
{
	PRECONDITION(mate.isKindOf(isA()));
        return (PTGAObj) deepCopy();
}

inline void     Population::oddPtCrossover(RCTGAObj mate, int nXpt,
                                           RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	(RPopulation) child = *this;
}

inline PTGAObj  Population::evenPtCrossover(RCTGAObj mate, int nXpts) const
{
	PRECONDITION(mate.isKindOf(isA()));
        return (PTGAObj) deepCopy();
}

inline void     Population::evenPtCrossover(RCTGAObj mate, int nXpts,
                                            RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	(RPopulation) child = *this;
}

inline PTGAObj  Population::uniformCrossover(RCTGAObj mate,
                                             float flXRate) const
{
	PRECONDITION(mate.isKindOf(isA()));
        return (PTGAObj) deepCopy();
}

inline void     Population::uniformCrossover(RCTGAObj mate, float flXRate,
                                             RTGAObj child) const
{
	PRECONDITION(child.isKindOf(isA()));
	(RPopulation) child = *this;
}

#endif

