//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __SCALE_H )
#define     __SCALE_H

#include "ga.h"

class	ScalingScheme : public TObject
{
public:

				DECLARE_RTTI()

	virtual 		~ScalingScheme();

	virtual void 		printOn( ostream  & out) const;

	virtual int 		compare( RCTObject ) const = 0;
	virtual PTObject 	shallowCopy() const = 0;
	virtual void 		deepenShallowCopy();

        virtual void            operator()(RGeneticAlgorithm,
                                           RPTGAObj, RPTGAObj);
};

class	SigmaScaling : public ScalingScheme
//                     _
//      f(x) = g(x) - (g - c * …)
//
//           f(x) is the fitness value
//           g(x) is the objective value (g(x) is the smaller the better)
//           _
//           g    is the mean of g(x)
//           …    is the SD of g(x)
//           c    is a value between 0 - 3
{
public:
				DECLARE_RTTI()

	SigmaScaling(float);

	virtual void 		printOn( ostream  & out) const;
	virtual int 		compare( RCTObject ) const;
				_shallowCopy(SigmaScaling)

        virtual void    operator()(RGeneticAlgorithm,
                                   RPTGAObj, RPTGAObj);
	virtual float   scaleFactor(float);
	virtual float   scaleFactor() const;

protected:
	float flSFactor;
};

class   Min2MaxScaling : public SigmaScaling
//              _                          _
//      f(x) = (g + c * …) - g(x) if g(x) < (g + c * …)
//           = 0                     otherwise
//
//           f(x) is the fitness value
//           g(x) is the objective value (g(x) is the smaller the better)
//           _
//           g    is the mean of g(x)
//           …    is the SD of g(x)
//           c    is a value between 0 - 3
{
public :
				DECLARE_RTTI()

				Min2MaxScaling(float);
				_shallowCopy(Min2MaxScaling)
        virtual void            operator()(RGeneticAlgorithm,
                                           RPTGAObj, RPTGAObj);
};

class   NonNegScaling : public ScalingScheme
{
public :
				DECLARE_RTTI()

	virtual PTObject 	shallowCopy() const;
	virtual int 		compare( RCTObject ) const;
        virtual void            operator()(RGeneticAlgorithm,
                                           RPTGAObj, RPTGAObj);
};

class	LinearScaling : public ScalingScheme
{
public:
				DECLARE_RTTI()

	virtual PTObject 	shallowCopy() const;
	virtual int 		compare( RCTObject ) const;
        virtual void            operator()(RGeneticAlgorithm,
                                           RPTGAObj, RPTGAObj);
};

inline  ScalingScheme::~ScalingScheme()
{
}

inline  void ScalingScheme::deepenShallowCopy()
{
}

inline  void ScalingScheme::printOn( ostream  & out) const
{
	out << nameOf();
}

inline	SigmaScaling::SigmaScaling(float flValue)
{
	scaleFactor(flValue);
}

inline  Min2MaxScaling::Min2MaxScaling(float flValue) :
	SigmaScaling(flValue)
{
}

inline float SigmaScaling::scaleFactor(float flScaleFactor)
{
	float   flOldValue = flSFactor;
	flSFactor = flScaleFactor > 0 ?
		    ((flScaleFactor <= 3.0) ? flScaleFactor : 1.0 ) : 0;
	return  flOldValue;
}

inline float  SigmaScaling::scaleFactor() const
{
	return flSFactor;
}

inline  void SigmaScaling::printOn(ostream& out) const
{
	ScalingScheme::printOn(out);
	out << " (" << scaleFactor() << ')';
}

inline int SigmaScaling::compare( RCTObject obj ) const
{
	if (obj.isKindOf(isA()))
	    return flSFactor == ((RCSigmaScaling) obj).flSFactor ? 0 : 1;
	else
	    return -1;
}

inline  PTObject NonNegScaling::shallowCopy() const
{
	return (PTObject) new NonNegScaling();
}

inline  PTObject LinearScaling::shallowCopy() const
{
	return (PTObject) new LinearScaling();
}

inline  int NonNegScaling::compare( RCTObject obj ) const
{
	return obj.isKindOf(isA()) ? 0 : -1;
}

inline  int LinearScaling::compare( RCTObject obj ) const
{
	return obj.isKindOf(isA()) ? 0 : -1;
}

#endif

