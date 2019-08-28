//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined (__TRAND_H)

#define __TRAND_H

#include <stdlib.h>
#include <time.h>
#include "tdefs.h"
#include "RNG.h"
#include "MLCG.h"

class   TRandom
{
 public:

       TRandom(long low, long high, RNG *gen);
       TRandom(long high, RNG *gen);
       TRandom(RNG *gen);

// read params

  virtual long low() const;
  virtual long high() const;
  virtual RNG* generator() const;

// change params

  virtual   long low(long x);
  virtual   long high(long x);
  virtual   RNG* generator(RNG *gen);

// get a random number

  virtual   long asLong();
  virtual   long operator()(); // synonym for asLong
  virtual   int  asInt();      // (possibly) truncate as int
  virtual   float asFloat();
  virtual   double asDouble();

// override params for one shot

  virtual   long asLong(long high);
  virtual   long asLong(long low, long high);

  virtual   long operator () (long high);  // synonyms
  virtual   long operator () (long low, long high);

  virtual   BOOL  flip(const float flVal);

protected:
  RNG *pGenerator;
  long pLow;
  long pHigh;

  long _asLong(long, long);
};

inline TRandom::TRandom(long low, long high, RNG *gen)
     : pLow((low < high) ? low : high),
       pHigh((low < high) ? high : low),
       pGenerator(gen)
{}

inline TRandom::TRandom(long high, RNG *gen)
     : pLow((0 < high) ? 0 : high),
       pHigh((0 < high) ? high : 0),
       pGenerator(gen)
{}


inline TRandom::TRandom(RNG *gen)
     : pLow(0),
       pHigh(1),
       pGenerator(gen)
{}

inline RNG* TRandom::generator() const { return pGenerator;}
inline long TRandom::low() const       { return pLow; }
inline long TRandom::high() const      { return pHigh; }

inline RNG* TRandom::generator(RNG *gen)
{
  RNG *tmp = pGenerator; pGenerator = gen;  return tmp;
}

inline long TRandom::low(long x)
{
  long tmp = pLow;  pLow = x;  return tmp;
}

inline long TRandom:: high(long x)
{
  long tmp = pHigh; pHigh = x; return tmp;
}

inline long TRandom:: _asLong(long low, long high)
{
  return (pGenerator->asLong() % (high-low+1)) + low;
}

inline long TRandom:: asLong()
{
  return _asLong(pLow, pHigh);
}

inline long TRandom:: asLong(long high)
{
  return _asLong(pLow, high);
}

inline long TRandom:: asLong(long low, long high)
{
  return _asLong(low, high);
}

inline long TRandom:: operator () ()
{
  return _asLong(pLow, pHigh);
}

inline long TRandom:: operator () (long high)
{
  return _asLong(pLow, high);
}

inline long TRandom:: operator () (long low, long high)
{
  return _asLong(low, high);
}

inline int TRandom:: asInt()
{
  return int(asLong());
}

inline float TRandom::asFloat()
{
	return pGenerator->asFloat();
}

inline double TRandom::asDouble()
{
	return pGenerator->asDouble();
}

inline BOOL TRandom::flip(const float flVal)
//
//      flip an biased coin
//
{
        return (pGenerator->asDouble() > flVal ? 0 : 1) ;
}

extern  TRandom        tRand;
extern  MLCG	       mlcg;

#endif

