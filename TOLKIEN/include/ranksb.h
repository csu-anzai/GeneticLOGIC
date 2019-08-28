//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __RANKSUB_H )
#define __RANKSUB_H

#include <iostream.h>
#include "IntVec.h"
#include "trand.h"

//
// use algorithm S and algorithm P of section 3.4.2 of
// Knuth's Seminumerical Algorithms to generate M
// random integers from 0, 1, .., N - 1
//
class   RandIntSubSet
{
public:
                RandIntSubSet();
        void    ordered(IntVec &, int, int);
        void    random(IntVec &, int, int);
};

inline RandIntSubSet::RandIntSubSet()
{
}

extern  RandIntSubSet    ranksb;

#endif

