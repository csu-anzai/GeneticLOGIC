//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TKCNVG_H )
#define __TKCNVG_H

#include        "ga.h"

class ConvergenceTest
{
public :
      ConvergenceTest(unsigned uCheckCycle) :
        uCycle(uCheckCycle), nCount(uCheckCycle) {}
      virtual ~ConvergenceTest() {}

      virtual   BOOL    checkNow()
      {
                if (--nCount <= 0) {
                        nCount = uCycle;
                        return TRUE;
                }
                else
                        return FALSE;
      }

      virtual   BOOL    operator()(RCGeneticAlgorithm) = 0;

protected :
      unsigned  uCycle; // check for convergence if nCount equals 0
      int       nCount; // current count
};

class   HammingConvergence : public ConvergenceTest
{
public :
        HammingConvergence(unsigned uCheckCycle, unsigned delta) :
                ConvergenceTest(uCheckCycle), hd(delta) {}

      virtual   BOOL    operator()(RCGeneticAlgorithm);

protected :
        unsigned        hd;     // the population is considered converged
                                // if none of the individuals differs
                                // from the best by a Hamming Distance
                                // of hd
};

#endif

