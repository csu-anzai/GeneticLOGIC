/* problem.c contains problem specific routines                              */
/* this is dynamic control problem and sample operators for FP vectors       */
/*   from "A Modified Genetic Algorithm for Optimal Control Problems"        */
/*   Z. Michalewicz, C.Z. Janikow, and J.B. Krawczyk, Computers and          */
/*   Mathematics with Applications, Vol. 23, No. 12, pp. 83-94, 1992.        */
/* assumed chromosome is a vector of NumElems doubles                        */

#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "main.h"
#include "io.h"                                  /* provides own io routines */
#include "representation.def"              /* representation for the problem */
#include "error.h"                                   /* provides fail(char*) */
#include "pop.h"              /* includes generic chromosome type definition */
#include "representation.h"                            /* standard interface */
#include "opers.h"        /* provides operType needed on problem.h interface */
#include "operators.h"                     /* interface to current operators */
#include "rand.h"            /* provides probRand(), intRand() and binRand() */
#include "problem.h"                /* check against your standard interafce */
#include "representationNS.h"    /* non-standatd, problem-specific interface */


/* problem VII from the paper - see the paper for other cases */
#define a 1.0
#define b 1.0
#define q 1000.0
#define r 1.0
#define s 1.0
#define x0 100.0


/* evalChrom() specific to representation and problem */
double evalChrom(void *chrom)
{ double *p=(double*)chrom;                
  double sum=0;
  double x;
  register int i;
  x=x0;
  for (i=0; i<NumGenes; i++)
  { sum+=s*x*x+r*p[i]*p[i];
    x=a*x+b*p[i];
  }
  return(sum);
}

/* initChrom(chrom) is problem-specific but only for the given representation*/
/* Here, random initialization within domains                                */
void initChrom(genericChromType *c, FILE *fp)
{ int i;
  double *chrom=(double*)c;
  for (i=0; i<NumGenes; i++)
    chrom[i]=LowerDom[i]+probRand()*(UpperDom[i]-LowerDom[i]);
}
