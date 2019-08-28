/* roulette.c */

#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "main.h"
#include "rand.h"
#include "error.h"
#include "storage.h"
#include "roulette.h"

typedef struct
        { int length;
          double *vals;  /* locally allocated or a pointer to external array */
        } rouletteType;     /* vals[k]=vals[k-1]+val[k], assuming all val>=0 */

/* createRoulette(valsP,len)                                                 */
/*   if valsP==NULL then allocate local storage for len values               */
/*   else use the supplied array                                             */
void *createRoulette(double *valsP, int len)
{ rouletteType *rP=(rouletteType*)getStorage(1,sizeof(rouletteType));
  if (valsP==NULL)
    rP->vals=makeDoubleVector(len,(double)0);
  else
    rP->vals=valsP;
  rP->length=len;
  return((void*)rP);
}

/* setRoulette(rouletteType *rP, double *val)                                */
/*   set up rP roulette using nonnegative values val                         */
void setRoulette(void *rP, double *val)
{ rouletteType *rouletteP=(rouletteType*)rP;
  register int max=rouletteP->length;
  register int i;
  rouletteP->vals[0]=val[0];
  for (i=1; i<max; i++)
    rouletteP->vals[i]=rouletteP->vals[i-1]+val[i];
}

/* spinRoulette(rP) spins the roulette rP and returns selected index         */
int spinRoulette(void *rP)
{ rouletteType *rouletteP=(rouletteType*)rP;
  register int i=rouletteP->length-1;
  double d=doubleRand(rouletteP->vals[i]);
  for (i--; i>=0; i--)
    if (d>=rouletteP->vals[i]) break;
  if (++i<0) FAIL("Roulette overrun");
  return(i);
}
 
/* copyRoulette(to,from) copies vals from from to to roulette                */
void copyRoulette(void *to, void *from)
{ rouletteType *toR=(rouletteType*)to;
  rouletteType *fromR=(rouletteType*)from;
  memcpy((void*)toR->vals,(void*)fromR->vals,sizeof(double)*toR->length);
}

void dspRoulette(void *r)
{ int i;
  rouletteType *R=(rouletteType *)r;
  printf("Roulette: ");
  for (i=0; i<R->length; i++)
    printf("%.4f ",R->vals[i]);
  printf("\n");
}
