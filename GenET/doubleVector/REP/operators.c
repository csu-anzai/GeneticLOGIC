/* operators.c defines all operators for the current representation          */
/* NOTE: finishOper(), which must be called at the end of each operator,     */
/*    has variable number of arguments: all parents must be listed           */
/* These operators are for vector of doubles                                 */
/* NOTE: NumGenes/LowerDom/UpperDom are imported                             */

#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include "main.h"         /* provides global information and getCurPrctRun() */
#include "io.h"
#include "error.h"
#include "representation.def"          /* representation for these operators */
#include "rand.h"            /* provides probRand(), intRand() and binRand() */
#include "pop.h"              /* includes generic chromosome type definition */
#include "opers.h"                                  /* includes finishOper() */ 
#include "representation.h"                            /* standard interface */
#include "operators.h"                             /* interface to operators */
#include "problem.h"                       /* standard interface to yourself */
#include "representationNS.h"    /* non-standard, problme-specific interface */

#define DEBUG 0

#define REPEAT_DIFF_PARENT 10  /* how long try looking for different parents */

/*   each is called indirectly by address                                    */
/*   follow the standard format                                              */
/*   get more parents if needed by calling getParent(), put offspring into   */
/*     newPop up to maxNumOff. Call FinishOper() for each of numOffspring    */
/*   return number of new offspring placed in newChroms                      */
/*   maxNumOffspring is the max to be generated - do not overflow            */
/*   each requires function "operName_setNS" (or NULL) to set NS parameters  */

/* This is adaptive mutation on one gene - dense the probability distribution*/
/*   of possible mutations around current value as the simulation runs       */
/* Gives initially random mutation, later highly localized tuning            */
/* It uses non-standard parameters:                                          */
/*   exponent to determine speed of non-uniformity change                    */
static double Oper0_exp;
void nUMut1_setNS(FILE *fp)
{ if (getDoubleLtd(fp,"Give exponent, lower slows non-uniformity growth",
                   (double)0,(double)10,&Oper0_exp)!=0)
    FAIL("");
}
int nUMut1(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ static const int operNum=0;
  static const int numParents=1;
  static const int numOffspring=1;
  double *chrom;
  int which=intRand(NumGenes-1);                     /* which gene to mutate */
  if (maxNumOff<=0)
    return(0);
                                               /************ actual operator */
  memcpy(newChroms[curNumOff]->chrom,parentChrom->chrom,
         sizeof(double)*NumGenes);                        /* copy chromosome */
  chrom=(double*)newChroms[curNumOff]->chrom;
  if (binRand())                        /* mutate to right within boundaries */
    chrom[which]+=(UpperDom[which]-chrom[which])*(1-pow(probRand(),
                     pow(1-getCurPrctRun(),Oper0_exp)));
  else
    chrom[which]-=(chrom[which]-LowerDom[which])*
                     (1-pow(probRand(),pow(1-getCurPrctRun(),Oper0_exp)));
                                               /******************************/
  finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParent:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\tOffspring:\n\t");
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  printf("\n");
#endif
  return(numOffspring);
}

/* This is adaptive mutation on one gene - dense the probability distribution*/
/*   of possible mutations around current value as the simulation runs       */
/* Gives initially random mutation, later highly localized tuning            */
/* It is slightly less dense at end of simulation than nUMut1                */
/* It uses non-standard parameters:                                          */
/*   exponent to determine speed of non-uniformity change                    */
static double Oper1_exp;
void nUMut2_setNS(FILE *fp)
{ if (getDoubleLtd(fp,"Give exponent, higher slows non-uniformity growth",
                   (double)-10,(double)0,&Oper1_exp)!=0)
    FAIL("");
}
static int nUMut2(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ static const int operNum=1;
  static const int numParents=1;
  static const int numOffspring=1;
  double *chrom;
  int which=intRand(NumGenes-1);
  if (maxNumOff<=0)
    return(0);
                                               /************ actual operator */
  memcpy(newChroms[curNumOff]->chrom,parentChrom->chrom,
         sizeof(double)*NumGenes);
  chrom=(double*)newChroms[curNumOff]->chrom;
  if (binRand())
    chrom[which]+=(UpperDom[which]-chrom[which])*
                    pow(probRand(),pow(1-getCurPrctRun(),Oper1_exp));
  else
    chrom[which]-=(chrom[which]-LowerDom[which])*
                    pow(probRand(),pow(1-getCurPrctRun(),Oper1_exp));
                                               /******************************/
  finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParent:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\tOffspring:\n\t");
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  printf("\n");
#endif
  return(numOffspring);
}

/* 1-point crossover between genes (doubles), two offspring if space permits */
static void onePCross_setNS(FILE*fp)
{ return;                                                   /* no parameters */
}
static int onePCross(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ int i=0;
  genericChromType *parent2Chrom=getParent();
  static const int operNum=2;
  static const int numParents=2;
  static const int numOffspring=2;
  int numGeneratedOff;
  double *off1=(double*)newChroms[curNumOff]->chrom;
  double *off2;
  double *p1=(double*)parentChrom->chrom;
  double *p2;
  int which=intRand(NumGenes-2)+1; /* which=1..lastGene */
  if (maxNumOff<=0)
    return(0);
  while (parentChrom==parent2Chrom && i<REPEAT_DIFF_PARENT)
  { parent2Chrom=getParent();     /* prevent crossing with itself, but allow */
    i++;                          /* if suitableother parent cannot be found */
  }
  p2=(double*)parent2Chrom->chrom;
                                                  /********* actual operator */
  if (maxNumOff==1)        
  { for (i=0; i<which; i++)
      off1[i]=p1[i];
    for ( ; i<NumGenes; i++)
      off1[i]=p2[i];
    numGeneratedOff=1;
  }
  else
  { off2=(double*)newChroms[curNumOff+1]->chrom;
    for (i=0; i<which; i++)
    { off1[i]=p1[i];
      off2[i]=p2[i];
    }
    for ( ; i<NumGenes; i++)
    { off1[i]=p2[i];
      off2[i]=p1[i];
    }
    numGeneratedOff=2;
  }
                                                  /***************************/
  finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
             parent2Chrom);
  if (numGeneratedOff==2)
    finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
               parent2Chrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParents:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\t");
  dspChrom(stdout,parent2Chrom->chrom);
  printf("\tOffspring:\n\t");
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  if (numGeneratedOff==2)
  { fprintf(stdout,"\t");
    dspChrom(stdout,newChroms[curNumOff-2]->chrom);
  }
  printf("\n");
#endif
  return(numGeneratedOff);
}

/* uniform crossover between genes (doubles), as of Spears and DeJong        */
/*   each gene is exchanged with prob. exchProb                              */
/* Two offspring if space permits                                            */
/* It uses non-standard parameters:                                          */
/*   exchProb, use 0.5 for completely uniform exchange                       */
static double Oper3_exchProb;
void uCross_setNS(FILE *fp)
{ if (getDoubleLtd(fp,"Give exchange probability, 0.5 for uniform",
                   (double)0,(double)1,&Oper3_exchProb)!=0)
    FAIL("");
}
static int uCross(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ int i=0;
  genericChromType *parent2Chrom=getParent();
  static const int operNum=3;
  static const int numParents=2;
  static const int numOffspring=2;
  int numGeneratedOff;
  double *off1=(double*)newChroms[curNumOff]->chrom;
  double *off2;
  double *p1=(double*)parentChrom->chrom;
  double *p2;
  if (maxNumOff<=0)
    return(0);
  while (parentChrom==parent2Chrom && i<REPEAT_DIFF_PARENT)
  { parent2Chrom=getParent();     /* prevent crossing with itself, but allow */
    i++;                          /* if suitableother parent cannot be found */
  }
  p2=(double*)parent2Chrom->chrom;
                                                  /********* actual operator */
  if (maxNumOff==1)                             /* only one offspring needed */
  { for (i=0; i<NumGenes; i++)
      if (probRand()<Oper3_exchProb)                             /* exchange */
        off1[i]=p2[i];
      else
        off1[i]=p1[i];
    numGeneratedOff=1;
  }
  else
  { off2=(double*)newChroms[curNumOff+1]->chrom;
    for (i=0; i<NumGenes; i++)
    if (probRand()<Oper3_exchProb)                               /* exchange */
    { off1[i]=p2[i];
      off2[i]=p1[i];
    }
    else
    { off1[i]=p1[i];
      off2[i]=p2[i];
    }
    numGeneratedOff=2;
  }
                                                  /***************************/
  finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
             parent2Chrom);
  if (numGeneratedOff==2)
    finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
               parent2Chrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParents:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\t");
  dspChrom(stdout,parent2Chrom->chrom);
  printf("\tOffspring:\n\t");
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  if (numGeneratedOff==2)
  { fprintf(stdout,"\t");
    dspChrom(stdout,newChroms[curNumOff-2]->chrom);
  }
  printf("\n");
#endif
  return(numGeneratedOff);
}

/* average of two whole chromosomes                                          */
static void aveWholeTwo_setNS(FILE*fp)
{ return;
}
static int aveWholeTwo(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ int i=0;
  static const int operNum=4;
  static const int numParents=2;
  static const int numOffspring=1;
  genericChromType *parent2Chrom=getParent();
  double *off=(double*)newChroms[curNumOff]->chrom;
  double *p1=(double*)parentChrom->chrom;
  double *p2=(double*)parent2Chrom->chrom;
  if (maxNumOff<=0)
    return(0);
  while (parentChrom==parent2Chrom && i<REPEAT_DIFF_PARENT) 
  { parent2Chrom=getParent();  
    i++;
  }
                                                  /********* actual operator */
  for (i=0; i<NumGenes; i++)
    off[i]=(p1[i]+p2[i])/2;
                                                  /***************************/
  finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
             parent2Chrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParents:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\t");
  dspChrom(stdout,parent2Chrom->chrom);
  printf("\tOffspring:\n\t");
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  printf("\n");
#endif
  return(numOffspring);
}

       /* NOTE: make sure the following information agrees with file content */
#define NUMOPERS 5                         
static char *OperNames[NUMOPERS]={"non-uniform mutation, less dense",
                                  "non-uniform mutation, more dense",
                                  "one-point crossover",
                                  "uniform crossover",
                                  "average of two whole chromosomes"};
static operAddrType OperAddrs[]=
    {nUMut1,nUMut2,onePCross,uCross,aveWholeTwo};
static void (*OperAddrs_setNS[])(FILE*)=
    {nUMut1_setNS,nUMut2_setNS,onePCross_setNS,uCross_setNS,aveWholeTwo_setNS};

char *getOperName(int operNum)
{ if (operNum>=NUMOPERS)
    FAIL("");
  return(OperNames[operNum]);
}

operAddrType getOperAddr(int operNum)
{ if (operNum>=NUMOPERS) 
    FAIL(""); 
  return(OperAddrs[operNum]); 
}

int getNumOpers(void)
{ return(NUMOPERS);
}

/* setOperSpecifics() sets any specific/interactive information for the run  */
void setOperSpecifics(FILE *fp, int operNum)
{ OperAddrs_setNS[operNum](fp);
}
