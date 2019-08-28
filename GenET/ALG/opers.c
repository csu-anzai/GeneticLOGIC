/* opers.c contains routines to fire operators and modify weights */
 
#include <stdio.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include "main.h"
#include "io.h"
#include "error.h"
#include "roulette.h"
#include "storage.h"
#include "pop.h"
#include "opers.h"
#include "operators.h"
#include "problem.h"  

#define COPY_ROULETTE 0     /* they slightly behave differently, even though */
                             /* they should be exactly the same. INVESTIGATE */

static boolean AdaptOpers;                    /* if true then modify weights */
static boolean AdaptOpersSet=false;        /* to ensure setting before using */
static boolean ResetOperApplCounters;  /* reset counters after modification? */
static double ChangePrct;               /* smaller values for slower changes */
static double MinProb;             /* 0..1/NumOpers, >0 to increase liveness */
static double AddProb;           /* p[i] in roulette will be p[i]+AddProb to */
                       /* guarantee p[i] actually used in roulette >=MinProb */ 

static double *OperProbs;                  /* actually used operator weights */
static boolean OperProbsSet=false;  /* to ensure OPerProbs is set before use */
static operType *Opers;                        /* dynamic array of operators */
static double *RouletteWghts;     /* allocated array for computing new wghts */
static NumUsedOpers; /* number actually used in a run, >0 && <=getNumOpers() */
static int *OperMap; /* provides mapping from available to used opers numbrs */

/* setOpers must be called prior to actual simulation and before setTiming   */
/* set up all operator data structures                                       */
/* Calls problem specific function to set operator addresses, names          */
/* and number of parent/offspring. Calculates initial oper. probabilities    */
/* in OperProbs vector */
void setOpers(FILE *fp)
{ int i, what;
  double totalWghts=0;
  double *whichOpers;              /* entry <0 will indicate unsied operator */
  char prompt[2*NAMESIZ];
  if (getNumOpers()<=0)            /* provides number of available operators */
    FAIL("Need at least one operator");
  if (getIntLtd(fp,"Do you want adaptable operator probabilities?",
                0,1,(int*)&AdaptOpers)!=0)
    FAIL("");
  AdaptOpersSet=true;
  if (AdaptOpers==true)
  { if (getDoubleLtd(fp,"Give change percent (smaller for smaller changes)",
                   (double)0.001,(double)1,&ChangePrct)!=0)
      FAIL("");
    if (getIntLtd(fp,"Do you want to reset oper. counters after modification?",
                0,1,(int*)&ResetOperApplCounters)!=0)
    FAIL("");
  }
  else 
    ChangePrct=0;

  NumUsedOpers=0;  /* first find number and identity of operators to be used */
  whichOpers=makeDoubleVector(getNumOpers(),(double)-1);   /* all unused yet */
  OperMap=makeIntVector(getNumOpers(),(int)-1);
  for (i=0; i<getNumOpers(); i++)         /* extract actually used operators */
  { sprintf(prompt,"Do you want to use %s?",getOperName(i));
    getIntLtd(fp,prompt,0,1,&what);
    if (what==1)
    { sprintf(prompt,"Give initial weight for : %s",getOperName(i));
      if (getDoubleLtd(fp,prompt,(double)0,(double)1,whichOpers+i)!=0)
        FAIL("");
      totalWghts+=whichOpers[i];
      OperMap[i]=NumUsedOpers;
      NumUsedOpers++;
      setOperSpecifics(fp,i);
    }
  }               /* after the loop, whichOper[i]>=0 indicates used operator */
          /* and OperMap[i]=j indicates implemented oper'i is used as oper'j */
  if (NumUsedOpers==0)
    FAIL("Need to use at least one operator");

                            /* now allocate storage for to-be-used operators */
  OperProbs=makeDoubleVector(NumUsedOpers,(double)0);
  Opers=(operType*)getStorage(NumUsedOpers,sizeof(operType));
  if (getDoubleLtd(fp,"Give min operator probability",(double)0,
                   (double)1/NumUsedOpers-0.01,&MinProb)!=0) 
    FAIL("");
  AddProb=MinProb/(1-MinProb*NumUsedOpers);         /* add this to each p[i] */
 
  printf("Initial operator probabilities are\n");  
  for (i=0; i<getNumOpers(); i++)           /* now set up the used operators */
  { if (OperMap[i]<0)                              /* operator i is not used */
      continue;
    strcpy(Opers[OperMap[i]].operName,getOperName(i));
    Opers[OperMap[i]].operAddr=getOperAddr(i);
    if (totalWghts==0)
      OperProbs[OperMap[i]]=(double)1/NumUsedOpers;
    else
      OperProbs[OperMap[i]]=whichOpers[i]/totalWghts;
    printf("\t%*s\t%f\n",NAMESIZ,Opers[OperMap[i]].operName,
           (OperProbs[OperMap[i]]+AddProb)/(1+NumUsedOpers*AddProb));
  }
  RouletteWghts=makeDoubleVector(NumUsedOpers,(double)0);
  OperProbsSet=true;
}

int getNumUsedOpers(void)
{ if (OperProbsSet==false)
    FAIL("Wrong call sequence in implementation");
  return(NumUsedOpers);
}

/* createInitOperRoulette() creates, initializes and returns roulette using  */
/*   initial OperProbs; no AddProb needed initially since initial OperProbs  */
/*   are set using MinProb                                                   */
void *createInitOperRoulette(void)
{ void *r;
  r=createRoulette(NULL,NumUsedOpers);
  setRoulette(r,OperProbs);
  return(r);
}

double getAddProb(void)
{ if (OperProbsSet==false)
    FAIL("Wrong call sequence in implementation");
  return(AddProb);
}

/* initOperProbs(p) initializes vector p with OperProbs                      */
void initOperProbs(double *p)
{ int i;
  if (OperProbsSet==false)
    FAIL("Wrong call sequence in implementation");
  for (i=0; i<NumUsedOpers; i++)
    p[i]=OperProbs[i];
}

/* modifyOperProbs(p,q) modifies probabilities p. q counts fired operators   */
/*  individually and *Q totals them. Afterwards, reset OperRoulette          */
void modifyOperProbs(double *p, double *q, double *Q, void *OperRoulette)
{ register int i;
  double qProb;
  if (*Q==0                                                 /* nothing fired */
      || NumUsedOpers<2)                          /* nothing can be modified */
    return;
  for (i=0; i<NumUsedOpers; i++)
  { qProb=q[i]/(*Q);
    p[i]=ChangePrct*qProb+(1-ChangePrct)*p[i];           /* true probability */
    RouletteWghts[i]=p[i]+AddProb;                   /* to guarantee MinProb */
    if (ResetOperApplCounters==true)
      q[i]=0;
  }
  if (ResetOperApplCounters==true)
    *Q=0;
  setRoulette(OperRoulette,RouletteWghts);
}

boolean adaptOpers(void)
{ if (AdaptOpersSet==false)
    FAIL("Wrong call sequence in implementation");
  return(AdaptOpers);
}

/* fireOper(operNum,parentChrom,parentChroms,newChroms,maxNumOffs,curOffs)   */
/*   fire operNum, and return its returning value                            */
int fireOper(int operNum, genericChromType *parentChrom, 
                    genericChromsType newChroms,int maxNumOffspring,
                    int curNumOffspring)
{ return(Opers[operNum].operAddr(parentChrom,newChroms,maxNumOffspring,
                                 curNumOffspring));
}

char *getUsedOperName(int whatOper)
{ return(Opers[whatOper].operName);
}

/* finishOper(newChrom, ...) is called after offspring are generated, once   */
/*   for each offspring. The variable list contains all parents for newChrom */
/*   It creates the proper oper counters for the offspring. The standard     */
/*   used is that the new counters are averages of those of parents          */
/*   It finally sets a roulette for the new chrom and evaluates it           */
/* NOTE: operNum is number if the available operator, not used; use OperMap  */
/*   to get the actual used number                                           */
void finishOper(genericChromType *newChrom, int operNum,
                       int *curNumOff, int numParents, ...)
{ register int i;
  register int j;
  va_list parentList;
  genericChromType *parent;
  va_start(parentList,numParents);

  parent=va_arg(parentList,genericChromType*);
         /* first copy operator probabilities and counters from first parent */
  memcpy(newChrom->operProbs,parent->operProbs,sizeof(double)*NumUsedOpers);
  memcpy(newChrom->operApplCounter,parent->operApplCounter,
           sizeof(double)*NumUsedOpers);
  newChrom->operApplCounterTotal=parent->operApplCounterTotal;
  for (i=1; i<numParents; i++)       /* accumulate from other parents if any */
  { parent=va_arg(parentList,genericChromType*);
    for (j=0; j<NumUsedOpers; j++)  
    { newChrom->operProbs[j]+=parent->operProbs[j];
      newChrom->operApplCounter[j]+=parent->operApplCounter[j];
    }
    newChrom->operApplCounterTotal+=parent->operApplCounterTotal;
  } 
  if (numParents>1)                                 /* must average the sums */
  for (i=0; i<NumUsedOpers; i++)                      
  { newChrom->operProbs[i]/=numParents;
    newChrom->operApplCounter[i]/=numParents;
  }
  newChrom->operApplCounter[OperMap[operNum]]++;
  newChrom->operApplCounterTotal=newChrom->operApplCounterTotal/numParents+1;
#if COPY_ROULETTE
  if (numParents>1)
    setRoulette(newChrom->roulette,newChrom->operProbs);
  else
    copyRoulette(newChrom->roulette,parent->roulette);
#else
  setRoulette(newChrom->roulette,newChrom->operProbs);
#endif
  newChrom->eval=evalChrom(newChrom->chrom);
  ++(*curNumOff);
}
