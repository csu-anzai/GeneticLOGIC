/* problem.c contains problem specific routines                              */
/* This is an example of a flat problme file - no explicit representation    */
/*   files are defined, all is included in this file                         */
/* NOTE: the standard interface to problem is evalChrom() and initChrom(),   */
/*   to representation is makeChrom() and dspChrom(), and to operators is    */
/*   getOperName(), getOperAddr(), getNumOpers() - they must be provided here*/
/* This example uses three double genes as a chromosome and attempts         */
/*   to maximize their sum; gene[i] in [i,i+1]. For illustration, a          */
/*   chromType is defined as a record containing one gene and an array of    */
/*   two other genes (doubles), to illustrates transparency of representation*/
/*   It uses a simple random mutation on one double, and an average operator */
/*   on whole chromosomes                                                    */
/* NOTE: finishOper() has variable number of arguments                       */

#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "main.h"
#include "error.h"
#include "storage.h"
#include "rand.h"
#include "pop.h"              /* includes generic chromosome type definition */
#include "opers.h"                           /* includes operType definition */
#include "problem.h"                                    /* standard interface*/
#include "operators.h"                                 /* standard interface */
#include "representation.h"                            /* standard interface */

#define DEBUG 0

typedef struct
        { double firstGene;
          double twoMoreGenes[2];  /* fixed two genes, but could be variable */
        } chromType;

double evalChrom(void *c)
{ chromType *chrom=(chromType*)c;
  return(chrom->firstGene+chrom->twoMoreGenes[0]+chrom->twoMoreGenes[1]-4);
                                     /* -4 to have some negative evaluations */
}

/* This is increasing mutation on one gene                                   */
static int mut(genericChromType *parentChrom,  
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ int i;
  static const int operNum=0;
  static const int numParents=1;
  static const int numOffspring=1;
  double *dP;
  int which=intRand(2);  /* 0..2 */
  if (maxNumOff<=0)
    return(0);
  newChroms[curNumOff]->eval=parentChrom->eval;
                                               /************ actual operator */
  memcpy(newChroms[curNumOff]->chrom,parentChrom->chrom,sizeof(chromType));
  if (which==0)                                          /* mutate firstGene */
    dP=&((chromType*)newChroms[curNumOff]->chrom)->firstGene;
  else
    dP=&(((chromType*)newChroms[curNumOff]->chrom)->twoMoreGenes[which-1]);
  if (binRand())  
    (*dP)+=probRand()*(1+which-(*dP)); 
  else
    (*dP)-=probRand()*(*dP-which);
                                               /******************************/
  for (i=0; i<numOffspring; i++)         /* an example, here numOffspring==1 */
    finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParent:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\tOffspring=%f\n\t",newChroms[curNumOff-1]->eval);
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  printf("\n");
#endif
  return(numOffspring);
}
 
/* average, two parents, one offspring */
static int ave(genericChromType *parentChrom,
                 genericChromsType newChroms, int maxNumOff, int curNumOff)
{ int i;
  genericChromType *parent2Chrom=getParent();
  static const int operNum=1;
  static const int numParents=2;
  static const int numOffspring=1;
  chromType *offspring=(chromType*)newChroms[curNumOff]->chrom;
  chromType *p1=(chromType*)parentChrom->chrom;
  chromType *p2;
  if (maxNumOff<=0)
    return(0);
  while (parent2Chrom==parentChrom)
    parent2Chrom=getParent();                    /* do not cross with itself */
  p2=(chromType*)parent2Chrom->chrom;
                                                  /********* actual operator */
  offspring->firstGene=(p1->firstGene+p2->firstGene)/2;
  offspring->twoMoreGenes[0]=(p1->twoMoreGenes[0]+p2->twoMoreGenes[0])/2;
  offspring->twoMoreGenes[1]=(p1->twoMoreGenes[1]+p2->twoMoreGenes[1])/2;
                                                  /***************************/
  for (i=0; i<numOffspring; i++)
    finishOper(newChroms[curNumOff],operNum,&curNumOff,numParents,parentChrom,
             parent2Chrom);
#if DEBUG
  printf("\nApplied operator %s\n",getOperName(operNum));
  printf("\tParents:\n\t");
  dspChrom(stdout,parentChrom->chrom);
  printf("\t");
  dspChrom(stdout,parent2Chrom->chrom);
  printf("\tOffspring=%f\n\t",newChroms[curNumOff-1]->eval);
  dspChrom(stdout,newChroms[curNumOff-1]->chrom);
  printf("\n");
#endif
  return(numOffspring);
}

#define NUMOPERS 2
static char *OperNames[NUMOPERS]={"random mutation on one gene",
                                  " average of two chroms"};
static operAddrType OperAddrs[NUMOPERS]={mut,ave};

char *getOperName(int operNum)
{ if (operNum>NUMOPERS)
    FAIL("");
  return(OperNames[operNum]);
}

operAddrType getOperAddr(int operNum)
{ if (operNum>NUMOPERS)
    FAIL("");
  return(OperAddrs[operNum]);
}
 
int getNumOpers(void)
{ return(NUMOPERS);
}

/* makeChrom() allocates storage for a chrom and initializes it              */
genericChromType *makeChrom(FILE *fp)
{ chromType *chrom;
  chrom=(chromType*)getStorage(1,sizeof(chromType));
  initChrom((genericChromType*)chrom,fp);
  return((genericChromType*)chrom);
}

void initChrom(genericChromType *c, FILE *fp)
{ chromType *chrom=(chromType*)c;
  chrom->firstGene=probRand();                                       /* 0..1 */
  chrom->twoMoreGenes[0]=probRand()+1;                               /* 1..2 */
  chrom->twoMoreGenes[1]=probRand()+2;                               /* 2..3 */
}

/* dspChrom() is provided if desired to display chromosomes                  */
void dspChrom(FILE *fp, void *c)
{ chromType *chrom=(chromType*)c;
  fprintf(fp,"Chrom:");
  fprintf(fp," %f %f %f\n",chrom->firstGene,chrom->twoMoreGenes[0],
         chrom->twoMoreGenes[1]);
}

void setOperSpecifics(FILE *fp, int oprNum)
{ return;                                                  /* nothing to set */
}

void setRepSpecifics(FILE *fp)
{ return;                                                    /* nothing here */
}
