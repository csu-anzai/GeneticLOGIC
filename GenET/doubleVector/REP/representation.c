/* rep.c includes allocate storage and display chrom functions for a given */
/* representation */
/* This is a simple example representation */

#include <stdio.h>
#include <stddef.h>
#include <values.h>
#include <limits.h>
#include <string.h>
#include "main.h"
#include "io.h"
#include "error.h"
#include "storage.h"
#include "pop.h"              /* includes generic chromosome type definition */
#include "representation.def"
#include "representation.h"
#include "opers.h"
#include "problem.h"

#ifndef MAXDOUBLE
#define MAXDOUBLE DBL_MAX
#endif

int NumGenes;                       /* vector length, non-standard interface */
double *LowerDom;                   /* lower domains, non-standard interface */
double *UpperDom;                   /* upper domains, non-standard interface */

/* setRepSpecifics() is used to set up any other information specific        */
/* to a representation - here, number of genes and domains                   */
void setRepSpecifics(FILE *fp)
{ 
#define NUMLEN 10
  char number[NUMLEN];
  int i;
  
  if (getIntLtd(fp,"Give number of genes",0,INT_MAX,&NumGenes)!=0)
    FAIL("");
  LowerDom=makeDoubleVector(NumGenes,(double)0);
  UpperDom=makeDoubleVector(NumGenes,(double)0);
  for (i=0; i<NumGenes; i++)
  { if (fp==stdin)
      printf("Gene number %d:\n",i);
    if (getDoubleLtd(fp,"\tGive lower boundary",-MAXDOUBLE,MAXDOUBLE,
                     &LowerDom[i])!=0)
      FAIL("");
    if (getDoubleLtd(fp,"\tGive upper boundary",LowerDom[i],MAXDOUBLE,
                     &UpperDom[i])!=0)
      FAIL("");
  }
}

/* makeChrom() allocates storage for a chrom and initializes it by calling   */
/*   problem-specific function initChrom()                                   */
genericChromType *makeChrom(FILE *fp)
{ double *chrom;
  chrom=(double*)makeDoubleVector(NumGenes,(double)0);
  initChrom((genericChromType*)chrom,fp);
  return((genericChromType*)chrom);
}

/* dspChrom() is provided if desired to display chromosomes */
void dspChrom(FILE *fp, void *c)
{ int i;
  double *chrom=(double*)c;
  fprintf(fp,"Chrom:\t");
  for (i=0; i<NumGenes; i++)
    fprintf(fp,"%f\t",chrom[i]);
  fprintf(fp,"\n");
}

