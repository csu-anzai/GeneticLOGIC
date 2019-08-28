/* pop.c contains population and its roulette:
There are two model choices: generational and operational
Generational model: selection used for new population, then operators
  Currently not implemented, but may be simulated with the other model
  and a copy operator for the selection
Operational model: operators w/ selection produce new population
  One generation (call to doGeneration()) does:
  1. From current population select parents and produce new pop
     Each parent can be selected randomlyly or by roulette
     Each parent selects an operator from its roulette and applies
       to produce any number of offspring (depending on operator)
  2. New pop, or new pop merged with pop of parents, are reduced to
       original population size by razor, randomly, or roulette
     If only new population is used, then new pop >= pop
     
There is a choice of ranks or true evaluations. When rank is used, selection 
  is done as: r=rInit*(1-prctRun^RExp), eval[i]=r^i 
There is a choice of preserving best chrom 
Negative evaluations do not matter when ranks are used. Otherwise,
  1. If max eval is still negative, then increase each eval by -(min+max)
  2. Else, increase each eval by -min                                        */
/* NOTE: roulettes are not initialized here                                  */
/* NOTE: assumes all chroms in Pop and NewPop and evaluated externally       */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "main.h"
#include "storage.h"
#include "roulette.h"
#include "error.h"
#include "io.h"
#include "mergeSorted.h"
#include "timing.h"
#include "pop.h"
#include "opers.h"
#include "rand.h"
#include "representation.h"
#include "problem.h"

#define DEBUG 0
#define DEBUG_generation 0
#define MINPOP 1
#define MAXPOP 10000      /* arbitrary high limit since no storage allocated */
                                            /* prior to reading actual sizes */
#define MINNEWPOP 1
#define MAXNEWPOP 10000
#define MINRINIT 0.001     /* the smaller, the stronger the initial pressure */
#define MINREXP 0.001
#define MAXREXP 10
#define RANK_SIM_LEN 50             /* how long simulation for dumping ranks */

typedef struct 
        { int popSize;                                    /* size after kill */
          int maxPopSize;         /* max number of offspring for NewPop only */
          int bestChrom;   /* always 0 if sorted, selected else - always set */
          double addToEval;/* used when evals used for roulette - always set */
          genericChromsType chroms;
        } popType;
typedef enum {randomly, roulette, razor} selectionType;
char *SelectionTypeNames[]={"randomly","roulette","razor"};

/************** these parameters control the model ***************************/
static selectionType PopToNew;                  /* randomly or roulette only */
static selectionType BackToPop;              /* randomly, roulette, or razor */
static boolean BothPopAndNewPopToPop;  /* alternative is only new pop -> pop */
static boolean UseRanks;
static boolean PreserveBest;
static double RInit;              /* initial prob. for ranked selection, <=1 */
static double RExp;                     /* exp. for changes of ranked probs, */
static popType Pop;             /* Pop.popSize+NewPop.maxPopSize pointers to */
                  /* genericChromType, but only Pop.popSize chroms allocated */
static popType MergedPop;                                  /* used for merge */
static popType NewPop;                        /* NewPop.maxPopSize allocated */
static void *ParentRoulette;               /* roulette for selecting parents */
static double *ParentEvals;    /* used to provide actual values for roulette */
static void *BackToPopRoulette;  /* roulette for selecting pop from pop/+new */
static double *BackToPopEvals;    /* same as PerentEvals, but for gener. pop */
         /* size is new pop size if BothPopAndNewPopToPop==false, else it is */
                                                  /* pop size + new pop size */
static boolean *SelectedChromFlags;   /* for selecting w/o repl. back to pop */
static boolean Minimalization;           /* true for minimalization problems */
static int (*Compr)(const void*,const void*);   /* asc() or desc() as needed */
                   /* use asc when Minimalization (sorts in ascending order) */
static int NumUsedOpers;            /* number of operators used for this run */
static boolean GenerationalModel;                   /* GA model if true; otherwise, ES */
                                 

/*************** Implementation notes: ****************************************
1. Pop is allocated pop_size generic chrom ptrs with actual chroms
   allocated (initially evaluated and, if necessary, sorted)
2. NewPop is allocated new_pop_size generic pointers with actual chroms
   allocated
3. ChromCopy is allocated with only generic pointers (no chrom storage)
   Its size is pop_szie+new+pop_size when BothPopAndNewPopToPop==true
   otherwise it is not allocated and is unused.
   If actually allocated, then SelectedChromsFlags is allocated of same size
   else this flag is allocated of size new_pop_size 
On a generation:
   1 Create new_pop_size offspring filling the storage in NewPop
     (no storage allocation - storage allocated dynamically at setPop())
     This continues until all chromosomes in NewPop are filled 
     Parents are selected according to PopToNew type (randomly or roulette)
     Each parent selects an operator from its individual oper. roulette
   2 if BothPopNadNewPopToPop==true then 
       NewPop and Pop merge all their actual chroms into MergedPop
       (moving pointers only), which is then used to select pop_size 
       chroms w/o replacement. Selection is w/o replacement with
       SelectedChromFlags true entries marking those selected
       After enough chroms is marked, then they replace (ptrs only)
       those in Pop and flags are reset to false
     else (only NewPop gives chroms back to pop)
       skip merge, repeat the same selection but from NewPop
All evaluations in the chromosomes are true evaluations. For minimalization
(Minimalization==true) problems, bestChrom will have index of the minimal
eval, else of the maximal. If sorting done, sorting is such that the bestChrom
is always 0 - ascending for minimalization (Compr==asc), else descending
(Compr==desc). 
When roulettes are used with evaluations:
   1 If Minimalization==true then each evaluation is negated
   2 Each evaluation is always increased by .addToEval
       .addToEval=0 if min eval>=0
       else if max eval >=0, .addToEval=-(min eval) 
       else .addToEval=-(min+max evals)

Dependent parameters and actions carried in different models:

BothPopAndNewPopToPop==false (pop -> new; new -> pop [new size >= pop size])
  PopToNew==randomly (select randomly parents)
    BackToPop==randomly (select randomly for survival)
      UseRank==true    : UseRank forced to false in this setting (irrelevant)
      UseRank==false   : Pop and NewPop= not sorted, evals/roulettes not set
    BackToPop==roulette (select from eval roulette for survival)
      UseRank==true    : Pop= not sorted, ParentEvals/roulette not set
                         NewPop= sorted, BackToPopEvals/roulette set with ranks 
      UseRank==false   : Pop= same as UseRank==true
                         NewPop= not sorted, BackToPopEvals/roulette set with 
                         evals
    BackToPop==razor (select strongest for survival)
      UseRank==true    : UseRank forced to false in this setting (irrelevant)
      UseRank==false   : Pop= not sorted, ParentEvals/roulette not set
                         NewPop= sorted, BackToPopEvals/roulette not set
  PopToNew==roulette (select parents from roulette)
    BackToPop==randomly
      UseRank==true    : Pop= sorted, PerentEvals/roulette set with ranks
                         NewPop= not sorted, BackToPopEvals/roulette not set
      UseRank==false   : Pop= not sorted, ParentEvals/roulette set with evals
                         NewPop= same as with UseRank==true
    BackToPop==roulette
      UseRank==true    : Pop and NewPop= sorted, evals/roulettes set with ranks
      UseRank==false   : Pop and NewPop= not sorted, evals/roulettes set with 
                         evals
    BackToPop==razor   
      UseRank==true    : Pop= sorted, ParentEvals/roulette set with ranks
                         NewPop= sorted, BackToPopEvals/roulette not set
      UseRank==false   : Pop= not sorted, parentEvals/roulette set with evals
                         NewPop= same as with UseRanks==true

BothPopAndNewPopToPop==true (pop -> new; pop+new -> pop)
  PopToNew==randomly (select randomly parents)
    BackToPop==randomly (select randomly for survival)
      UseRank==true    : UseRank forced to false in this setting (irrelevant)
      UseRank==false   : Pop= not sorted, ParentEvals/roulette not set
                         NewPop= not sorted
                         merge w/o sort, BackToPopEvals/roulette not set
    BackToPop==roulette (select from eval roulette for survival)
      UseRank==true    : Pop= sorted, ParentEvals/roulette not set
                         NewPop= sorted
                         merge sorted, BackToPopEvals/roulette set with ranks
      UseRank==false   : Pop= not sorted, ParentEvals/roulette not set
                         NewPop= not sorted
                         merge w/o sort, BackToPopEvals/roulette set with evals
    BackToPop==razor (select strongest for survival)
      UseRank==true    : UseRank forced to false in this setting (irrelevant)
      UseRank==false   : Pop= sorted, ParentEvals/roulette not set
                         NewPop= sorted
                         merge sorted, BackToPopEvals/roulette not set
  PopToNew==roulette (select parents from roulette)
    BackToPop==randomly
      UseRank==true    : Pop= sorted, ParentEvals/roulette set with ranks
                         NewPop= not sorted
                         merge w/o sort, BackToPopEvals/roulette not set
      UseRank==false   : Pop= not sorted, ParentEvals/roulette set with evals
                         NewPop= not sorted
                         merge w/o sort, BackToPopEvals/roulette not set
    BackToPop==roulette
      UseRank==true    : Pop and NewPop= sorted, ParentEvals/roulette set with 
                         ranks
                         merge sorted, BackToPopEvals/roulette set with ranks
      UseRank==false   : Pop and NewPop= sorted, ParentEvals/roulette set with 
                         evals
                         merge sorted, BackToPopEvals/roulette set with evals
    BackToPop==razor   
      UseRank==true    : Pop= sorted, ParentEvals/roulette set with ranks
                         NewPop= sorted
                         merge sorted, BackToPopEvals/roulette not set
      UseRank==false   : Pop= sorted, ParentEvals/roulette set with evals
                         NewPop= sorted
                         merge sorted, BackToPopEvals/roulette not set

NOTE: when PreserveBest==true ensure best from new (or pop+new) survives. This
  will be accomplished by selecting the best on last survival selection if
  the best was not selected yet
NOTE: in cases when roulette is not set, either randomly selection or razor_cut
  will be called upon
NOTE: ParentEvals is always set with evaluations from Pop; BackToPopEvals is
  set with evals from NewPop or from merged MergedPop based on the value
  of BothPopAndNewPopToPop (false/true)
******************************************************************************/

/*************** Dependent parameters to simplify processing *****************/
static boolean SortPop;                     /* true when sort needed for Pop */
static boolean SetParentEvalsRoulette;
static boolean SetParentEvalsRoulette_ranks;  
                              /* used only when SetParentEvalsRoulette==true */
                                      /* true when use ranks, else use evals */
static boolean SortNewPop;
static boolean SetBackToPopEvalsRoulette;
static boolean SetBackToPopEvalsRoulette_ranks;
static boolean SortOnMerge;

/*********** rest parameters is to control i/o operations ********************/
static boolean DspModOperProbs;       /* display to stdout on modifications? */
static boolean DspModOperProbsSet=false;       /* for proper execution order */
static FILE *FDumpModOperProbs=NULL;           /* if opened, dump them there */
static boolean DspBestChrom;           /* display to stdout on isDspChrom()? */
static boolean DspBestEval;            /* display to stdout on isDspChrom()? */
static boolean DspChromEvalSet=false;
static FILE *FDumpBestEval;               /* if opened, dump best evaluation */
static FILE *FDumpBestChrom;                   /* if opened, dump best chrom */
static boolean DspEvalEachImprovement;


/* desc(a,b) : return 1 if a<b, 0 if equal, -1 otherwise                     */
/*   this will cause descending ordering on qsort()                          */
static int desc(const void *a, const void *b)
{ genericChromType **AA=(genericChromType**)a;
  genericChromType **BB=(genericChromType**)b;
  genericChromType *A=*AA;
  genericChromType *B=*BB;
  return(A->eval<B->eval ? 1 : (A->eval>B->eval ? -1 : 0));
}

/* asc(a,b) : return 1 if a>b, 0 if equal, -1 otherwise                      */
static int asc(const void *a, const void *b)
{ genericChromType **AA=(genericChromType**)a;
  genericChromType **BB=(genericChromType**)b;
  genericChromType *A=*AA;
  genericChromType *B=*BB;
  return(A->eval>B->eval ? 1 : (A->eval<B->eval ? -1 : 0));
}

/* printPop() for debugging, dummy function                                  */
static void printPop(genericChromsType chroms, int popSize, int bestI, 
                     double addToEval, const char *c)
{ int i,j;
  printf("%s: popSize=%2d bestChrom=%2d addToEval=%8.5f\n",c,popSize,bestI,
         addToEval);
  for (i=0; i<popSize; i++)
  { if (chroms[i]==NULL)
      continue;
    printf("%3d: \teval=%.5f @%p\n\t",i,chroms[i]->eval,
           (void*)chroms[i]);
    dspChrom(stdout,chroms[i]->chrom);
    printf("\tOperProbs OperApplCounter OperApplSum\n");
    for (j=0; j<NumUsedOpers; j++)
      printf("\t%9.5f %15.5f %11.5f\n",chroms[i]->operProbs[j],
             chroms[i]->operApplCounter[j],chroms[i]->operApplCounterTotal);
    printf("\t");
    dspRoulette(chroms[i]->roulette);
  }
}

void dspPop(void)
{ printPop(Pop.chroms,Pop.popSize,Pop.bestChrom,Pop.addToEval,"Population");
  if (SetParentEvalsRoulette==true)
  { printf("  ");
    dspRoulette(ParentRoulette);
  }
}

static void dspNewPop(void)
{ printPop(NewPop.chroms,NewPop.popSize,NewPop.bestChrom,
           NewPop.addToEval,"Offspring population");
  if (BothPopAndNewPopToPop==false && SetBackToPopEvalsRoulette==true)
  { printf("  ");
    dspRoulette(BackToPopRoulette);
  }
}

static void dspMergedPop(void)
{ if (MergedPop.chroms==NULL)
    FAIL("Merge not used");
  printPop(MergedPop.chroms,MergedPop.popSize,MergedPop.bestChrom,
           MergedPop.addToEval,"Merged population");
  if (BothPopAndNewPopToPop==true && SetBackToPopEvalsRoulette==true)
  { printf("  ");
    dspRoulette(BackToPopRoulette);
  }
} 

void setDspChromEval(FILE *fp, boolean use)
{ int what;
  char file[NAMESIZ];
  DspChromEvalSet=true;
  if (use==false)
  { DspBestChrom=false;
    DspBestEval=false;
    FDumpBestChrom=NULL;
    FDumpBestEval=NULL;
    return;
  }
  if (getIntLtd(fp,"Do you want to display best eval to stdout?",
                0,1,(int*)&DspBestEval)!=0)   
    FAIL("");
  if (getIntLtd(fp,"Do you want to dump best eval to a file?",
                0,1,&what)!=0)   
    FAIL("");
  if (what==1)
  { if (getStrLtd(fp,"Give dump file name",file,1,NAMEMAX)!=0)
      FAIL("");
    if ((FDumpBestEval=fopen(file,"r"))!=NULL)
    { tmpnam(file);
      fprintf(stderr,"WARNING: that file exists. Using %s instead%c\n",file,7);
    }
    if ((FDumpBestEval=fopen(file,"w"))==NULL)
      FAIL("Could not open dump file");
  }
  if (getIntLtd(fp,"Do you want to display best chrom to stdout?",
                0,1,(int*)&DspBestChrom)!=0)   
    FAIL("");
  if (getIntLtd(fp,"Do you want to dump best chrom to a file?",
                0,1,&what)!=0)   
    FAIL("");
  if (what==1)
  { if (getStrLtd(fp,"Give dump file name",file,1,NAMEMAX)!=0)
      FAIL("");
    if ((FDumpBestChrom=fopen(file,"r"))!=NULL)
    { tmpnam(file);
      fprintf(stderr,"WARNING: that file exists. Using %s instead%c\n",file,7);
    }
    if ((FDumpBestChrom=fopen(file,"w"))==NULL)
      FAIL("Could not open dump file");
  }
}

void setDspModOperProbs(FILE *fp, boolean use)
{ int what,i;
  char file[NAMESIZ];
  DspModOperProbsSet=true;
  if (use==false)
  { DspModOperProbs=false;
    FDumpModOperProbs=NULL;
    return;
  }
  if (getIntLtd(fp,"Do you want to display modified probabilities to stdout?",
                0,1,(int*)&DspModOperProbs)!=0)   
    FAIL("");
  if (getIntLtd(fp,"Do you want to dump modified probabilities to a file?",
                0,1,&what)!=0)   
    FAIL("");
  if (what==1)
  { if (getStrLtd(fp,"Give dump file name",file,1,NAMEMAX)!=0)
      FAIL("");
    if ((FDumpModOperProbs=fopen(file,"r"))!=NULL)
    { tmpnam(file);
      fprintf(stderr,"WARNING: that file exists. Using %s instead%c\n",file,7);
    }
    if ((FDumpModOperProbs=fopen(file,"w"))==NULL)
      FAIL("Could not open dump file");
    for (i=0; i<getNumUsedOpers(); i++)
      fprintf(FDumpModOperProbs,"%s\t",getUsedOperName(i));
    fprintf(FDumpModOperProbs,"\n");
  }
}
                       
/* setDependentParameters() - see Implementation notes                       */
static void setDependentParameters(void)
{  if (BothPopAndNewPopToPop==false)
  { SortOnMerge=false;            /* actually, not merge at all in this case */
    if (PopToNew==randomly)
    { switch (BackToPop)
      { case randomly:   if (UseRanks==true)
                       { FAIL("Use ranks irrelevant, force to false");
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
        case roulette: if (UseRanks==true)
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=true;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
        case razor:    if (UseRanks==true)
                       { FAIL("UseRank irrelevant, force to false");
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
      } 
    }
    else                                               /* PopToNew==roulette */
    { switch (BackToPop)
      { case randomly:   if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
        case roulette: if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=true;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
        case razor:    if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                       }
                       break;
      } 
    }
  }
  else                                        /* BothPopAndNewPopToPop==true */
  { if (PopToNew==randomly)
    { switch (BackToPop)
      { case randomly:   if (UseRanks==true)
                       { FAIL("UseRank irrelevant, force to false");
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=false;
                       }
                       break;
        case roulette: if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=true;
                         SortOnMerge=true;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=false;
                       }
                       break;
        case razor:    if (UseRanks==true)
                       { FAIL("UseRank irrelevenat, force to false");
                       }
                       else
                       { SortPop=true;
                         SetParentEvalsRoulette=false;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=true;
                       }
                       break;
      } 
    }
    else                                               /* PopToNew==roulette */
    { switch (BackToPop)
      { case randomly:   if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=false;
                       }
                       else
                       { SortPop=false;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=false;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=false;
                       }
                       break;
        case roulette: if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=true;
                         SortOnMerge=true;
                       }
                       else
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=true;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=true;
                       }
                       break;
        case razor:    if (UseRanks==true)
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=true;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=true;
                       }
                       else
                       { SortPop=true;
                         SetParentEvalsRoulette=true;
                         SetParentEvalsRoulette_ranks=false;
                         SortNewPop=true;
                         SetBackToPopEvalsRoulette=false;
                         SetBackToPopEvalsRoulette_ranks=false;
                         SortOnMerge=true;
                       }
                       break;
      } 
    }
  } 
#if DEBUG
  printf("\n");
  printf("\nSortPop=%s",SortPop==true ? "true" : "false");
  printf("\nSetParentEvalsRoulette=%s",SetParentEvalsRoulette==true ? "true" :
         "false");
  printf("\nSetParentEvalsRoulette_ranks=%s",SetParentEvalsRoulette_ranks==true ? 
         "true" : "false");
  printf("\nSortNewPop=%s",SortNewPop==true ? "true" : "false");
  printf("\nSetBackToPopEvalsRoulette=%s",SetBackToPopEvalsRoulette==true ? 
         "true" : "false");
  printf("\nSetBackToPopEvalsRoulette_ranks=%s",
         SetBackToPopEvalsRoulette_ranks==true ? "true" : "false");
  printf("\nSortOnMerge=%s\n",SortOnMerge==true ? "true" : "false");
#endif
  if (SortOnMerge==true && (SortPop==false || SortNewPop==false)
      || SetParentEvalsRoulette_ranks==true && SortPop==false 
      || SetBackToPopEvalsRoulette_ranks==true && SortNewPop==false)
    FAIL("Check dependent parameters");                 /* just a self-check */
}

static void dumpSampleRanks(FILE *fp)
{ int i,j;
  double r;
  char file[NAMESIZ];
  FILE *fDump;
  if (getStrLtd(fp,"Give dump file name",file,1,NAMEMAX)!=0)
    FAIL("");
  if ((fDump=fopen(file,"r"))!=NULL)
  { tmpnam(file);
    fprintf(stderr,"WARNING: that file exists. Using %s instead%c\n",file,7);
  }
  if ((fDump=fopen(file,"w"))==NULL)
    FAIL("Could not open dump file");
  fprintf(fDump,"%d iteration samples, %d chrom probs; RInit=%f RExp=%f\n",
          RANK_SIM_LEN,Pop.popSize,RInit,RExp);
  for (i=0; i<RANK_SIM_LEN; i++)      /* illustrative samples will be dumped */
  { r=RInit*(1-pow((double)i/RANK_SIM_LEN,RExp));
    for (j=0; j<Pop.popSize; j++)
      fprintf(fDump,"%f\t",pow(r,(double)j));
    fprintf(fDump,"\n");
  }
  fclose(fDump);
}

/* findAddToEval(min,max) finds add factor to avoid negative evaluations     */
/*   when evaluations are used for roulettes - see Implementation notes      */
static double findAddToEval(double minEval, double maxEval)
{ if (minEval<0)
    if (maxEval<0)
      return(-(minEval+maxEval));
    else
      return(-minEval);
  else
    return(0.0);
}

/* setBestAndAddToEval(aPop) returns index of best chrom                     */
/*   The best may be needed fro Pop, and/or from                             */
/*     NewPop if BothPopAndNewPopToPop==false, and from MergedPop otherwise  */
/* NOTE: best is maximal eval if Minimalization==false, else it is min eval  */
/* NOTE: may assume aPop is sorted if needed to speed up search              */
/* NOTE: also set up addToEval value - see Implementation notes              */
static void setBestAndAddToEval(popType *aPop)
{ register int i;
  if (Minimalization==true)                 /* if sorted, order is ascending */
  {       /* NOTE: min and max evals will be negated when used for rouelette */
    if (aPop==&Pop && SortPop==true
        || aPop==&NewPop && SortNewPop==true
        || aPop==&MergedPop && SortOnMerge==true)   /* assume aPop is sorted */
    { aPop->bestChrom=0; 
      aPop->addToEval=findAddToEval(-aPop->chroms[0]->eval,
                    -aPop->chroms[aPop->popSize-1]->eval);
    }
    else                 /* no shortcut - must search for min and max values */
    { int minI=0;                                          /* the best index */
      double minEval, maxEval;
      minEval=maxEval=aPop->chroms[0]->eval;
      for (i=1; i<aPop->popSize; i++)
      { if (aPop->chroms[i]->eval<minEval)
        { minI=i;
          minEval=aPop->chroms[i]->eval;
        }
        else if (aPop->chroms[i]->eval>maxEval)
               maxEval=aPop->chroms[i]->eval;
      }
      aPop->bestChrom=minI;
      aPop->addToEval=findAddToEval(-maxEval,-minEval);
    }
  }
  else                                     /* if sorted, order is descending */
  { if (aPop==&Pop && SortPop==true
        || aPop==&NewPop && SortNewPop==true
        || aPop==&MergedPop && SortOnMerge==true)   /* assume aPop is sorted */
    { aPop->bestChrom=0; 
      aPop->addToEval=findAddToEval(aPop->chroms[aPop->popSize-1]->eval,
                    aPop->chroms[0]->eval);
    }
    else                 /* no shortcut - must search for min and max values */
    { int maxI=0;                                          /* the best index */
      double minEval, maxEval;
      minEval=maxEval=aPop->chroms[0]->eval;
      for (i=1; i<aPop->popSize; i++)
      { if (aPop->chroms[i]->eval>maxEval)
        { maxI=i;
          maxEval=aPop->chroms[i]->eval;
        }
        else if (aPop->chroms[i]->eval<minEval)
               minEval=aPop->chroms[i]->eval;
      }
      aPop->bestChrom=maxI;
      aPop->addToEval=findAddToEval(minEval,maxEval);
    }
  }
}

/* fillEvals(evals,chroms,popSize,addToEval) fills the evals vector with     */
/*   rank values when UseRanks==true; else use the evaluations from chroms   */
/* NOTE: Evaluations are adjusted by negating when Mimimalization==true and  */
/*   then always by adding addToEvals                                        */
/* NOTE: assume called only when needed and entries are sorted if needed     */
static void fillEvals(double *evals, genericChromsType chroms, int popSize,
                      double addToEval)
{ register int i;
  if (UseRanks==true)
  { double r;
    r=RInit*(1-pow(getCurPrctRun(),RExp));
    for (i=0; i<popSize; i++)
      evals[i]=pow(r,(double)i);
  }
  else if (Minimalization==true)
         for (i=0; i<popSize; i++)
           evals[i]=(-1)*chroms[i]->eval+addToEval;
       else
         for (i=0; i<popSize; i++)
           evals[i]=chroms[i]->eval+addToEval;
}

/* getGenerationalParameters() sets pop size for the generational model      */
void getGenerationalParameters(FILE *fp)
{ if (getIntLtd(fp,"Give population size",MINPOP,MAXPOP,&(Pop.popSize))!=0)
    FAIL("");
  NewPop.popSize=Pop.maxPopSize; 
  PopToNew=roulette;
  BackToPop=razor;                     /* does not matter, razor is cheapest */
  BothPopAndNewPopToPop=false;
  FAIL("");
}

/* getOperationalParameters() sets (n,m)ES with m>=n, or (n+m)ES            */
void getOperationalParameters(FILE *fp)
{ char name[NAMESIZ];
  if (getIntLtd(fp,"Give population size",MINPOP,MAXPOP,&(Pop.popSize))!=0)
    FAIL("");
   sprintf(name,"Give me parent selection kind %d:%s %d:%s",randomly,
          SelectionTypeNames[randomly],roulette,SelectionTypeNames[roulette]);
  if (getIntLtd(fp,name,randomly,roulette,(int*)(&PopToNew))!=0)
    FAIL("");
  if (getIntLtd(fp,"Do you want to use both parents and offspring to select\
 end population?",
                  0,1,(int*)(&BothPopAndNewPopToPop))!=0)
    FAIL("");
  if (getIntLtd(fp,"Give new population size",
                (BothPopAndNewPopToPop==true) ? MINNEWPOP : Pop.popSize,
                MAXNEWPOP,&(NewPop.maxPopSize))!=0)
    FAIL(""); 
  sprintf(name,"Give me end pop selection kind %d:%s %d:%s %d:%s",randomly,
          SelectionTypeNames[randomly],roulette,SelectionTypeNames[roulette],
          razor,SelectionTypeNames[razor]);
  if (getIntLtd(fp,name,randomly,razor,(int*)(&BackToPop))!=0)
    FAIL("");
}

/* setPop(fp) allocates storage for Pop, NewPop, and MergedPop and sets      */
/*   independent and dependent parameters                                    */
/* NOTE: call after setting operators and timing                             */
/* NOTE must evaluate Pop and set best and worst, also presort if neeed      */
void setPop(FILE *fp)
{ int i;
  NumUsedOpers=getNumUsedOpers();
  if (DspModOperProbsSet==false || DspChromEvalSet==false)
    FAIL("Wrong order in implementation, first set display");      /* safety */

                                           /* first read in model parameters */
  if (getIntLtd(fp,"Do you want to minimize?",0,1,(int*)(&Minimalization))!=0)
    FAIL("");
  if (Minimalization==true)
    Compr=asc;      /* will sort and merge in ascending order of evaluations */
  else
    Compr=desc;                   /* will sort and merge in descending order */
  if (getIntLtd(fp,"Do you want to display best eval on each improvement?",
                0,1,(int*)(&DspEvalEachImprovement))!=0)
    FAIL("");

  if (getIntLtd(fp,"Do you want 0:Operational 1:Generational model?",
                0,1,(int*)&GenerationalModel)!=0)
    FAIL("");
  if (GenerationalModel==true)
    getGenerationalParameters(fp);
  else
    getOperationalParameters(fp);
  if (PopToNew==randomly && BackToPop==randomly)
  { fprintf(stderr,"WARNING: you selected a completely randomly run%c\n",7);
    UseRanks=false;                    /* Irrelevant UseRank forced to false */
  }
  else if (PopToNew!=roulette && BackToPop!=roulette)
          UseRanks=false;   /* roulettes not needed - force UseRank to false */
       else if (getIntLtd(fp,"Do you want to use rank for probabilities?",0,1,
                (int*)(&UseRanks))!=0)
              FAIL("");

  if (UseRanks==true)
  { int dumpRanks;
    if (getDoubleLtd(fp,"Give initial weight for rank-based probs",
                     (double)MINRINIT,(double)1,&RInit)!=0)
      FAIL("");
    if (getDoubleLtd(fp,"Give rank exponent",
                     (double)MINREXP,(double)MAXREXP,&RExp)!=0)
      FAIL("");
    if (getIntLtd(fp,"Do you want to dump rank samples to a file?",
                  0,1,(int*)(&dumpRanks))!=0)     
      FAIL("");
    if (dumpRanks==true)                  /* means want to dump rank samples */
      dumpSampleRanks(fp);
  }
  if (!(BothPopAndNewPopToPop==true && BackToPop==razor))
  { if (getIntLtd(fp,"Do you want to preserve best chrom?",0,1,
                (int*)(&PreserveBest))!=0)
      FAIL("");
  }
  else
    PreserveBest=true;    /* actually, best will automatically be preserved  */

  setDependentParameters();
                        
                       /* now allocate Evals vectors and Roulletes as needed */
  if (SetParentEvalsRoulette==true)
  { ParentEvals=makeDoubleVector(Pop.popSize,(double)0);
    ParentRoulette=createRoulette(ParentEvals,Pop.popSize);
  }
  else
  { ParentEvals=NULL;
    ParentRoulette=NULL;
  }
  if (SetBackToPopEvalsRoulette==true)
  { int size;
    size=(BothPopAndNewPopToPop==true ? Pop.popSize+NewPop.maxPopSize :
          NewPop.maxPopSize);
    BackToPopEvals=makeDoubleVector(size,(double)0);
    BackToPopRoulette=createRoulette(BackToPopEvals,size);
  }
  else
  { BackToPopEvals=NULL;
    BackToPopRoulette=NULL;
  }

   /* now allocate Pop of proper size with proper number of chroms allocated */
                            /* and evaluated, and bestChrom and minChrom set */
  Pop.chroms=(genericChromsType)getStorage(Pop.popSize,
                                           sizeof(genericChromType*));
  for (i=0; i<Pop.popSize; i++)    /* only that many chroms always allocated */
  { Pop.chroms[i]=(genericChromType*)getStorage(1,sizeof(genericChromType));
    Pop.chroms[i]->chrom=makeChrom(fp);                  /* problem-specific */
    Pop.chroms[i]->eval=evalChrom(Pop.chroms[i]->chrom); /* problem-specific */
    Pop.chroms[i]->operProbs=makeDoubleVector(NumUsedOpers,(double)0);
    Pop.chroms[i]->operApplCounter=makeDoubleVector(NumUsedOpers,(double)0);
    Pop.chroms[i]->operApplCounterTotal=0;
    Pop.chroms[i]->roulette=createInitOperRoulette();
    initOperProbs(Pop.chroms[i]->operProbs);           /* sets initial probs */
  }
  if (SortPop)                                               /* must presort */
    qsort((void*)Pop.chroms,(size_t)Pop.popSize,sizeof(genericChromType*),
          Compr);
  Pop.maxPopSize=Pop.popSize;                         /* always equal in Pop */
  setBestAndAddToEval(&Pop);                 /* set bestChrom and addToEvals */
  
                                      /* fill initial pop roulette if needed */
  if (SetParentEvalsRoulette==true)
  { if (SetParentEvalsRoulette_ranks==true)
      for (i=0; i<Pop.popSize; i++)
        ParentEvals[i]=pow(RInit,(double)i);    /* use ranks for evaluations */
    else                          /* use plain evaluations; each must be >=0 */
      fillEvals(ParentEvals,Pop.chroms,Pop.popSize,Pop.addToEval);
    setRoulette(ParentRoulette,ParentEvals);             /* initial roulette */
  }
#if DEBUG
  printf("****************** After setPop **********\n");
  dspPop();
  printf("****************** End of after setPop ***\n");
#endif

                   /* now allocated generic ptrs in MergedPop only if needed */
  if (BothPopAndNewPopToPop==true)
  { MergedPop.popSize=MergedPop.maxPopSize=Pop.popSize+NewPop.maxPopSize;
    MergedPop.chroms=(genericChromsType)getStorage(MergedPop.popSize,
                       sizeof(genericChromType));
  }
  else                                         /* MergedPop will not be used */
  { MergedPop.popSize=MergedPop.popSize=0;
    MergedPop.chroms=NULL;
  }

       /* now allocate NewPop with maxPopSize of completely allocated chroms */
  NewPop.popSize=0;
  NewPop.chroms=(genericChromsType)getStorage(NewPop.maxPopSize,
                                           sizeof(genericChromType*));
  for (i=0; i<NewPop.maxPopSize; i++)
  { NewPop.chroms[i]=(genericChromType*)getStorage(1,sizeof(genericChromType));
    NewPop.chroms[i]->chrom=makeChrom(fp);            /* just to get storage */
    NewPop.chroms[i]->operProbs=makeDoubleVector(NumUsedOpers,(double)0);
    NewPop.chroms[i]->operApplCounter=makeDoubleVector(NumUsedOpers,(double)0);
    NewPop.chroms[i]->roulette=createInitOperRoulette(); 
  }
                                          
                                          /* now allocate SelectedChromFlags */
  if (BothPopAndNewPopToPop==true)
    SelectedChromFlags=makeBooleanVector(MergedPop.popSize,false);
  else
    SelectedChromFlags=makeBooleanVector(NewPop.maxPopSize,false);
}

genericChromType *getParent(void)
{ int parent;
  if (PopToNew==roulette)                          /* get parent by roulette */
    parent=spinRoulette(ParentRoulette);
  else                                                    /* randomly parent */
    parent=intRand(Pop.popSize-1); 
  return(Pop.chroms[parent]);
}

static void swapChroms(genericChromType **a, genericChromType **b)
{ genericChromType *p;
  p=*a;
  *a=*b;
  *b=p;
}

/* doGenerationOperational() performs one generation in operational model,   */
/*   that is model where offspring go to new population (like ES)            */
/* (n,m)-ES: create m offspring from n parents, then select n from m (m>=n)  */
/* (n+m)-ES: create m offspring from n parents, then select n from n+m       */
static void doGenerationOperational(void)
{ int whichOper;
  static genNumber=1;
  genericChromType *parentChrom;
  popType *sourcePop;
  while (NewPop.popSize<NewPop.maxPopSize)               /* create offspring */
  { parentChrom=getParent();
    whichOper=spinRoulette(parentChrom->roulette);
    NewPop.popSize+=fireOper(whichOper,parentChrom,NewPop.chroms,
                                NewPop.maxPopSize-NewPop.popSize,
                                NewPop.popSize);
  }
  if (SortNewPop==true)
    qsort((void*)NewPop.chroms,(size_t)NewPop.popSize,
            sizeof(genericChromType*),Compr);
#if DEBUG_generation
  printf("*********** doGeneration %d: after fireOper ********\n",genNumber++);
  dspPop();
  dspNewPop();
#endif
                         /* now select back population in the selected model */
  if (BothPopAndNewPopToPop==true)        /* merge to MergedPop, then select */
  { if (SortOnMerge==true)        /* NewPop is sorted - mergeSorted with Pop */
      mergeSorted((void*)MergedPop.chroms,(void*)Pop.chroms,
                  (void*)NewPop.chroms,(size_t)Pop.popSize,
                  (size_t)NewPop.popSize,sizeof(genericChromType*),Compr);
    else                                     /* just put them into MergedPop */
    { register int i;
      int j;
      for (i=0; i<Pop.popSize; i++)
        MergedPop.chroms[i]=Pop.chroms[i];
      for (j=0; j<NewPop.popSize; j++)
        MergedPop.chroms[i+j]=NewPop.chroms[j];
    }
    sourcePop=&MergedPop;                  /* use MergedPop to get survivals */
  }
  else                                        /* select straigth from NewPop */
    sourcePop=&NewPop;                        /* use NewPop to get survivals */

  if (PreserveBest==true || BackToPop!=randomly)
    setBestAndAddToEval(sourcePop); /* bestChrom or AddToEval will be needed */
  if (BackToPop==roulette)
  { fillEvals(BackToPopEvals,sourcePop->chroms,sourcePop->popSize,
              sourcePop->addToEval);
    setRoulette(BackToPopRoulette,BackToPopEvals);
                    /* Implement selection w/o replacement by roulette using */
                       /*  SelectedChromFlags and putting them back into Pop */
    FAIL("Sorry, back selection by roulette not finished");
  }
  else if (BackToPop==randomly)
               /* select randomly Pop.popSize numbers from sourcePop.popSize */
        /* w/o replacement using SelectedChromFlags and put them back to Pop */
         FAIL("Sorry, back selection randomly not finished");
       else                               /* razor cut - sourcePop is sorted */
       {      /* if BothPopAndNewPopToPop==true then PreserveBest irrelevant */
            /* since it is always preserved; if BothPopAndNewPopToPop==false */
          /* then if PreserveBest==true and  best in Pop is better than best */
          /* in source: Compr==(-1) then swap best and 0th in Pop, then swap */
                          /* Pop.popSize-1 from 0th of source to rest of Pop */
             /* otherwise, always swap Pop.popSize from 0th in source to Pop */
         register int i;
         if (BothPopAndNewPopToPop==false && PreserveBest==true
             && Compr((void*)(&(Pop.chroms[Pop.bestChrom])),
                      (void*)(&(sourcePop->chroms[0])))==(-1))
         { swapChroms(&(Pop.chroms[Pop.bestChrom]),&(Pop.chroms[0]));
           for (i=1; i<Pop.popSize; i++)
             swapChroms(&(Pop.chroms[i]),&(sourcePop->chroms[i-1]));
         }
         else if (BothPopAndNewPopToPop==false)
                for (i=0; i<Pop.popSize; i++)
                  swapChroms(&(Pop.chroms[i]),&(sourcePop->chroms[i]));
              else                            /* BothPopAndNewPopToPop==true */
              { int j;
                for (i=0; i<Pop.popSize; i++)
                  swapChroms(&(Pop.chroms[i]),&(sourcePop->chroms[i]));
                for (j=0; j<NewPop.popSize; j++)  
                  swapChroms(&(NewPop.chroms[j]),&(sourcePop->chroms[i+j]));
              }
       }
  NewPop.popSize=0;
  setBestAndAddToEval(&Pop);       /* always needed e.g. for display of best */
  if (SetParentEvalsRoulette==true)
  { fillEvals(ParentEvals,Pop.chroms,Pop.popSize,Pop.addToEval);
    setRoulette(ParentRoulette,ParentEvals);             /* initial roulette */
  }
#if DEBUG_generation
  dspPop();
  printf("************** doGeneration : end *********************\n");
#endif
}

static void doGenerationGenerational(void)
{ FAIL("Sorry, generational model not finished");
}

/* doGeneration(), does one generation according to model                    */
void doGeneration(void)
{ if (GenerationalModel==true)
    doGenerationGenerational();
  else
    doGenerationOperational();
}

/* dspAveOperProbs() displays average probabilities as used in roulette,     */
/*   which includes getAddProb() value to guarantee minimal probability      */
/*   actualProb[i]=(p[i]+addProb)/(1+addProb*NumUsedOpers)                       */
/* may display to stdout if DspModOperProbs==true and a file if              */
/*   FDumpModOperProbs!=NULL                                                 */
void dspAveOperProbs(void (*tryDspPrct)(void))
{ int i,j;
  double sum;
  double addProb, addProbDenum, stdDevSum;
  double actualProb;                    /* the one actually used in roulette */

  addProb=getAddProb();
  addProbDenum=1+addProb*NumUsedOpers;
  if (DspModOperProbs==false && FDumpModOperProbs==false) 
    return;                                            /* nothing to do here */
  if (DspModOperProbs==true)
  { tryDspPrct();                         /* display prct to stdout if neeed */
    printf("\tAverage operator probabilties\n");
  }
  for (i=0; i<NumUsedOpers; i++)
  { sum=0;
    for (j=0; j<Pop.popSize; j++)
      sum+=Pop.chroms[j]->operProbs[i];
    actualProb=(sum/Pop.popSize+addProb)/addProbDenum;
    if (DspModOperProbs==true)
    { printf("\t  %30s\t",getUsedOperName(i));
      printf("%f ",actualProb);
      stdDevSum=0;
      for (j=0; j<Pop.popSize; j++)
        stdDevSum+=(Pop.chroms[j]->operProbs[i]-sum/Pop.popSize)*
                   (Pop.chroms[j]->operProbs[i]-sum/Pop.popSize); 
      printf("stdDev=%f\n",sqrt(stdDevSum/Pop.popSize));
    }
    if (FDumpModOperProbs!=NULL)
      fprintf(FDumpModOperProbs,"%f\t",actualProb);
  }
  if (FDumpModOperProbs!=NULL)
      fprintf(FDumpModOperProbs,"\n");
}

void modOperProbs(void)
{ register int i;
  genericChromType *chrom;
  for (i=0; i<Pop.popSize; i++)
  { chrom=Pop.chroms[i];
    modifyOperProbs(chrom->operProbs,chrom->operApplCounter,
                    &(chrom->operApplCounterTotal),chrom->roulette);
  }
}

void dspBestEval(void (*tryDspPrct)(void), boolean doItAnyway)
{ if (DspBestEval==true || doItAnyway)
  { tryDspPrct();
    printf("\tBest eval=%f\n",Pop.chroms[Pop.bestChrom]->eval);
  }
  if (FDumpBestEval!=NULL)
    fprintf(FDumpBestEval,"%f\t%f\n",getCurPrctRun(),
            Pop.chroms[Pop.bestChrom]->eval);
}

void dspBestChrom(void (*tryDspPrct)(void), boolean doItAnyway)
{ if (DspBestChrom==true || doItAnyway)
  { tryDspPrct();
    printf("\t");
    dspChrom(stdout,Pop.chroms[Pop.bestChrom]->chrom);
  }
  if (FDumpBestChrom!=NULL)
  { fprintf(FDumpBestChrom,"%f ",getCurPrctRun());
    dspChrom(FDumpBestChrom,Pop.chroms[Pop.bestChrom]->chrom);
  }
}

void tryDspEachImprovement(void (*tryDspPrct)(void))
{ if (DspEvalEachImprovement==true)
  { tryDspPrct();
    printf("\tBest eval= %f\n",Pop.chroms[Pop.bestChrom]->eval);
  }
}
