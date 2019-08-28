/* opers.h contains routines to fire operators and modify weights */

typedef int (*operAddrType)(genericChromType *parent1,
              genericChromsType NewPopChroms, 
              int maxNumOffspring, int curNumOffspring);
           /* pointer to operator, which is a function taking parent and 
              offspring population inserting offspring into the new population; 
              return number of chroms inserted */
typedef struct 
        { char operName[NAMESIZ];
          operAddrType operAddr;
        } operType;

/* setOpers must be called prior to actual simulation and before setTiming */
/* set up all operator data structures */
/* Calls problem specific function to set operator addresses, names */
/* and number of parent/offspring. Calculates initial oper. probabilities */
/* in OperWghts vector */
extern void setOpers(FILE *fp);

/* createInitOperRoulette() creates, initializes and returns roulette using */
/*   initial OperWghts */
extern void *createInitOperRoulette(void);

extern double getAddProb(void);

/* initOperProbs(p) initializes vector p with OperProbs */
extern void initOperProbs(double *p);

/* modifyOperWghts(p,q) modifies probabilities p. q counts fired operators */
/*  individually and *Q totals them. Afterwards, reset OperRoulette */
extern void modifyOperProbs(double *p,double *q,double *Q,void *OperRoulette);

extern boolean adaptOpers(void);

extern int fireOper(int operNum, genericChromType *parentChrom,
                    genericChromsType newChroms, int maxNumOffspring,
                    int curNewPopSize);

extern char *getUsedOperName(int whatOper);

/* finishOper(newChrom, ...) is called after offspring are generated, once   */
/*   for each offspring. The variable list contains all parents for newChrom */
/*   It creates the proper oper counters for the offspring. The standard     */
/*   used is that the new counters are averages of those of parents          */
/*   It finally sets a roulette for the new chrom and evaluates it           */
extern void finishOper(genericChromType *newChrom, int operNum,
                       int *curNumOff, int numParents, ...);

/* getNumUsedOpers() returns number of actually used opers in a run          */
extern int getNumUsedOpers(void);
