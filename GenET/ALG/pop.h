/* pop.h contains population and its roulette: popRoulette for population */
/*   and mergePopRoulette for merged populations */

#include <stdio.h>

typedef struct
        { double eval;
          void *chrom;
          double *operProbs;
          double *operApplCounter;
          double operApplCounterTotal;
          void *roulette;
        } genericChromType;
typedef genericChromType **genericChromsType; /* dynamic array of pointers */

extern void setDspChromEval(FILE *, boolean use);
extern void setDspModOperProbs(FILE *, boolean use);

extern void setPop(FILE *fp);

extern genericChromType *getParent(void);

/* doGeneration() */
extern void doGeneration(void);

extern void dspAveOperProbs(void (*tryDspPrct)(void));

extern void dspPop(void);

extern void modOperProbs(void);

extern void dspBestEval(void (*tryDspPrct)(void), boolean doItAnyway);

extern void dspBestChrom(void (*tryDspPrct)(void), boolean doItAnyway);

extern void tryDspEachImprovement(void (*tryDspPrct)(void));
