#include <stdio.h>
#include <stdlib.h>
#include "main.h"
#include "pop.h"
#include "opers.h"
#include "roulette.h"
#include "timing.h"
#include "io.h"
#include "error.h"
#include "rand.h"
#include "representation.h"

#define DEBUG 0

static double Prct;                                    /* how far in the run */
static boolean PrctDisplayed=false;  /* to dsp prct only once per generation */

void tryDspPrct(void)
{ if (PrctDisplayed==true)
    return;
  PrctDisplayed=true;
  printf("%.4f",Prct);
}

int main(int argc, char *argv[])  /* if argument present, this is param file */
{ FILE *fp;
  int what;
  if (argc==1)
    fp=stdin;
  else if ((fp=fopen(argv[1],"r"))==NULL)
         FAIL("Could not open argument file");
  Prct=0;
  if (getIntLtd(fp,"Do you want to randomize random calls?",0,1,&what)!=0)
     FAIL("");
  if (what==1)
    makeSeed();
  setOpers(fp);
  setRunLimits(fp);
  setRepSpecifics(fp);
  setPop(fp);
  dspBestEval(tryDspPrct,false);
  dspBestChrom(tryDspPrct,false);
  dspAveOperProbs(tryDspPrct);

  setTiming();                               /* call just prior to iterating */
  Prct=prctRun();
  while (Prct<1)
  { PrctDisplayed=false;   /* prct will be displayed on stdout once per iter */
    doGeneration();
    tryDspEachImprovement(tryDspPrct);
    if (isModOper()==true)
    { modOperProbs();
      dspAveOperProbs(tryDspPrct);
    }
    if (isDspChrom()==true)
    { dspBestEval(tryDspPrct,false);
      dspBestChrom(tryDspPrct,false);
    }
#if DEBUG
    tryDspPrct();
    dspPop();
#endif
    updateTiming(); 
    Prct=prctRun();
  }
  PrctDisplayed=false;
  printf("\nFINAL: ");
  dspBestEval(tryDspPrct,true);
  dspBestChrom(tryDspPrct,true);
  return(0);
}

double getCurPrctRun(void)
{ return(Prct); 
}
