/* timing.c contains routines to control iterations and operator             */
/*   weights modifications                                                   */
/* Runs can be controlled by time limits or iteration limits                 */
/* NOTE: setTiming() should be called just prior to begin simulation         */
/*       if second-based run is used to ensure starting at 0 seconds         */

#include <stdio.h>
#include <time.h>
#include <limits.h>
#include "main.h"
#include "error.h"
#include "io.h"
#include "pop.h"
#include "opers.h"
#include "timing.h" 

typedef long unsigned timingType;
typedef enum {seconds, iterations} timingKindType;

static char *timingKindNames[]={"seconds","iterations"};
static timingKindType 
         RunLimitKind, DspChromLimitKind, ModOperLimitKind;
static timingType 
         StartRunTime,           /* time of start for seconds or 0 for iters */
         StartDspChromTime,
         StartModOperTime,
         CurRunTime=0,               /* relative to startRunTime, seconds or */
                                /* iterations - total run time for algorithm */
         CurDspChromTime=0, /* rel. to startDspChromTime, mod. DspChromLimit */
         CurModOperTime=0;    /* rel. to startModOperTime, mod. ModOperLimit */
static timingType RunLimit;
static timingType DspChromLimit;
static timingType ModOperLimit;

static boolean LimitsSet=false;           /* to ensure limits set before use */         
/* updateTiming() sets counters for each new generation                      */
/* called at end of each generation                                          */
void updateTiming(void)
{ if (RunLimitKind==seconds)
    CurRunTime=(timingType)time(NULL)-StartRunTime;
  else
    CurRunTime++;
  if (DspChromLimitKind==seconds)
    CurDspChromTime=(timingType)time(NULL)-StartDspChromTime;
  else
    CurDspChromTime++;
  if (ModOperLimitKind==seconds)
    CurModOperTime=(timingType)time(NULL)-StartModOperTime;
  else
    CurModOperTime++;
}

static void setRun(FILE *fp)
{ char message[BUFSIZ];
  sprintf(message,"Give run time kind %d:%s %d:%s",seconds,
          timingKindNames[seconds],iterations,timingKindNames[iterations]);
  if (getIntLtd(fp,message,seconds,iterations,(int*)&RunLimitKind)!=0)
    FAIL("");
  sprintf(message,"Give me run time as number of %s",
                   timingKindNames[RunLimitKind]);
  if (getLUnsLtd(fp,message,(long unsigned )0,ULONG_MAX,&RunLimit)!=0)
    FAIL("");
}

/* setDspChrom(file) sets the display interval and kind (seconds or iter.)   */
/*   for chroms; fp is open and provides the values                          */
/* NOTE: called after setting run time                                       */
static void setDspChrom(FILE *fp)
{ char message[BUFSIZ];
  int what;
  if (getIntLtd(fp,"Do you want to set interval for best chrom/eval disp/dump?",
                0,1,&what)!=0)
    FAIL("");
  if (what==0) 
  { DspChromLimitKind=iterations;
    DspChromLimit=ULONG_MAX;            /* set max iteration as the interval */
    setDspChromEval(fp,false);
    return;
  }
  sprintf(message,"Give display chrom/eval interval kind %d:%s %d:%s",seconds,
          timingKindNames[seconds],iterations,timingKindNames[iterations]);
  if (getIntLtd(fp,message,seconds,iterations,(int*)&DspChromLimitKind)!=0)
    FAIL("");
  sprintf(message,"Give display chrom/eval interval as number of %s",
                   timingKindNames[DspChromLimitKind]);
  if (getLUnsLtd(fp,message,(long unsigned )1,(long unsigned)RunLimit,
                 &DspChromLimit)!=0)
    FAIL("");
  setDspChromEval(fp,true);
}

/* setModOper(fp) sets the interval for modifying operator probabilities     */
/* NOTE: called after setting run time                                       */
static void setModOper(FILE *fp)
{ char message[BUFSIZ];
  int what;
  if (adaptOpers()==false)
  { ModOperLimitKind=iterations;
    ModOperLimit=ULONG_MAX;
    setDspModOperProbs(fp,false);
    return;
  }
  sprintf(message,"Give operator prob modification interval kind %d:%s %d:%s",
      seconds,timingKindNames[seconds],iterations,timingKindNames[iterations]);
  if (getIntLtd(fp,message,seconds,iterations,(int*)&ModOperLimitKind)!=0)
    FAIL("");
  sprintf(message,"Give operator prob modification interval as number of %s",
                   timingKindNames[ModOperLimitKind]);
  if (getLUnsLtd(fp,message,(long unsigned )1,(long unsigned)RunLimit,
                 &ModOperLimit)!=0)
    FAIL("");
  if (getIntLtd(fp,"Do you want to display/dump modified operator probs?",
                0,1,&what)!=0)
    FAIL("");
  if (what==0)
    setDspModOperProbs(fp,false);
  else
    setDspModOperProbs(fp,true);
}

void setRunLimits(FILE *fp)
{ setRun(fp);                       /* must be called first in this sequence */
  setDspChrom(fp);
  setModOper(fp);
  LimitsSet=true;        /* currently unused - can be used to check settings */
}

/* setTiming must be called just before first generation                     */
void setTiming()
{ StartRunTime=(timingType)time(NULL);             /* start time for seconds */
  StartDspChromTime=StartModOperTime=StartRunTime;
}

/* prctRun() returns the percentage of the current run                       */
/* it should check for TimingSet==true to make sure setTiming is called 1st  */
double prctRun(void)
{ return((RunLimit==0) ? (double)1 : (double)CurRunTime/RunLimit);
}

/* isDspChrom() returns true and resets internal counter if time for display */
boolean isDspChrom(void)
{ if (CurDspChromTime>=DspChromLimit)
  { if (DspChromLimitKind==seconds)
      StartDspChromTime=(timingType)time(NULL);
    CurDspChromTime=0;
    return(true);
  }
  else
    return(false);
}

/* isModOper() returns true and resets internal counter if time for oper mod */
boolean isModOper(void)
{ if (CurModOperTime>=ModOperLimit)
  { if (ModOperLimitKind==seconds)
      StartModOperTime=(timingType)time(NULL);
    CurModOperTime=0;
    return(true);
  }
  else
    return(false);
}


