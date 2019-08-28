/* $Id: measure.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : measure.c                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "converge.h"
#include "dbuff.h"
#include "error.h"
#include "fopen.h"
#include "logit.h"
#include "other.h"
#include "schema.h"
#include "trace.h"
#include "measure.h"


#define EPSILON		1.0e-4		/* epsilon value */


/*****************************************************************************/
/* Compute new worst current fitness                                         */
/*****************************************************************************/
static double new_worst(void)
{
  if (WorstCurrPerf[P] == 0.0)
  { return(EPSILON);
  }
  if (WorstCurrPerf[P] > 0.0)
  { return(WorstCurrPerf[P] * (1.0 + EPSILON));
  }
  return(WorstCurrPerf[P] * (1.0 - EPSILON));
}


/*****************************************************************************/
/* Measure performance and append to output file                             */
/*****************************************************************************/
void measure()
{ static BOOLEAN spaceFlag = TRUE;
  static double *VAvg, *VVar, *VSkw, *Hlp;
  register int i, j, w;
  FILE *fp;
  int size;

  trace("measure() entered");

  if (spaceFlag)
  { if (VarFlag)
    { VAvg = (double *) emalloc((unsigned long) OrderLen * sizeof(double),
        TRUE);
      VVar = (double *) emalloc((unsigned long) OrderLen * sizeof(double),
        TRUE);
      VSkw = (double *) emalloc((unsigned long) OrderLen * sizeof(double),
        TRUE);
      Hlp = (double *) emalloc((unsigned long) PopSize * sizeof(double),
        TRUE);
    }
    spaceFlag = FALSE;
  }

  BestCurrPerf[P] = NewPop[P]->rep[NewPop[P]->fitvec[PopSize - 1]].fitness;
  WorstCurrPerf[P] = NewPop[P]->rep[NewPop[P]->fitvec[0]].fitness;
  AvgCurrPerf[P] = NewPop[P]->fitsum / PopSize;

  if (UpperIndex <= PopSize)
  { for (i = LowerIndex, AvgBestPerf[P] = 0.0; i <= UpperIndex; i++)
    { AvgBestPerf[P] += NewPop[P]->rep[NewPop[P]->fitvec[PopSize - i]].fitness;
    }
    AvgBestPerf[P] /= (double) (UpperIndex - LowerIndex + 1);
  }

  if (WindowSize)
  { w = Generation[P] % WindowSize;
    Win[P][w] = new_worst();
    Worst[P] = Win[P][0];
    for (i = 1; i < WindowSize; i++)
    { if (Win[P][i] > Worst[P])
      { Worst[P] = Win[P][i];
      }
    }
  }
  else
  { if (Worst[P] < WorstCurrPerf[P])
    { Worst[P] = new_worst();
    }
  }

  Online[P] = OnSum[P] / Trials[P];
  Offline[P] = OffSum[P] / Trials[P];

  if (TraceFlag)
  { log_num_int(stdout, "Generation", Generation[P], FALSE);
    log_num_int(stdout, "Trials", Trials[P], FALSE);
  }
 
  if (Interval && ((Trials[P] >= Plateau[P]) || DoneFlag[P]))
  { converge();

    if (VarFlag)
    { for (i = 0; i < OrderLen; i++)
      { for (j = 0; j < PopSize; j++)
        { Hlp[j] = (double) 1.0 + NewPop[P]->rep[j].myrep->job[i];
        }
        VAvg[i] = avgval(Hlp, PopSize);
        VVar[i] = varval(Hlp, PopSize);
        VSkw[i] = skwval(Hlp, PopSize, VAvg[i]);
      }
      enter_buffer(VAvg, ValBuf[P][0], OrderLen, SGLLINE);
      enter_buffer(VVar, ValBuf[P][1], OrderLen, SGLLINE);
      enter_buffer(VSkw, ValBuf[P][2], OrderLen, SGLLINE);

      if (DoneFlag[P])
      { for (i = 0; i < BUFCNT; i++)
        { flush_buffer(ValBuf[P][i], OrderLen, SGLLINE);
        }
      }
    }

    write_pfmbuf(&size, SGLLINE);

    if (DoneFlag[P])
    { flush_buffer(PfmBuf[P], size, SGLLINE);
    }

    Plateau[P] = (Trials[P] / Interval) * Interval + Interval;
  }

  if (OutFlag && (Spin[P] >= MaxSpin))
  { if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
    { log_int(fp, "Experiment", Experiment);
      log_int(fp, "SPINNING at Generation", Generation[P]);
      log_int(fp, "Trials", Trials[P]);
      fclose(fp);
    }
    else
    { sprintf(Msg, "Measure: can't open '%s'", get_fname(LogFile));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }

  if (SchemaFlag)
  { schema();
  }

  trace("measure() completed");
}
 

/*****************************************************************************/
/* Average value                                                             */
/*****************************************************************************/
double avgval(x, n)
  double *x;		/* pointer to values */
  int n;		/* number of values */
{ register int i;
  double res, avg;

  trace("avgval() entered");

  if (n == 0)
  { critical_error(ERR_DIV_BY_ZERO, "AvgVal: Division by Zero");
  }

  for (i = 0, avg = 0.0; i < n; i++)
  { avg += x[i];
  }

  res = avg / (double) n;

  trace("avgval() completed");

  return(res);
}


/*****************************************************************************/
/* Variance value                                                            */
/*****************************************************************************/
double varval(x, n)
  double *x;		/* pointer to values */
  int n;		/* number of values */
{ register int i;
  double res, ex1, ex2;

  trace("varval() entered");

  if (n < 2)
  { critical_error(ERR_VARVAL, "VarVal: Illegal Number of Values");
  }

  for (i = 0, ex1 = 0.0, ex2 = 0.0; i < n; i++)
  { ex1 += x[i];
    ex2 += x[i] * x[i];
  }
  ex1 = ex1 * ex1 / (double) n;

  res = (ex2 - ex1) / (double) (n - 1);

  trace("varval() completed");

  return(res);
}


/*****************************************************************************/
/* Skewness value                                                            */
/*****************************************************************************/
double skwval(x, n, avg)
  double *x;		/* pointer to values */
  int n;		/* number of values */
  double avg;		/* average value */
{ register int i;
  double res, val, mo2, mo3;

  trace("skwval() entered");

  for (i = 0, mo2 = 0.0, mo3 = 0.0; i < n; i++)
  { val  = x[i] - avg;
    mo2 += val * val;
    mo3 += val * mo2;
  }
  mo2 /= ((double) n);
  mo3 /= ((double) n);

  res = (mo2 == 0) ? 0.0 : mo3 / sqrt(mo2 * mo2 * mo2);

  trace("skwval() completed");

  return(res);
}

 
/*** end of file ***/
