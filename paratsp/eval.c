/* $Id: eval.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : eval.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "best.h"
#include "fitscale.h"
#include "other.h"
#include "readfile.h"
#include "trace.h"
#include "eval.h"


#define GEO_PI	3.1415927	/* PI */
#define RRR	6378.388	/* earth radius */


/*****************************************************************************/
/* Allocate memory for distance of towns                                     */
/*****************************************************************************/
void alloc_line()
{ register TOUR i;

  trace("alloc_line() entered");

  LinePtr = (LINE **) emalloc((unsigned long) TownNum * sizeof(LINE *), TRUE);

  for (i = 0; (i < TownNum) && (! CalcLine); i++)
  { LinePtr[i] = (LINE *) emalloc((unsigned long) (TownNum - 1 - i) *
      sizeof(LINE), FALSE);
    if (LinePtr[i] == NULL)
    { CalcLine = TRUE;
    }
  }

  if (CalcLine)
  { for (i = 0; LinePtr[i] != NULL; i++)
    { free(LinePtr[i]);
    }
    free(LinePtr);
  }

  trace("alloc_line() completed");
}


/*****************************************************************************/
/* Free memory of distance of towns                                          */
/*****************************************************************************/
void free_line()
{ register TOUR i;

  trace("free_line() entered");

  if (! CalcLine)
  { for (i = 0; i < TownNum; i++)
    { free(LinePtr[i]);
    }
    free(LinePtr);
  }

  trace("free_line() completed");
}


/*****************************************************************************/
/* Set distance of two towns                                                 */
/*****************************************************************************/
void set_line(t1, t2, w)
  TOUR t1;		/* first town index */
  TOUR t2;		/* second town index */
  LINE w;		/* distance value */
{
  trace("set_line() entered");

  LinePtr[t1][TownNum - 1 - t2] = w;

  trace("set_line() completed");
}


/*****************************************************************************/
/* Get distance of two towns                                                 */
/*****************************************************************************/
LINE get_line(t1, t2)
  TOUR t1;		/* first town index */
  TOUR t2;		/* second town index */
{ LINE res;

  trace("get_line() entered");

  if (t1 > t2)
  { SWAP(t1, t2);
  }
  res = LinePtr[t1][TownNum - 1 - t2];

  trace("get_line() completed");

  return(res);
}


/*****************************************************************************/
/* Calculate distance of two towns                                           */
/*****************************************************************************/
LINE calc_line(t1, t2)
  TOUR t1;		/* first town index */
  TOUR t2;		/* second town index */
{ double xd, yd, zd, res, deg, min, latitude[2], longitude[2];

  trace("calc_line() entered");

  if (t1 > t2)
  { SWAP(t1, t2);
  }

  switch (FDat.ewt)
  { case EWT_HOTO:
      xd = abs(Town[t1].x - Town[t2].x);
      yd = abs(Town[t1].y - Town[t2].y);
      res = sqrt(xd * xd + yd * yd) / 7.69;
      break;
    case EWT_EUC_2D:
    case EWT_EUC_3D:
      xd = Town[t1].x - Town[t2].x;
      yd = Town[t1].y - Town[t2].y;
      zd = Town[t1].z - Town[t2].z;
      res = sqrt(xd * xd + yd * yd + zd * zd) + 0.5;
      break;
    case EWT_MAN_2D:
    case EWT_MAN_3D:
      xd = abs(Town[t1].x - Town[t2].x);
      yd = abs(Town[t1].y - Town[t2].y);
      zd = abs(Town[t1].z - Town[t2].z);
      res = xd + yd + zd + 0.5;
      break;
    case EWT_MAX_2D:
    case EWT_MAX_3D:
      xd = abs(Town[t1].x - Town[t2].x);
      yd = abs(Town[t1].y - Town[t2].y);
      zd = abs(Town[t1].z - Town[t2].z);
      res = MAX(MAX(xd, yd), zd) + 0.5;
      break;
    case EWT_GEO:
      deg = (double) ((int) Town[t1].x);
      min = Town[t1].x - deg;
      latitude[0] = GEO_PI * (deg + (5.0 * min) / 3.0) / 180.0;
      deg = (double) ((int) Town[t1].y);
      min = Town[t1].y - deg;
      longitude[0] = GEO_PI * (deg + (5.0 * min) / 3.0) / 180.0;
      deg = (double) ((int) Town[t2].x);
      min = Town[t2].x - deg;
      latitude[1] = GEO_PI * (deg + (5.0 * min) / 3.0) / 180.0;
      deg = (double) ((int) Town[t2].y);
      min = Town[t2].y - deg;
      longitude[1] = GEO_PI * (deg + (5.0 * min) / 3.0) / 180.0;
      xd = cos(longitude[0] - longitude[1]);
      yd = cos(latitude[0] - latitude[1]);
      zd = cos(latitude[0] + latitude[1]);
      res = RRR * acos(0.5 * ((1.0 + xd) * yd - (1.0 - xd) * zd)) + 1.0;
      break;
    case EWT_ATT:
      xd = Town[t1].x - Town[t2].x;
      yd = Town[t1].y - Town[t2].y;
      res = sqrt((xd * xd + yd * yd) / 10.0);
      if (((int) res) < res)
      { res++;
      }
      break;
    default:
      res = 0.0;
      break;
  }

  trace("calc_line() completed");

  return((LINE) res);
}


/*****************************************************************************/
/* Calculate all distances of two towns                                      */
/*****************************************************************************/
void calc_all_lines()
{ register TOUR i, j;

  trace("calc_all_lines() entered");

  alloc_line();

  if (! CalcLine)
  { for (i = 0; i < TownNum - 1; i++)
    { for (j = i + 1; j < TownNum; j++)
      { set_line(i, j, calc_line(i, j));
      }
    }
  }

  trace("calc_all_lines() completed");
}


/*****************************************************************************/
/* Evaluate for Lin's 2-opt                                                  */
/*****************************************************************************/
double eval_lin_2opt(t1, t2, t3, t4)
  TOUR t1;		/* town index 1 */
  TOUR t2;		/* town index 2 */
  TOUR t3;		/* town index 3 */
  TOUR t4;		/* town index 4 */
{ double delta;

  trace("eval_lin_2opt() entered");

  if (CalcLine)
  { delta = calc_line(t1, t2) + calc_line(t3, t4) - calc_line(t1, t3) -
      calc_line(t2, t4);
  }
  else
  { delta = get_line(t1, t2) + get_line(t3, t4) - get_line(t1, t3) -
      get_line(t2, t4);
  }

  trace("eval_lin_2opt() completed");

  return(delta);
}


/*****************************************************************************/
/* Evaluate for 2-opt                                                        */
/*****************************************************************************/
double eval_2opt(ind)
  CHROM *ind;		/* pointer to individual */
{ register TOUR i, act, lst;
  MYREP *o = ind->myrep;
  double res, len = 0.0;

  trace("eval_2opt() entered");

  for (i = 0; i < o->n; i++)
  { act = o->job[i];
    lst = (i ? o->job[i - 1] : o->job[o->n - 1]);
    len += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
  }

  res = fitness_scale(len);

  trace("eval_2opt() completed");

  return(res);
}


/*****************************************************************************/
/* Evaluate for or-opt                                                       */
/*****************************************************************************/
double eval_or_kopt(ind, first, k)
  CHROM *ind;		/* pointer to individual */
  BOOLEAN first;	/* first call per loop */
  int k;		/* position */
{ static double l1, l2;
  register TOUR i, act, lst;
  MYREP *o = ind->myrep;
  double res, len;

  trace("eval_or_kopt() entered");

  if (first)
  { l1 = 0.0;
    l2 = 0.0;
    for (i = k; i < o->n - 1; i++)
    { act = o->job[i];
      lst = o->job[i + 1];
      l2 += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
    }
    for (i = 1; i < k; i++)
    { act = o->job[i - 1];
      lst = o->job[i];
      l1 += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
    }
  }
  else
  { act = o->job[o->n - 1];
    lst = o->job[k];
    l2 -= (CalcLine) ? calc_line(lst, act) : get_line(lst, act);

    act = o->job[k];
    lst = o->job[k + 1];
    l2 += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
  }

  len = l1 + l2;

  act = o->job[0];
  lst = o->job[o->n - 1];
  len += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);

  act = o->job[k - 1];
  lst = o->job[k];
  len += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);

  res = fitness_scale(len);

  trace("eval_or_kopt() completed");

  return(res);
}


/*****************************************************************************/
/* Evaluate an individual                                                    */
/*****************************************************************************/
void evaluate(ind)
  CHROM *ind;		/* pointer to individual */
{ register TOUR i, act, lst;
  MYREP *o = ind->myrep;
  double len = 0.0;

  trace("evaluate() entered");

  if (ind->needsEval)
  { for (i = 0; i < o->n; i++)
    { act = o->job[i];
      lst = (i ? o->job[i - 1] : o->job[o->n - 1]);
      len += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
    }
    ind->quality = len;
    ind->fitness = fitness_scale(len);
    ind->needsEval = FALSE;

    Spin[P] = 0;
    if (SaveSize)
    { save_best(ind);
    }

    Trials[P]++;
    ind->myTrial = Trials[P];
    ind->myGeneration = Generation[P];

    if ((ind->fitness < Best[P]) || (Trials[P] == 1))
    { Best[P] = ind->fitness;
    }

    OnSum[P] += ind->fitness;
    OffSum[P] += Best[P];
  }

  trace("evaluate() completed");
}


/*** end of file ***/
