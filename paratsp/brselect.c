/* $Id: brselect.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : brselect.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "boltzman.h"
#include "random.h"
#include "trace.h"
#include "brselect.h"


#define GAUSS_CONST	0	/* const for gauss selection */


/*****************************************************************************/
/* Number of offsprings for mating                                           */
/*****************************************************************************/
int num_offsprings()
{ register int num;

  trace("num_offsprings() entered");

  num = (int) (GapSize * PopSize + 0.5);

  trace("num_offsprings() completed");

  return(num);
}


/*****************************************************************************/
/* Inverse roulette wheel breeder selection                                  */
/*****************************************************************************/
void bs_inv_roulette(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ register int i, idx;
  double sum, rnd, memo;

  trace("bs_inv_roulette() entered");

  for (i = 0; i < num; i++)
  { idx = 0;
    rnd = equal_double_random(pop->invfitsum);
    memo = pop->rep[pop->fitvec[0]].fitness - pop->rep[0].fitness;
    sum = MAX(memo, MINDOUBLE);
    while (sum < rnd)
    { memo = pop->rep[pop->fitvec[0]].fitness - pop->rep[++idx].fitness;
      sum += MAX(memo, MINDOUBLE);
    }
    pool[i] = idx;
  }

  trace("bs_inv_roulette() completed");
}


/*****************************************************************************/
/* Gauss function                                                            */
/*****************************************************************************/
static unsigned long gauss(unsigned long n)
{
  return((n * n + n) / 2);
}


/*****************************************************************************/
/* Gauss breeder selection                                                   */
/*****************************************************************************/
void bs_gauss(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ register int i, idx;
  unsigned long start, rnd;

  trace("bs_gauss() entered");

  for (i = 0; i < num; i++)
  { start = gauss(GAUSS_CONST);
    rnd = equal_long_random(gauss(pop->size + GAUSS_CONST) - start) + start;
    idx = (int) (sqrt((double) rnd * 8 + 1) - 1) / 2;
    pool[i] = pop->fitvec[idx];
  }

  trace("bs_gauss() completed");
}


/*****************************************************************************/
/* Random breeder selection                                                  */
/*****************************************************************************/
void bs_random(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ register int i, idx;

  trace("bs_random() entered");

  for (i = 0; i < num; i++)
  { idx = LowerIndex + equal_unsigned_random(UpperIndex + 1 - LowerIndex);
    pool[i] = pop->fitvec[PopSize - idx];
  }

  trace("bs_random() completed");
}


/*****************************************************************************/
/* Shuffle breeder pool                                                      */
/*****************************************************************************/
static void shuffle(int *pool, int num, int a)
{ register int i, k;
  register unsigned rnd;

  for (i = a; i < num; i++)
  { rnd = equal_unsigned_random(a);
    pool[i] = pool[rnd];
  }

  for (i = 0; i < num - 1; i++)
  { rnd = i + equal_unsigned_random(num - i);
    k = pool[rnd];
    pool[rnd] = pool[i];
    pool[i] = k;
  }
}


/*****************************************************************************/
/* Proportional breeder selection                                            */
/*****************************************************************************/
void bs_proportional(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ static BOOLEAN flag = TRUE;
  static double expfrac;
  register int i, k;
  double fac, xpd, ptr, sum;

  trace("bs_proportional() entered");

  if (flag)
  { expfrac = PopSize / (UpperIndex - LowerIndex + 1);
    flag = FALSE;
  }

  if (UpperIndex == PopSize)
  { fac = 1.0 / (Worst[P] - AvgCurrPerf[P]);
  }
  else
  { fac = 1.0 / (Worst[P] - AvgBestPerf[P]);
  }

  k = 0;
  ptr = equal_random();

  for (i = LowerIndex, sum = 0.0; i <= UpperIndex; i++)
  { xpd = (Worst[P] - pop->rep[pop->fitvec[PopSize - i]].fitness) * expfrac *
      fac;
    for (sum += xpd; sum > ptr; ptr++)
    { pool[k++] = pop->fitvec[PopSize - i];
    }
  }

  shuffle(pool, num, k);

  trace("bs_proportional() completed");
}


/*****************************************************************************/
/* Whitley's breeder selection                                               */
/*****************************************************************************/
void bs_whitley(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ static BOOLEAN flag = TRUE;
  static double e1, e2;
  register int i;

  trace("bs_whitley() entered");

  if (flag)
  { e1 = EtaMax - 1.0;
    e2 = EtaMax * EtaMax;
    flag = FALSE;
  }

  for (i = 0; i < num; i++)
  { pool[i] = pop->fitvec[PopSize - 1 - (int) (((double) PopSize) * (EtaMax -
      sqrt(e2 - 4.0 * e1 * equal_random())) / (2.0 * e1))];
  }

  shuffle(pool, num, num);

  trace("bs_whitley() completed");
}


/*****************************************************************************/
/* Ranking selection                                                         */
/*****************************************************************************/
static void ranking(POPULATION *pop, int *pool, int num, BOOLEAN inv)
{ static BOOLEAN flag = TRUE;
  static double expfrac;
  register int i, k;
  double comval, xpt, ptr, sum;

  if (flag)
  { expfrac = PopSize / UpperIndex;
    flag = FALSE;
  }

  k = 0;
  ptr = equal_random();

  for (i = LowerIndex, sum = 0.0; i <= UpperIndex; i++)
  { if (UpperIndex == 1)
    { xpt = (double) PopSize;
    }
    else
    { comval = 2.0 * (EtaMax - 1.0) * ((double) (i - 1)) /
        ((double) UpperIndex - 1.0);
      xpt = (inv) ? (2.0 - EtaMax + comval) * expfrac :
                    (EtaMax - comval) * expfrac;
    }
    for (sum += xpt; sum > ptr; ptr++)
    { pool[k++] = pop->fitvec[PopSize - i];
    }
  }

  shuffle(pool, num, k);
}


/*****************************************************************************/
/* Linear ranking breeder selection                                          */
/*****************************************************************************/
void bs_linear_rank(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{
  trace("bs_linear_rank() entered");

  ranking(pop, pool, num, FALSE);

  trace("bs_linear_rank() completed");
}


/*****************************************************************************/
/* Inverse linear ranking breeder selection                                  */
/*****************************************************************************/
void bs_inv_linear_rank(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{
  trace("bs_inv_linear_rank() entered");

  ranking(pop, pool, num, TRUE);

  trace("bs_inv_linear_rank() completed");
}


/*****************************************************************************/
/* Boltzmann breeder selection                                               */
/*****************************************************************************/
void bs_boltzmann(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{ register int k;

  trace("bs_boltzmann() entered");

  k = boltzmann_select(pop, pool);
  shuffle(pool, num, k);

  trace("bs_boltzmann() completed");
}


/*****************************************************************************/
/* Breeder selection                                                         */
/*****************************************************************************/
void breeder_select(pop, pool, num)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
  int num;		/* number of individuals to select */
{
  trace("breeder_select() entered");

  switch (BreederSelect)
  { case BRS_INV:
      bs_inv_roulette(pop, pool, num);
      break;
    case BRS_GAU:
      bs_gauss(pop, pool, num);
      break;
    case BRS_RND:
      bs_random(pop, pool, num);
      break;
    case BRS_PRO:
      bs_proportional(pop, pool, num);
      break;
    case BRS_WHI:
      bs_whitley(pop, pool, num);
      break;
    case BRS_LIR:
      bs_linear_rank(pop, pool, num);
      break;
    case BRS_ILR:
      bs_inv_linear_rank(pop, pool, num);
      break;
    case BRS_BOL:
      bs_boltzmann(pop, pool, num);
      break;
    default:
      break;
  }

  trace("breeder_select() completed");
}


/*** end of file ***/
