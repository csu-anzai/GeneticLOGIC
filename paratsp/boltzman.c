/* $Id: boltzman.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : boltzman.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "random.h"
#include "trace.h"
#include "boltzman.h"


#define POP_FIT(x)	pop->rep[pop->fitvec[PopSize - 1 - x]].fitness

#define CHECK		0.1		/* check part of population */
#define LIMIT		11.513		/* limit for logistic function */
#define THSLD		1.0e+10		/* threshold default */


/*****************************************************************************/
/* Choose an alternative individual                                          */
/*****************************************************************************/
static int chsalt(POPULATION *pop, double f1, double f2, double thshld)
{ register int alt, i = LowerIndex - 1, chksize = CHECK * (UpperIndex - 1);

  do
  { alt = LowerIndex - 1 + equal_unsigned_random(UpperIndex - LowerIndex + 1);
    i++;
  }
  while (((fabs(f1 - POP_FIT(alt)) <= thshld) ||
          (fabs(f2 - POP_FIT(alt)) <= thshld)) &&
         (i <= chksize));

  return(alt);
}


/*****************************************************************************/
/* Logistic function                                                         */
/*****************************************************************************/
static double logfct(double x)
{
  if (x > LIMIT)
  { return(1.0);
  }
  if (x < -LIMIT)
  { return(0.0);
  }
  return(1.0 / (1.0 + exp(-x)));
}


/*****************************************************************************/
/* Calculate threshold                                                       */
/*****************************************************************************/
static double calc_thshld(double gap, double tmp)
{ double prb;

  prb = 0.5 + fabs(gap) * 0.5;

  if (prb >= 1.0)
  { return(THSLD);
  }

  return(-tmp * log(1.0 / prb - 1.0));
}


/*****************************************************************************/
/* Boltzmann breeder selection                                               */
/*****************************************************************************/
int boltzmann_select(pop, pool)
  POPULATION *pop;	/* pointer to population */
  int *pool;		/* pointer to breeder pool */
{ static BOOLEAN flag = TRUE;
  static int markov;
  static double tmp, thshld;
  register int i, idx1, idx2, idxthd, k;
  double prb1, prb2;

  trace("boltzmann_select() entered");

  if (flag)
  { tmp = 10.0;
    markov = ChnLen * PopSize;
    thshld = 0.5;
    flag = FALSE;
  }

  if (Trials[P] % markov == 0)
  { tmp *= CtrlParam;
  }

  for (i = LowerIndex - 1, k = 0; i < UpperIndex; i++)
  { idx1 = i;
    idx2 = chsalt(pop, POP_FIT(idx1), POP_FIT(idx1), thshld);
    if (equal_unsigned_random(2))
    { idxthd = chsalt(pop, POP_FIT(idx1), POP_FIT(idx2), thshld);
    }
    else
    { idxthd = chsalt(pop, POP_FIT(idx1), POP_FIT(idx1), thshld);
    }

    prb1 = logfct((POP_FIT(idxthd) - POP_FIT(idx2)) / tmp);
    if (equal_random() < prb1)
    { idx2 = idxthd;
    }

    prb2 = logfct((POP_FIT(idx2) - POP_FIT(idx1)) / tmp);
    if (equal_random() < prb2)
    { pool[k++] = pop->fitvec[PopSize - 1 - idx1];
    }
    else
    { pool[k++] = pop->fitvec[PopSize - 1 - idx2];
    }

    thshld = calc_thshld(prb2 - prb1, tmp);
  }

  trace("boltzmann_select() completed");

  return(k);
}


/*** end of file ***/
