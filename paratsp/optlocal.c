/* $Id: optlocal.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : optlocal.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "eval.h"
#include "fitscale.h"
#include "mutation.h"
#include "trace.h"
#include "optlocal.h"


#define C_JOB	c->myrep->job

#define MAX_KOPT	3		/* maximal k for k-opt */


/*****************************************************************************/
/* Lin's 2-opt                                                               */
/*****************************************************************************/
void op_lin_2opt(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, j, i1, j1;
  BOOLEAN newtour;
  double delta;

  trace("opt_lin_2opt() entered");

  if (c->needsEval)
  { do
    { newtour = FALSE;
      for (i = 0; i < OrderLen; i++)
      { for (j = 0; j < OrderLen; j++)
        { i1 = (i + 1) % OrderLen;
          j1 = (j + 1) % OrderLen;
          if ((i1 != j) &&
              (i != j1) &&
              (i != j))
          { delta = eval_lin_2opt(C_JOB[i], C_JOB[i1], C_JOB[j], C_JOB[j1]);
            if (delta > 0.0)
            { invert_mutation(c, i1, j);
              newtour = TRUE;
              i = OrderLen;
              j = OrderLen;
            }
          }
        }
      }
    }
    while (newtour);

    c->fitness = eval_2opt(c);
  }

  trace("opt_lin_2opt() completed");
}


/*****************************************************************************/
/* 2-opt                                                                     */
/*****************************************************************************/
void op_2opt(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, x;
  BOOLEAN newtour;
  double fit, cfit;

  trace("opt_2opt() entered");

  if (c->needsEval)
  { cfit = c->fitness;

    do
    { newtour = FALSE;
      for (x = OrderLen - 2; (x > 0) && (! newtour); x--)
      { for (i = 0; (i < OrderLen) && (! newtour); i++)
        { invert_mutation(c, i, (i + x) % OrderLen);
          fit = eval_2opt(c);
          if (fit < cfit)
          { newtour = TRUE;
            cfit = fit;
          }
          else
          { invert_mutation(c, i, (i + x) % OrderLen);
          }
        }
      }
    }
    while (newtour);

    c->fitness = cfit;
  }

  trace("opt_2opt() completed");
}


/*****************************************************************************/
/* 2-quick                                                                   */
/*****************************************************************************/
void op_2quick(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, j, i1, j1;
  double delta;

  trace("op_2quick() entered");

  if (c->needsEval)
  { for (i = 0; i < OrderLen - 2; i++)
    { for (j = i + 2; j < OrderLen; j++)
      { i1 = (i + 1) % OrderLen;
        j1 = (j + 1) % OrderLen;
        delta = eval_lin_2opt(C_JOB[i], C_JOB[i1], C_JOB[j], C_JOB[j1]);
        if (delta > 0.0)
        { invert_mutation(c, i1, j);
          j = i + 1;
        }
      }
    }

    c->fitness = eval_2opt(c);
  }

  trace("op_2quick() completed");
}


/*****************************************************************************/
/* or-opt(k)                                                                 */
/*****************************************************************************/
void op_or_kopt(c, k)
  CHROM *c;		/* pointer to individual */
  int k;		/* argument */
{ register TOUR i, j, x, t;
  BOOLEAN first, newtour;
  double fit, cfit = 0.0;

  trace("op_or_kopt() entered");

  if (c->needsEval && (k <= MAX_KOPT))
  { for (x = k; x > 0; x--)
    { t = 0;
      do
      { do
        { newtour = FALSE;
          first = TRUE;
          for (i = x; (i < OrderLen) && (! newtour); i++)
          { fit = eval_or_kopt(c, first, x);
            if (first)
            { cfit = fit;
              first = FALSE;
            }
            else
            { if (fit < cfit)
              { newtour = TRUE;
              }
            }
            if (newtour)
            { for (j = 0; j < OrderLen - i; j++)
              { move_mutation(c, 0, OrderLen - 1);
              }
              t = 0;
            }
            else
            { move_mutation(c, x, OrderLen - 1);
            }
          }
        }
        while (newtour);
        move_mutation(c, 0, OrderLen - 1);
        t++;
      }
      while (t < OrderLen);
    }

    c->fitness = cfit;
  }

  trace("op_or_kopt() completed");
}


/*****************************************************************************/
/* Series of local optimizations                                             */
/*****************************************************************************/
void op_all(c)
  CHROM *c;		/* pointer to individual */
{
  trace("op_all() entered");

  op_lin_2opt(c);
  op_or_kopt(c, 1);
  op_or_kopt(c, 2);
  op_or_kopt(c, 3);
  op_or_kopt(c, 1);

  trace("op_all() completed");
}


/*****************************************************************************/
/* Local optimization for TSP                                                */
/*****************************************************************************/
void opt_local(c)
  CHROM *c;		/* pointer to individual */
{
  trace("opt_local() entered");

  switch (OptLocal)
  { case OPL_NOP:
      break;
    case OPL_LO2:
      op_lin_2opt(c);
      break;
    case OPL_OP2:
      op_2opt(c);
      break;
    case OPL_2QU:
      op_2quick(c);
      break;
    case OPL_ORA:
      op_all(c);
      break;
    case OPL_1OR:
      op_or_kopt(c, 1);
      break;
    case OPL_2OR:
      op_or_kopt(c, 2);
      break;
    case OPL_3OR:
      op_or_kopt(c, 3);
      break;
    default:
      break;
  }

  trace("opt_local() completed");
}


/*** end of file ***/
