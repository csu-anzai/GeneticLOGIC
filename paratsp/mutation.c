/* $Id: mutation.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : mutation.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "optlocal.h"
#include "other.h"
#include "random.h"
#include "trace.h"
#include "mutation.h"


#define C_N	c->myrep->n
#define C_JOB	c->myrep->job

#define MINDIM		1		/* minimal mutation dimension */
#define MAXDIM		(C_N / 2.0)	/* maximal mutation dimension */
#define MINPROB		0.000001	/* minimal mutation probability */
#define MAXPROB		0.1		/* maximal mutation probability */


typedef void (*MyMutationType)(CHROM *, TOUR, TOUR);	/* mutation type */


/*****************************************************************************/
/* Move mutation                                                             */
/*****************************************************************************/
void move_mutation(c, p1, p2)
  CHROM *c;		/* pointer to individual */
  TOUR p1;		/* first position */
  TOUR p2;		/* second position */
{ register TOUR i, val;

  trace("move_mutation() entered");

  val = C_JOB[p2];

  if (p2 >= p1)
  { for (i = p2; i > p1; i--)
    { C_JOB[i] = C_JOB[i - 1];
    }
  }
  else
  { for (i = p2 + C_N; i > p1; i--)
    { C_JOB[i % C_N] = C_JOB[(i - 1) % C_N];
    }
  }

  C_JOB[p1] = val;

  c->needsEval = TRUE;

  trace("move_mutation() completed");
}


/*****************************************************************************/
/* Swap mutation                                                             */
/*****************************************************************************/
void swap_mutation(c, p1, p2)
  CHROM *c;		/* pointer to individual */
  TOUR p1;		/* first position */
  TOUR p2;		/* second position */
{
  trace("swap_mutation() entered");

  SWAP(C_JOB[p1], C_JOB[p2]);

  c->needsEval = TRUE;

  trace("swap_mutation() completed");
}


/*****************************************************************************/
/* Invert mutation                                                           */
/*****************************************************************************/
void invert_mutation(c, p1, p2)
  CHROM *c;		/* pointer to individual */
  TOUR p1;		/* first position */
  TOUR p2;		/* second position */
{ register TOUR i, u;

  trace("invert_mutation() entered");

  if (p2 >= p1)
  { u = (p2 - p1 + 1) / 2;
    for (i = 0; i < u; i++, p1++, p2--)
    { SWAP(C_JOB[p1], C_JOB[p2]);
    }
  }
  else
  { p2 += C_N;
    u = (p2 - p1 + 1) / 2;
    for (i = 0; i < u; i++, p1++, p2--)
    { SWAP(C_JOB[p1 % C_N], C_JOB[p2 % C_N]);
    }
  }

  c->needsEval = TRUE;

  trace("invert_mutation() completed");
}


/*****************************************************************************/
/* Binomial function                                                         */
/*****************************************************************************/
static double bin(int n, double p, int k)
{ register int i, m = n + 1;
  double b, r, q = 1.0 - p;

  r = log(p / q);
  b = n * log(q);
  for (i = 1; i <= k; i++)
  { b += r + log((double) m / i - 1.0);
  }

  return(exp(b));
}


/*****************************************************************************/
/* Constant rate mutation                                                    */
/*****************************************************************************/
static void constant_mutation(MyMutationType mute, CHROM *c)
{ register TOUR i, p, n;
  unsigned *vec;
  double border, rnd;

  rnd = equal_double_random(1.0);
  border = 1.0;
  for (n = 0; (rnd < border) && (n <= C_N); n++)
  { border -= bin(C_N, c->mutprob, n);
  }
  n--;

  if (n)
  { vec = (unsigned *) emalloc((unsigned long) n * sizeof(unsigned), TRUE);
    equal_random_int_vec(vec, n, C_N);
    for (i = 0; i < n; i++)
    { do
      { p = equal_unsigned_random(C_N);
      }
      while (p == vec[i]);
      mute(c, vec[i], p);
    }
    free(vec);
  }
}


/*****************************************************************************/
/* Adaptation                                                                */
/*****************************************************************************/
static void adaptation(CHROM *c)
{ register TOUR n;
  double rnd;

  rnd = 1.0 + equal_random();
  if (c->mutprob == MINPROB)
  { n = equal_unsigned_random(2);
    if (n)
    { c->mutprob *= rnd;
    }
  }
  else
  { if (c->mutprob == MAXPROB)
    { n = equal_unsigned_random(2);
      if (n)
      { c->mutprob /= rnd;
      }
    }
    else
    { n = equal_unsigned_random(3);
      if (! n)
      { c->mutprob /= rnd;
      }
      else
      { if (n == 2)
        { c->mutprob *= rnd;
        }
      }
    }
  }
  if (c->mutprob < MINPROB)
  { c->mutprob = MINPROB;
  }
  if (c->mutprob > MAXPROB)
  { c->mutprob = MAXPROB;
  }

  rnd = 1.0 + equal_random();
  if (c->mutdim == MINDIM)
  { n = equal_unsigned_random(2);
    if (n)
    { c->mutdim *= rnd;
    }
  }
  else
  { if (c->mutdim == MAXDIM)
    { n = equal_unsigned_random(2);
      if (n)
      { c->mutdim /= rnd;
      }
    }
    else
    { n = equal_unsigned_random(3);
      if (! n)
      { c->mutdim /= rnd;
      }
      else
      { if (n == 2)
        { c->mutdim *= rnd;
        }
      }
    }
  }
  if (c->mutdim < MINDIM)
  { c->mutdim = MINDIM;
  }
  if (c->mutdim > MAXDIM)
  { c->mutdim = MAXDIM;
  }
}


/*****************************************************************************/
/* Adapted rate mutation                                                     */
/*****************************************************************************/
static void adapted_mutation(MyMutationType mute, CHROM *c)
{ register TOUR i, m, n, p;
  unsigned *vec;
  double border, rnd;

  adaptation(c);

  rnd = equal_double_random(1.0);
  border = 1.0;
  for (n = 0; (rnd < border) && (n <= C_N); n++)
  { border -= bin(C_N, c->mutprob, n);
  }
  n--;

  if (n)
  { vec = (unsigned *) emalloc((unsigned long) n * sizeof(unsigned), TRUE);
    equal_random_int_vec(vec, n, C_N);
    for (i = 0; i < n; i++)
    { m = equal_unsigned_random((unsigned) c->mutdim);
      if (equal_unsigned_random(2))
      { p = (vec[i] + m >= C_N) ? C_N - 1 : vec[i] + m;
      }
      else
      { p = (vec[i] < m) ? 0 : vec[i] - m;
      }
      if (vec[i] != p)
      { mute(c, vec[i], p);
      }
    }
    free(vec);
  }
}


/*****************************************************************************/
/* Constant move mutation                                                    */
/*****************************************************************************/
void mu_constant_move_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_constant_move_mutation() entered");

  constant_mutation(move_mutation, c);

  trace("mu_constant_move_mutation() completed");
}


/*****************************************************************************/
/* Constant swap mutation                                                    */
/*****************************************************************************/
void mu_constant_swap_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_constant_swap_mutation() entered");

  constant_mutation(swap_mutation, c);

  trace("mu_constant_swap_mutation() completed");
}


/*****************************************************************************/
/* Constant invert mutation                                                  */
/*****************************************************************************/
void mu_constant_invert_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_constant_invert_mutation() entered");

  constant_mutation(invert_mutation, c);

  trace("mu_constant_invert_mutation() completed");
}


/*****************************************************************************/
/* Adapted move mutation                                                     */
/*****************************************************************************/
void mu_adapted_move_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_adapted_move_mutation() entered");

  adapted_mutation(move_mutation, c);

  trace("mu_adapted_move_mutation() completed");
}


/*****************************************************************************/
/* Adapted swap mutation                                                     */
/*****************************************************************************/
void mu_adapted_swap_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_adapted_swap_mutation() entered");

  adapted_mutation(swap_mutation, c);

  trace("mu_adapted_swap_mutation() completed");
}


/*****************************************************************************/
/* Adapted invert mutation                                                   */
/*****************************************************************************/
void mu_adapted_invert_mutation(c)
  CHROM *c;		/* pointer to individual */
{
  trace("mu_adapted_invert_mutation() entered");

  adapted_mutation(invert_mutation, c);

  trace("mu_adapted_invert_mutation() completed");
}


/*****************************************************************************/
/* Constant random mutation                                                  */
/*****************************************************************************/
void mu_constant_rand_mutation(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;
 
  trace("mu_constant_rand_mutation() entered");

  rnd = equal_unsigned_random(3);
  switch (rnd)
  { case 0:
      mu_constant_move_mutation(c);
      break;
    case 1:
      mu_constant_swap_mutation(c);
      break;
    case 2:
      mu_constant_invert_mutation(c);
      break;
    default:
      break;
  }

  trace("mu_constant_rand_mutation() completed");
}


/*****************************************************************************/
/* Adapted random mutation                                                   */
/*****************************************************************************/
void mu_adapted_rand_mutation(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;
 
  trace("mu_adapted_rand_mutation() entered");

  rnd = equal_unsigned_random(3);
  switch (rnd)
  { case 0:
      mu_adapted_move_mutation(c);
      break;
    case 1:
      mu_adapted_swap_mutation(c);
      break;
    case 2:
      mu_adapted_invert_mutation(c);
      break;
    default:
      break;
  }

  trace("mu_adapted_rand_mutation() completed");
}


/*****************************************************************************/
/* Constant localopt mutation                                                */
/*****************************************************************************/
void mu_constant_localopt_mutation(c)
  CHROM *c;		/* pointer to individual */
{ double rnd;

  trace("mu_constant_localopt_mutation() entered");

  rnd = equal_double_random(1.0);
  if (rnd < c->mutprob)
  { opt_local(c);
  }

  trace("mu_constant_localopt_mutation() completed");
}


/*****************************************************************************/
/* Adapted localopt mutation                                                 */
/*****************************************************************************/
void mu_adapted_localopt_mutation(c)
  CHROM *c;		/* pointer to individual */
{ double rnd;

  trace("mu_adapted_localopt_mutation() entered");

  adaptation(c);

  rnd = equal_double_random(1.0);
  if (rnd < c->mutprob)
  { opt_local(c);
  }

  trace("mu_adapted_localopt_mutation() completed");
}


/*****************************************************************************/
/* Constant random with localopt mutation                                    */
/*****************************************************************************/
void mu_constant_randall_mutation(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;
 
  trace("mu_constant_randall_mutation() entered");

  rnd = equal_unsigned_random(4);
  switch (rnd)
  { case 0:
      mu_constant_move_mutation(c);
      break;
    case 1:
      mu_constant_swap_mutation(c);
      break;
    case 2:
      mu_constant_invert_mutation(c);
      break;
    case 3:
      mu_constant_localopt_mutation(c);
      break;
    default:
      break;
  }

  trace("mu_constant_randall_mutation() completed");
}


/*****************************************************************************/
/* Adapted random with localopt mutation                                     */
/*****************************************************************************/
void mu_adapted_randall_mutation(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;
 
  trace("mu_adapted_randall_mutation() entered");

  rnd = equal_unsigned_random(4);
  switch (rnd)
  { case 0:
      mu_adapted_move_mutation(c);
      break;
    case 1:
      mu_adapted_swap_mutation(c);
      break;
    case 2:
      mu_adapted_invert_mutation(c);
      break;
    case 3:
      mu_adapted_localopt_mutation(c);
      break;
    default:
      break;
  }

  trace("mu_adapted_randall_mutation() completed");
}


/*****************************************************************************/
/* Mutation                                                                  */
/*****************************************************************************/
void mutation(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned p1, p2;

  trace("mutation() entered");

  if (c->mutprob == FILTER_MUTPROB)
  { p1 = equal_unsigned_random(C_N);
    while ((p2 = equal_unsigned_random(C_N)) == p1) ;
    swap_mutation(c, p1, p2);
  }
  else
  { switch (Mutation[0])
    { case MUT_NOP:
        break;
      case MUT_CON:
        switch (Mutation[1])
        { case MUT_SWP:
            mu_constant_swap_mutation(c);
            break;
          case MUT_MOV:
            mu_constant_move_mutation(c);
            break;
          case MUT_INV:
            mu_constant_invert_mutation(c);
            break;
          case MUT_RND:
            mu_constant_rand_mutation(c);
            break;
          case MUT_OPL:
            mu_constant_localopt_mutation(c);
            break;
          case MUT_ALL:
            mu_constant_randall_mutation(c);
            break;
          default:
            break;
        }
        break;
      case MUT_ADA:
        switch (Mutation[1])
        { case MUT_SWP:
            mu_adapted_swap_mutation(c);
            break;
          case MUT_MOV:
            mu_adapted_move_mutation(c);
            break;
          case MUT_INV:
            mu_adapted_invert_mutation(c);
            break;
          case MUT_RND:
            mu_adapted_rand_mutation(c);
            break;
          case MUT_OPL:
            mu_adapted_localopt_mutation(c);
            break;
          case MUT_ALL:
            mu_adapted_randall_mutation(c);
            break;
          default:
            break;
        }
        break;
      default:
        break;
    }
  }

  trace("mutation() completed");
}


/*** end of file ***/
