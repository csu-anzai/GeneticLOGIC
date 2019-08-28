/* $Id: popinit.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : popinit.c                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "bit.h"
#include "eval.h"
#include "filtwins.h"
#include "normal.h"
#include "random.h"
#include "trace.h"
#include "popinit.h"


/*****************************************************************************/
/* Random individual population init                                         */
/*****************************************************************************/
void in_random(c)
  CHROM *c;		/* pointer to individual */
{
  trace("in_random() entered");

  equal_random_int_vec(c->myrep->job, OrderLen, OrderLen);

  trace("in_random() completed");
}


/*****************************************************************************/
/* Tour file population init                                                 */
/*****************************************************************************/
void in_tour_file(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR j;

  trace("in_tour_file() entered");

  for (j = 0; j < OrderLen; j++)
  { c->myrep->job[j] = InitTour[j];
  }

  trace("in_tour_file() completed");
}


/*****************************************************************************/
/* Nearest neighbour population init                                         */
/*****************************************************************************/
void in_nearest_neighbour(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, j, t, p, pos = 0.0;
  BIT *bit;
  double len, min;

  trace("in_nearest_neighbour() entered");

  bit = alloc_bit(OrderLen);
  clear_bits(bit);

  t = (TOUR) equal_unsigned_random(OrderLen);
  set_bit(bit, t);
  c->myrep->job[0] = t;

  for (i = 1; i < OrderLen; i++)
  { min = MAXDOUBLE;
    for (j = 0; j < OrderLen - i; j++)
    { p = get_next_0_bit(bit, j + 1);
      len = (CalcLine) ? calc_line(t, p) : get_line(t, p);
      if (len < min)
      { min = len;
        pos = p;
      }
    }
    t = pos;
    set_bit(bit, t);
    c->myrep->job[i] = t;
  }

  free_bit(bit);

  trace("in_nearest_neighbour() completed");
}


/*****************************************************************************/
/* Cheapest insertion population init                                        */
/*****************************************************************************/
void in_cheapest_insertion(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, j, t, start, p, pos = 0.0;
  BIT *bit;
  double len, min;

  trace("in_cheapest_insertion() entered");

  bit = alloc_bit(OrderLen);
  clear_bits(bit);

  t = (TOUR) equal_unsigned_random(OrderLen);
  set_bit(bit, t);
  c->myrep->job[0] = t;
  start = t;

  for (i = 1; i < OrderLen; i++)
  { min = MAXDOUBLE;
    for (j = 0; j < OrderLen - i; j++)
    { p = get_next_0_bit(bit, j + 1);
      len = (CalcLine) ? calc_line(t, p) + calc_line(start, p) :
                         get_line(t, p) + get_line(start, p);
      if (len < min)
      { min = len;
        pos = p;
      }
    }
    t = pos;
    set_bit(bit, t);
    c->myrep->job[i] = t;
  }

  free_bit(bit);

  trace("in_cheapest_insertion() completed");
}


/*****************************************************************************/
/* Farthest insertion population init                                        */
/*****************************************************************************/
void in_farthest_insertion(c)
  CHROM *c;		/* pointer to individual */
{ register TOUR i, j, t, start, p, pos = 0.0;
  BIT *bit;
  double len, max;

  trace("in_farthest_insertion() entered");

  bit = alloc_bit(OrderLen);
  clear_bits(bit);

  t = (TOUR) equal_unsigned_random(OrderLen);
  set_bit(bit, t);
  c->myrep->job[0] = t;
  start = t;

  for (i = 1; i < OrderLen; i++)
  { max = MINDOUBLE;
    for (j = 0; j < OrderLen - i; j++)
    { p = get_next_0_bit(bit, j + 1);
      len = (CalcLine) ? calc_line(t, p) + calc_line(start, p) :
                         get_line(t, p) + get_line(start, p);
      if (len > max)
      { max = len;
        pos = p;
      }
    }
    t = pos;
    set_bit(bit, t);
    c->myrep->job[i] = t;
  }

  free_bit(bit);

  trace("in_farthest_insertion() completed");
}


/*****************************************************************************/
/* Random population init without tour file                                  */
/*****************************************************************************/
void in_without_random(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;

  trace("in_without_random() entered");

  rnd = equal_unsigned_random(4);
  switch (rnd)
  { case 0:
      in_random(c);
      break;
    case 1:
      in_nearest_neighbour(c);
      break;
    case 2:
      in_cheapest_insertion(c);
      break;
    case 3:
      in_farthest_insertion(c);
      break;
    default:
      break;
  }

  trace("in_without_random() completed");
}


/*****************************************************************************/
/* Random population init with tour file                                     */
/*****************************************************************************/
void in_all_random(c)
  CHROM *c;		/* pointer to individual */
{ register unsigned rnd;

  trace("in_all_random() entered");

  rnd = equal_unsigned_random(5);
  switch (rnd)
  { case 0:
      in_random(c);
      break;
    case 1:
      in_tour_file(c);
      break;
    case 2:
      in_nearest_neighbour(c);
      break;
    case 3:
      in_cheapest_insertion(c);
      break;
    case 4:
      in_farthest_insertion(c);
      break;
    default:
      break;
  }

  trace("in_all_random() completed");
}


/*****************************************************************************/
/* Population initialization                                                 */
/*****************************************************************************/
void init_pop()
{ register int i;
  CHROM *old, *new;

  trace("init_pop() entered");

  for (i = 0; i < PopSize; i++)
  { old = (CHROM *) &OldPop[P]->rep[i];
    new = (CHROM *) &NewPop[P]->rep[i];

    switch (PopInit)
    { case POP_RND:
        in_random(new);
        break;
      case POP_TOU:
        in_tour_file(new);
        break;
      case POP_NEA:
        in_nearest_neighbour(new);
        break;
      case POP_CHE:
        in_cheapest_insertion(new);
        break;
      case POP_FAR:
        in_farthest_insertion(new);
        break;
      case POP_ALT:
        in_without_random(new);
        break;
      case POP_ALL:
        in_all_random(new);
        break;
      default:
        break;
    }

    new->mutprob = MutRate;
    old->mutprob = MutRate;
    new->mutdim = MutDim;
    old->mutdim = MutDim;
    new->needsEval = TRUE;
    new->myGeneration = Generation[P];

    normalize(new);
    filter_twins(new, NewPop[P], i);
    evaluate(new);

    NewPop[P]->fitvec[i] = i;
    OldPop[P]->fitvec[i] = i;
  }

  trace("init_pop() completed");
}


/*** end of file ***/
