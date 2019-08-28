/* $Id: replace.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : replace.c                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "bit.h"
#include "random.h"
#include "trace.h"
#include "replace.h"


/*****************************************************************************/
/* Compare tours from two individuals                                        */
/*****************************************************************************/
static TOUR compare_order(CHROM *c1, CHROM *c2)
{ register TOUR i, cmp = 0;
  MYREP *o1 = c1->myrep;
  MYREP *o2 = c2->myrep;

  for (i = 0; i < o1->n; i++)
  { if (o1->job[i] == o2->job[i])
    { cmp++;
    }
  }

  return(cmp);
}


/*****************************************************************************/
/* Random individual replace selection                                       */
/*****************************************************************************/
void re_random(oldpop, newpop, bit)
  POPULATION *oldpop;	/* parents */
  POPULATION *newpop;	/* childs */
  BIT *bit;		/* choosed parents */
{ register int i, p;
  register unsigned rnd;

  trace("re_random() entered");

  clear_bits(bit);
  p = oldpop->size;
  for (i = NumOff; i < oldpop->size; i++)
  { rnd = 1 + equal_unsigned_random(p--);
    set_next_bit(bit, rnd);
  }

  trace("re_random() completed");
}


/*****************************************************************************/
/* Crowding replace selection                                                */
/*****************************************************************************/
void re_crowding(oldpop, newpop, bit)
  POPULATION *oldpop;	/* pointer to parents */
  POPULATION *newpop;	/* pointer to childs */
  BIT *bit;		/* bit field of choosed parents */
{ register int i, j, p;
  register TOUR cmp, cmpmax;
  register unsigned rnd, pos, pmax = 0;
  BIT *rb;

  trace("re_crowding() entered");

  rb = alloc_bit(oldpop->size);

  clear_bits(bit);

  for (i = 0; i < NumOff; i++)
  { clear_bits(rb);
    p = oldpop->size - i;
    for (j = 0; j < i; j++)
    { set_bit(rb, get_next_1_bit(bit, j + 1));
    }
    cmpmax = 0;
    for (j = 0; j < CrowFactor; j++)
    { rnd = 1 + equal_unsigned_random(p--);
      pos = get_next_0_bit(rb, rnd);
      cmp = compare_order(&newpop->rep[i], &oldpop->rep[pos]);
      if (cmp >= cmpmax)
      { cmpmax = cmp;
        pmax = pos;
      }
    }
    set_bit(bit, pmax);
  }

  invert_bits(bit);

  trace("re_crowding() completed");
}


/*****************************************************************************/
/* Replace selection (dispersal)                                             */
/*****************************************************************************/
void replace(oldpop, newpop, bit)
  POPULATION *oldpop;	/* pointer to parents */
  POPULATION *newpop;	/* pointer to childs */
  BIT *bit;		/* bit field of choosed parents */
{
  trace("replace() entered");

  switch (Replace)
  { case REP_RND:
      re_random(oldpop, newpop, bit);
      break;
    case REP_CRW:
      re_crowding(oldpop, newpop, bit);
      break;
    default:
      break;
  }

  trace("replace() completed");
}


/*** end of file ***/
