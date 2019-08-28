/* $Id: elite.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : elite.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "bit.h"
#include "chrom.h"
#include "eval.h"
#include "filtwins.h"
#include "mutation.h"
#include "normal.h"
#include "random.h"
#include "trace.h"
#include "elite.h"


/*****************************************************************************/
/* Random individual elite selection                                         */
/*****************************************************************************/
int es_random(bit, num)
  BIT *bit;		/* pointer to selection bit field */
  int num;		/* number of selectable individuals */
{ int idx;

  trace("es_random() entered");

  idx = set_next_bit(bit, 1 + equal_unsigned_random(num));

  trace("es_random() completed");

  return(idx);
}


/*****************************************************************************/
/* Weakest individual elite select                                           */
/*****************************************************************************/
int es_weakest(pop, bit)
  POPULATION *pop;	/* pointer to population */
  BIT *bit;		/* pointer to selection bit field */
{ register int i, idx = 0;
  double maxfit = MINDOUBLE;

  trace("es_weakest() entered");

  for (i = 0; i < pop->size; i++)
  { if (! get_bit(bit, i))
    { if (pop->rep[i].fitness >= maxfit)
      { maxfit = pop->rep[i].fitness;
        idx = i;
      }
    }
  }
  set_bit(bit, idx);

  trace("es_weakest() completed");

  return(idx);
}


/*****************************************************************************/
/* First weaker individual elite selection                                   */
/*****************************************************************************/
int es_first_weaker(pop, bit, num, ind)
  POPULATION *pop;	/* pointer to population */
  BIT *bit;		/* pointer to selection bit field */
  int num;		/* number of selectable individuals */
  CHROM *ind;		/* pointer to elite individual */
{ register int i, idx = 0;
  BOOLEAN found = FALSE;

  trace("es_first_weaker() entered");

  for (i = 0; (i < pop->size) && (! found); i++)
  { if (! get_bit(bit, i))
    { if (pop->rep[i].fitness > ind->fitness)
      { idx = i;
        set_bit(bit, idx);
        found = TRUE;
      }
    }
  }

  if (! found)
  { idx = es_random(bit, num);
  }

  trace("es_first_weaker() completed");

  return(idx);
}


/*****************************************************************************/
/* Elite selection (sacrifice selection)                                     */
/*****************************************************************************/
int elite_select(pop, bit, num, ind)
  POPULATION *pop;	/* pointer to population */
  BIT *bit;		/* pointer to selection bit field */
  int num;		/* number of selectable individuals */
  CHROM *ind;		/* pointer to elite individual */
{ register int res;

  trace("elite_select() entered");

  switch (EliteSelect)
  { case ELS_RND:
      res = es_random(bit, num);
      break;
    case ELS_WEA:
      res = es_weakest(pop, bit);
      break;
    case ELS_FIW:
      res = es_first_weaker(pop, bit, num, ind);
      break;
    default:
      res = 0;
      break;
  }

  trace("elite_select() completed");

  return(res);
}


/*****************************************************************************/
/* Elitism                                                                   */
/*****************************************************************************/
void elite()
{ register int i, idx;
  CHROM *ind, *curr;
  BIT *bit;

  trace("elite() entered");

  switch (Elite)
  { case ELI_NOP:
      break;
    case ELI_NOR:
    case ELI_MUT:
      bit = alloc_bit(PopSize);
      clear_bits(bit);
      for (i = 0; i < EliteHold; i++)
      { curr = (CHROM *) &OldPop[P]->rep[OldPop[P]->fitvec[PopSize - 1 - i]];
        idx = elite_select(NewPop[P], bit, PopSize - i, curr);
        ind = (CHROM *) &NewPop[P]->rep[idx];
        order_copy(ind, curr);
        if (Elite == ELI_MUT)
        { mutation(ind);
          normalize(ind);
          filter_twins(ind, NewPop[P], PopSize);
          evaluate(ind);
          ind->myGeneration = Generation[P];
        }
      }
      free_bit(bit);
      break;
    default:
      break;
  }

  trace("elite() completed");
}


/*** end of file ***/
