/* $Id: chrom.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : chrom.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <string.h>
#else
#include <memory.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "other.h"
#include "trace.h"
#include "chrom.h"


/*****************************************************************************/
/* Copy an individual to an other individual                                 */
/*****************************************************************************/
void order_copy(dest, src)
  CHROM *dest;		/* pointer to target */
  CHROM *src;		/* pointer to source */
{
  trace("order_copy() entered");

  memcpy(dest->myrep->job, src->myrep->job, dest->myrep->n * sizeof(TOUR));
  dest->myGeneration = src->myGeneration;
  dest->myTrial = src->myTrial;
  dest->mutprob = src->mutprob;
  dest->mutdim = src->mutdim;
  dest->quality = src->quality;
  dest->fitness = src->fitness;
  dest->needsEval = src->needsEval;

  trace("order_copy() completed");
}


/*****************************************************************************/
/* Compare an individual with other individuals of a population              */
/*****************************************************************************/
BOOLEAN order_comp(pop, num, ind)
  POPULATION *pop;	/* pointer to population */
  int num;		/* number of individuals to compare */
  CHROM *ind;		/* pointer to individual */
{ register int i;
  BOOLEAN twin = FALSE;

  trace("order_comp() entered");

  for (i = 0; (i < num) && (! twin); i++)
  { if (ind != &pop->rep[i])
    { if (! memcmp(ind->myrep->job, pop->rep[i].myrep->job, ind->myrep->n *
         sizeof(TOUR)))
      { twin = TRUE;
      }
    }
  }

  trace("order_comp() completed");

  return(twin);
}


/*****************************************************************************/
/* Allocate memory for a population                                          */
/*****************************************************************************/
POPULATION *alloc_pop(popsize)
  int popsize;		/* size of population */
{ register int i;
  POPULATION *pop;

  trace("alloc_pop() entered");

  pop = (POPULATION *) emalloc((unsigned long) sizeof(POPULATION), TRUE);
  pop->size = popsize;
  pop->fitvec = (int *) emalloc((unsigned long) popsize * sizeof(int), TRUE);
  pop->rep = (CHROM *) emalloc((unsigned long) popsize * sizeof(CHROM), TRUE);

  for (i = 0; i < popsize; i++)
  { pop->rep[i].myrep = (MYREP *) emalloc((unsigned long) sizeof(MYREP),
      TRUE);
    pop->rep[i].myrep->n = OrderLen;
    pop->rep[i].myrep->job = (TOUR *) emalloc((unsigned long) OrderLen *
      sizeof(TOUR), TRUE);
  }

  trace("alloc_pop() completed");

  return(pop);
}


/*****************************************************************************/
/* Free memory of population                                                 */
/*****************************************************************************/
void free_pop(pop)
  POPULATION *pop;	/* pointer to population */
{ register int i;

  trace("free_pop() entered");

  for (i = 0; i < pop->size; i++)
  { free(pop->rep[i].myrep->job);
    free(pop->rep[i].myrep);
  }

  free(pop->rep);
  free(pop->fitvec);
  free(pop);

  trace("free_pop() completed");
}


/*****************************************************************************/
/* Allocate memory for BestSet                                               */
/*****************************************************************************/
BESTCHROM *alloc_best(bestsize)
  int bestsize;		/* size of BestSet */
{ register int i;
  BESTCHROM *best;

  trace("alloc_best() entered");

  best = (BESTCHROM *) emalloc((unsigned long) bestsize * sizeof(BESTCHROM),
    TRUE);

  for (i = 0; i < bestsize; i++)
  { best[i].myrep = (MYREP *) emalloc((unsigned long) sizeof(MYREP),
      TRUE);
    best[i].myrep->n = OrderLen;
    best[i].myrep->job = (TOUR *) emalloc((unsigned long) OrderLen *
      sizeof(TOUR), TRUE);
  }

  trace("alloc_best() completed");

  return(best);
}


/*****************************************************************************/
/* Free memory of BestSet                                                    */
/*****************************************************************************/
void free_best(best)
  BESTCHROM *best;	/* pointer to BestSet */
{ register int i;

  trace("free_best() entered");

  if (SaveSize)
  { for (i = 0; i < SaveSize; i++)
    { free(best[i].myrep->job);
      free(best[i].myrep);
    }
    free(best);
  }

  trace("free_best() completed");
}


/*** end of file ***/
