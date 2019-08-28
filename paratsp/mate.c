/* $Id: mate.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : mate.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "bit.h"
#include "brselect.h"
#include "chrom.h"
#include "cross.h"
#include "elite.h"
#include "eval.h"
#include "filtwins.h"
#include "maselect.h"
#include "mutation.h"
#include "normal.h"
#include "optlocal.h"
#include "other.h"
#include "replace.h"
#include "trace.h"
#include "mate.h"


/*****************************************************************************/
/* Mating                                                                    */
/*****************************************************************************/
void mate()
{ register int i, j, p1, p2;
  CHROM *ind, *parent1, *parent2, *child[2];
  BIT *bit;

  trace("mate() entered");

  NumMates = (3 - CrossOff) * NumOff;
  MaxNumMates = (3 - CrossOff) * PopSize;
  if (CrossTwoOff && (NumOff % 2))
  { NumMates++;
  }
  MatePool = (int *) emalloc((unsigned long) (MaxNumMates + 1) * sizeof(int),
    TRUE);

  breeder_select(OldPop[P], MatePool, NumMates);

  for (i = 0; i < NumOff; i++)
  { for (j = 0; j < CrossOff; j++)
    { child[j] = (CHROM *) &NewPop[P]->rep[i + j];
    }
    if ((! CrossTwoOff) || (i + 1 >= NumOff))
    { child[1] = NULL;
    }
    if (i < NumCross)
    { p1 = mate_select();
      parent1 = (CHROM *) &OldPop[P]->rep[p1];
      p2 = mate_select();
      parent2 = (CHROM *) &OldPop[P]->rep[p2];
      crossover(parent1, parent2, child[0], child[1]);
    }
    else
    { p1 = mate_select();
      parent1 = (CHROM *) &OldPop[P]->rep[p1];
      order_copy(child[0], parent1);
      if (CrossTwoOff && (child[1] != NULL))
      { p2 = mate_select();
        parent2 = (CHROM *) &OldPop[P]->rep[p2];
        order_copy(child[1], parent2);
      }
    }
    for (j = 0; (j < CrossOff) && (child[j] != NULL); j++)
    { mutation(child[j]);
      if (AllLocalOpt)
      { opt_local(child[j]);
      }
      normalize(child[j]);
      filter_twins(child[j], NewPop[P], i + j);
      evaluate(child[j]);
    }
    if (CrossTwoOff)
    { i++;
    }
  }
  free(MatePool);

  bit = alloc_bit(PopSize);
  replace(OldPop[P], NewPop[P], bit);
  for (i = 0, j = NumOff; i < PopSize; i++)
  { if (get_bit(bit, i))
    { ind = (CHROM *) &NewPop[P]->rep[j];
      order_copy(ind, &OldPop[P]->rep[i]);
      filter_twins(ind, NewPop[P], j);
      evaluate(ind);
      j++;
    }
  }
  free_bit(bit);

  elite();

  trace("mate() completed");
}


/*** end of file ***/
