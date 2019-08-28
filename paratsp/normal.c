/* $Id: normal.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : normal.c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "mutation.h"
#include "trace.h"
#include "normal.h"


/*****************************************************************************/
/* Normalize an individual                                                   */
/*****************************************************************************/
void normalize(ind)
  CHROM *ind;		/* pointer to individual */
{
  trace("normalize() entered");

  if (Normalize)
  { while (ind->myrep->job[0] != NormNum - 1)
    { move_mutation(ind, 0, OrderLen - 1);
    }
    if (ind->myrep->job[1] > ind->myrep->job[OrderLen - 1])
    { invert_mutation(ind, 1, OrderLen - 1);
    }
  }

  trace("normalize() completed");
}


/*** end of file ***/
