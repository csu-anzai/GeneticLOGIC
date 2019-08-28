/* $Id: filtwins.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : filtwins.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "chrom.h"
#include "mutation.h"
#include "normal.h"
#include "trace.h"
#include "filtwins.h"


/*****************************************************************************/
/* Filter twins of individuals in population                                 */
/*****************************************************************************/
void filter_twins(ind, pop, num)
  CHROM *ind;		/* pointer to individual to compare */
  POPULATION *pop;	/* pointer to population */
  int num;		/* number of individuals to compare */
{ register int i;
  double hlp;

  trace("filter_twins() entered");

  if (FilterTwins)
  { hlp = ind->mutprob;
    ind->mutprob = FILTER_MUTPROB;
    for (i = 0; (i < MAX_FILTER) && order_comp(pop, num, ind); i++)
    { mutation(ind);
      normalize(ind);
    }
    ind->mutprob = hlp;
  }

  trace("filter_twins() completed");
}


/*** end of file ***/
