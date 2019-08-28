/* $Id: done.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : done.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "trace.h"
#include "done.h"


/*****************************************************************************/
/* Check if done                                                             */
/*****************************************************************************/
int done()
{ register int res = 0;
  BOOLEAN minq;

  trace("done() entered");

  if (SaveSize)
  { minq = (BestSet[P][0].quality <= MinQuality) ? TRUE : FALSE;
  }
  else
  { minq =
      (NewPop[P]->rep[NewPop[P]->fitvec[PopSize - 1]].quality <= MinQuality) ?
      TRUE : FALSE;
  }

  if (NoTermFlag)
  { if (TotalTrials == 0)
    { res = ((Generation[P] >= TotalGenerations) ||
             (minq)                              ||
             (KeyFlag));
    }
    else
    { res = ((Trials[P] >= TotalTrials) ||
             (minq)                     ||
             (KeyFlag));
    }
  }
  else
  { if (TotalTrials == 0)
    { res = ((Generation[P]         >= TotalGenerations) || 
             (Lost[P]               >= OrderLen)         || 
             (Spin[P]               >= MaxSpin)          ||
             (minq)                                      ||
             (KeyFlag));
    }
    else
    { res = ((Trials[P]             >= TotalTrials) || 
             (Lost[P]               >= OrderLen)    || 
             (Spin[P]               >= MaxSpin)     ||
             (minq)                                 ||
             (KeyFlag));
    }
  }

  trace("done() completed");

  return(res);
}
 

/*** end of file ***/
