/* $Id: converge.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : converge.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "other.h"
#include "trace.h"
#include "converge.h"


/*****************************************************************************/
/* Measure system convergence                                                */
/*****************************************************************************/
void converge()
{ register int i, ons;
  register TOUR j, k;
  FILE *fp;
  TOUR *p;

  trace("converge() entered");
 
  p = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR), TRUE);

  for (j = 0, Lost[P] = 0, Conv[P] = 0; j < OrderLen; j++)
  { for (k = 0; k < OrderLen; k++)
    { p[k] = 0;
    }
    for (i = 0; i < PopSize; i++)
    { p[NewPop[P]->rep[i].myrep->job[j]]++;
    }
    ons = p[0];
    for (k = 1; k < OrderLen; k++)
    { if (p[k] > ons)
      { ons = p[k];
      }
    }
    Lost[P] += (ons == PopSize) ? 1 : 0;
    Conv[P] += (ons >= PopSize - (PopSize / CONVDIV)) ? 1 : 0;
  }
 
  if (OutFlag && (Lost[P] == OrderLen))
  { if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
    { fprintf(fp, "CONVERGED at Generation %1d, ", Generation[P]);
      fprintf(fp, "after %1d Trials\n", Trials[P]);
      fclose(fp);
    }
    else
    { sprintf(Msg, "Converge: can't open '%s'", get_fname(LogFile));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }

  free(p);
 
  trace("converge() completed");
}

 
/*** end of file ***/
