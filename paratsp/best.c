/* $Id: best.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : best.c                                                        */
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
#include "fopen.h"
#include "trace.h"
#include "best.h"


static double max;	/* minimum (best) fitness in the BestSet */
static int maxptr;	/* pointer to structure with fitness = max */
 
 
/*****************************************************************************/
/* Copy an individual to the best structure                                  */
/*****************************************************************************/
void copy_best(best, ind)
  BESTCHROM *best;	/* pointer to target */
  CHROM *ind;		/* pointer to source */
{
  trace("copy_best() entered");

  memcpy(best->myrep->job, ind->myrep->job, best->myrep->n * sizeof(TOUR));
  best->myGeneration = ind->myGeneration;
  best->myTrial = ind->myTrial;
  best->quality = ind->quality;
  best->fitness = ind->fitness;

  trace("copy_best() completed");
}


/*****************************************************************************/
/* Save an individual if it is one of the SaveSize best seen so far          */
/*****************************************************************************/
void save_best(ind)
  CHROM *ind;		/* individual to save */
{ register int i, ptr;
  register TOUR j;
  BOOLEAN found;

  trace("save_best() entered");

  for (i = 0, found = FALSE; (i < BestSize[P]) && (! found); i++)
  { for (j = 0, found = TRUE; (j < OrderLen) && (found); j++)
    { found = (ind->myrep->job[j] == BestSet[P][i].myrep->job[j]) ? TRUE :
        FALSE;
    }
  }

  if (! found) 
  { if (BestSize[P] < SaveSize)
    { ptr = BestSize[P];
      copy_best(&BestSet[P][ptr], ind);
      BestSize[P]++;
    }
    else
    { for (max = BestSet[P][0].fitness, maxptr = 0, i = 1; i < SaveSize; i++)
      { if (max < BestSet[P][i].fitness)
        { max = BestSet[P][i].fitness;
          maxptr = i;
        }
      }
      ptr = maxptr;
      if (ind->fitness < max)
      { copy_best(&BestSet[P][ptr], ind);
      }
    }
  }

  trace("save_best() completed");
}


/*****************************************************************************/
/* Print the best structure in a file                                        */
/*****************************************************************************/
void print_best(fname, mode)
  char *fname;		/* file name */
  char *mode;		/* file mode */
{ register TOUR i;
  FILE *fp;

  trace("print_best() entered");

  if (OutFlag)
  { if ((fp = file_open(fname, mode, TRUE)) != NULL)
    { fprintf(fp, "NAME : Created by %s\n", LOGO);
      fprintf(fp, "COMMENT : Optimum tour for '%s' (Length: %.6g)\n",
        TspFileName, BestSet[P][0].quality);
      fprintf(fp, "TYPE : TOUR\n");
      fprintf(fp, "DIMENSION : %d\n", OrderLen);
      fprintf(fp, "TOUR_SECTION\n");
      for (i = 0; i < OrderLen; i++)
      { fprintf(fp, "%d\n", 1 + BestSet[P][0].myrep->job[i]);
      }
      fprintf(fp, "-1\n");
      fprintf(fp, "EOF\n");
      fclose(fp);
    }
    else
    { sys_perror("PrintBest: open Minfile");
    }
  }
 
  trace("print_best() completed");
}


/*** end of file ***/
