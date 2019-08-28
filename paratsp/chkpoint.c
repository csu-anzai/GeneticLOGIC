/* $Id: chkpoint.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : chkpoint.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "best.h"
#include "error.h"
#include "fopen.h"
#include "trace.h"
#include "chkpoint.h"


/*****************************************************************************/
/* Save global variables in a file for later restart                         */
/*****************************************************************************/
void checkpoint(file)
  char *file;		/* file name */
{ register int i;
  register TOUR j;
  FILE *fp;

  trace("checkpoint() entered");
 
  if (OutFlag)
  { if ((fp = file_open(file, "w", TRUE)) != NULL)
    { fprintf(fp, "Experiment   : %d\n", Experiment);
      fprintf(fp, "TotalOnline  : %.3f\n", TotalOnline[P]);
      fprintf(fp, "TotalOffline : %.3f\n", TotalOffline[P]);
      fprintf(fp, "Generation   : %d\n", Generation[P]);
      fprintf(fp, "OnSum        : %.3f\n", OnSum[P]);
      fprintf(fp, "OffSum       : %.3f\n", OffSum[P]);
      fprintf(fp, "Trials       : %d\n", Trials[P]);
      fprintf(fp, "Plateau      : %d\n", Plateau[P]);
      fprintf(fp, "Best         : %.3f\n", Best[P]);
      fprintf(fp, "Worst        : %.3f\n", Worst[P]);
      fprintf(fp, "Spin         : %d\n", Spin[P]);
      fprintf(fp, "CurrDump     : %d\n", CurrDump[P]);
      fprintf(fp, "Seed         : %u\n", Seed);
      fprintf(fp, "InitSeed     : %u\n", InitSeed);
      fprintf(fp, "\n");

      fprintf(fp, "Window\n");
      for (i = 0; i < WindowSize; i++)
      { fprintf(fp, "%.3f\n", Win[P][i]);
      }
      fprintf(fp, "\n");
 
      for (i = 0; i < PopSize; i++)
      { fprintf(fp, "Individual #%d ---------------------\n", i + 1);
        fprintf(fp, "Tour:\n");
        for (j = 0; j < OrderLen; j++)
        { fprintf(fp, "%4d ", 1 + NewPop[P]->rep[i].myrep->job[j]);
          if ((j + 1) % 15 == 0)
          { fprintf(fp, "\n");
          }
        }
        if ((OrderLen - 1) % 15 != 0)
        { fprintf(fp, "\n");
        }
        fprintf(fp, "\n");
        fprintf(fp, "Generation : %d\n", NewPop[P]->rep[i].myGeneration);
        fprintf(fp, "Trial      : %d\n", NewPop[P]->rep[i].myTrial);
        fprintf(fp, "MutProb    : %.4f\n", NewPop[P]->rep[i].mutprob);
        fprintf(fp, "MutDim     : %.3f\n", NewPop[P]->rep[i].mutdim);
        fprintf(fp, "Quality    : %.3f\n", NewPop[P]->rep[i].quality);
        fprintf(fp, "Fitness    : %.3f\n", NewPop[P]->rep[i].fitness);
        fprintf(fp, "NeedsEval  : %d\n",
          (NewPop[P]->rep[i].needsEval) ? 1 : 0);
        fprintf(fp, "\n");
      }
      fclose(fp);
    }
    else
    { sprintf(Msg, "Checkpoint: can't open '%s'", get_fname(file));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }
 
  if (SaveSize)
  { print_best(BestFile, "w");
  }
 
  trace("checkpoint() completed");
}
 

/*** end of file ***/
