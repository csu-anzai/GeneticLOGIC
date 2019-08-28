/* $Id: dump.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : dump.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "trace.h"
#include "dump.h"


/*****************************************************************************/
/* Create pgm file                                                           */
/*****************************************************************************/
void create_pgm()
{ FILE *fp;

  trace("create_pgm() entered");

  if (OutFlag && (PgmFreq > 0))
  { for (P = 0; P < MyPopNum; P++)
    { if ((fp = file_open(PgmFile, "w", TRUE)) != NULL)
      { fprintf(fp, "T1\n");
        fprintf(fp, "#\tTour dumps for %s\n", PARATSP);
        fprintf(fp, "%d\n", OrderLen);
        fclose(fp);
      }
      else
      { sprintf(Msg, "CrtPgm: can't open '%s'", get_fname(PgmFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
    P = 0;
  }

  trace("create_pgm() completed");
}


/*****************************************************************************/
/* Dump an individual in a file                                              */
/*****************************************************************************/
void dump_ind(fp, gen, ind, dim)
  FILE *fp;		/* file pointer */
  int gen;		/* generation */
  CHROM *ind;		/* pointer to individual */
  TOUR dim;		/* tour length */
{ register TOUR i, k;

  trace("dump_ind() entered");

  fprintf(fp, "%-5d ", gen);

  for (i = 1; i <= dim; i++)
  { k = ind->myrep->job[i - 1];
    fprintf(fp, "% 4d ", k + 1);
    fprintf(fp, "[%-16.16s] ", Town[k].name);
    if (i % 3 == 0)
    { fprintf(fp, "\n      ");
    }
  }

  if (dim % 3 != 0)
  { fprintf(fp, "\n      ");
  }
  fprintf(fp, "     Fitness     : %-12.3f\n", ind->fitness);
  fprintf(fp, "           Tour length : %-12.10g\n\n", ind->fitness);

  trace("dump_ind() completed");
}


/*****************************************************************************/
/* Dump a population in a file                                               */
/*****************************************************************************/
void dump_pop(pop, gen, popsiz, popfil)
  POPULATION *pop;	/* pointer to population */
  int gen;		/* generation */
  int popsiz;		/* size of population */
  char *popfil;		/* file name */
{ register int i;
  FILE *fp;
  CHROM *ind;

  trace("dump_pop() entered");

  if (OutFlag)
  { if ((fp = file_open(popfil, "a", TRUE)) != NULL)
    { for (i = 0; i < popsiz; i++)
      { ind = (CHROM *) &pop->rep[pop->fitvec[PopSize - 1 - i]];
        dump_ind(fp, gen, ind, OrderLen);
      }
      fclose(fp);
    }
    else
    { sprintf(Msg, "DumpPop: can't open '%s'", get_fname(popfil));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }

  trace("dump_pop() completed");
}


/*****************************************************************************/
/* Dump a tour in a file                                                     */
/*****************************************************************************/
void dump_tour(idx)
  int idx;		/* index of individual */
{ register TOUR i;
  FILE *fp;

  trace("dump_tour() entered");

  if (OutFlag)
  { if ((fp = file_open(PgmFile, "a", TRUE)) != NULL)
    { fprintf(fp, "\n");
      for (i = 0; i < OrderLen; i++)
      { fprintf(fp, "%4d ", 1 +
          NewPop[P]->rep[NewPop[P]->fitvec[idx]].myrep->job[i]);
        if ((i + 1) % 15 == 0)
        { fprintf(fp, "\n");
        }
      }
      fclose(fp);
    }
    else
    { sys_perror("DumpTour: Writing Pgmfile");
    }
  }

  trace("dump_tour() completed");
}
 
 
/*** end of file ***/
