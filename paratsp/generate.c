/* $Id: generate.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : generate.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "best.h"
#include "chkpoint.h"
#include "commpop.h"
#include "control.h"
#include "done.h"
#include "dump.h"
#include "error.h"
#include "fopen.h"
#include "graphics.h"
#include "init.h"
#include "logit.h"
#include "mate.h"
#include "measure.h"
#include "parallel.h"
#include "trace.h"
#include "generate.h"


static POPULATION *SortPop;	/* pointer to sorted population */


/*****************************************************************************/
/* Compare function for sorting                                              */
/*****************************************************************************/
static int compare(const void *idx1, const void *idx2)
{ CHROM *c1 = (CHROM *) &SortPop->rep[*((unsigned *) idx1)];
  CHROM *c2 = (CHROM *) &SortPop->rep[*((unsigned *) idx2)];
  double fit1 = c1->fitness;
  double fit2 = c2->fitness;
  int gen1, gen2, t1, t2;

  if (fit1 > fit2)
    return(-1);
  if (fit1 < fit2)
    return(1);

  gen1 = c1->myGeneration;
  gen2 = c2->myGeneration;
  if (gen1 > gen2)
    return(1);
  if (gen1 < gen2)
    return(-1); 

  t1 = c1->myTrial;
  t2 = c2->myTrial;
  if (t1 > t2)
    return(1);
  if (t1 < t2)
    return(-1);

  return(0);
}


/*****************************************************************************/
/* Sort a population                                                         */
/*****************************************************************************/
void sort_pop(pop)
  POPULATION *pop;	/* pointer to population */
{ register int i;
  CHROM *c;
  double worstFit, hlp;

  trace("sort_pop() entered");

  SortPop = pop;

  qsort(pop->fitvec, PopSize, sizeof(int), compare);

  pop->fitsum = 0.0;
  pop->invfitsum = 0.0;
  worstFit = pop->rep[pop->fitvec[0]].fitness;
  for (i = 0; i < PopSize; i++)
  { c = (CHROM *) &pop->rep[i];
    pop->fitsum += c->fitness;
    hlp = worstFit - c->fitness;
    pop->invfitsum += MAX(hlp, MINDOUBLE);
  }

  trace("sort_pop() completed");
}


/*****************************************************************************/
/* Dump informations in files                                                */
/*****************************************************************************/
void dump_infos()
{
  trace("dump_infos() entered");

  if ((PgmFreq > 0) && (Generation[P] % PgmFreq == 0))
  { dump_tour(PopSize - 1);	
  }

  if (OutFlag && (DumpFreq > 0) && (Generation[P] % DumpFreq == 0))
  { sprintf(DumpFile, "%s.%03d", CkptFile, CurrDump[P]);
    CurrDump[P]++;
    if (PrintPopFlag)
    { dump_pop(NewPop[P], Generation[P], PopSize, DumpFile);	
      sprintf(TourFile, "%s.%d.%d", "tour", Experiment, Generation[P]);
      print_best(TourFile, "a");
    }
  }
  else
  { if (DoneFlag[P])
    { if (LastFlag)
      { checkpoint(CkptFile);
      }
      else
      { if (SaveSize)
        { print_best(BestFile, "w");
        }
      }
    }
  }

  trace("dump_infos() completed");
}


/*****************************************************************************/
/* Optimate a generation of population                                       */
/*****************************************************************************/
void generate()
{ POPULATION *tmp;
  CHROM *ind;

  trace("generate() entered");

  for (P = 0; P < MyPopNum; P++)
  { if (TraceFlag && (P == 0))
    { log_num_int(stdout, "Generation", Generation[P], FALSE);
    }
 
    if ((P == 0) && (Generation[P] > 0))
    { if ((PopNum > 1) && (CommInt > 0) && (Generation[P] % CommInt == 0))
      { comm_pop();
      }
    }

    Spin[P]++;

    if (Generation[P] == 0)
    { initialize();
    }
    else
    { mate();
    }

    sort_pop(NewPop[P]);

    DoneFlag[P] = (done()) ? TRUE : FALSE;

    if (P == 0) 
    { DoneAllFlag = DoneFlag[P];
    }
    else
    { DoneAllFlag |= DoneFlag[P];
    }
  }

  DoneAllFlag = sync_done(DoneAllFlag);

  for (P = 0; P < MyPopNum; P++)
  { DoneFlag[P] = DoneAllFlag;

    measure();
    dump_infos();

    if (DisplayFlag && (PopDisplay == P * NProcs + MyProcID + 1))
    { control();
    }

    if (GraphicsFlag)
    { ind = &NewPop[P]->rep[NewPop[P]->fitvec[PopSize - 1]];
      if (! send_tour_graph(ind->quality, ind->myrep))
      { sprintf(Msg, "SendTourGraph: can't open '%s'", get_fname(GraphFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }

    tmp = OldPop[P];
    OldPop[P] = NewPop[P];
    NewPop[P] = tmp;

    Generation[P]++;
  }

  trace("generate() completed");
}


/*** end of file ***/
