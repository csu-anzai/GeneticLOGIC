/* $Id: control.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : control.c                                                     */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef USE_CURSES
#include <curses.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "trace.h"
#include "control.h"


#define ABORT_CHAR	'q'	/* press this key to abort */


static double bestfit;		/* best fitness */


/*****************************************************************************/
/* Show fitness values of a population on display                            */
/*****************************************************************************/
void control()
{ register int j;
  register TOUR i;
  CHROM *theBest = &NewPop[P]->rep[NewPop[P]->fitvec[PopSize - 1]];
  CHROM *theWorst = &NewPop[P]->rep[NewPop[P]->fitvec[0]];
  MYREP *o;
  double sum = 0.0;
#ifdef USE_CURSES
  int ch;
#endif

  trace("control() entered");

  if (Generation[P] == 0)
  { bestfit = MAXDOUBLE;
  }

#ifdef USE_CURSES
  if (TRUE)
#else
  if (Best[P] < bestfit)
#endif
  {
    if (Best[P] < bestfit)
    { bestfit = Best[P];
    }

    if (SaveSize)
    {
#ifdef USE_CURSES
      if (Generation[P] == 0)
      { wclear(MyWin);
      }
      else
      { werase(MyWin);
      }
      mvwprintw(MyWin, 10, 0, "Optimum   : %lu\n", (unsigned long)
        BestSet[P][0].quality);
#else
      printf("Optimum   : %lu     \n", (unsigned long)
        BestSet[P][0].quality);
#endif

      o = BestSet[P][0].myrep;
      for (i = 0; i < o->n; i++)
      {
#ifdef USE_CURSES
        wprintw(MyWin, " %3d", o->job[i] + 1);
#else
        printf(" %3d", o->job[i] + 1);
#endif
      }
    }
    else
    {
#ifdef USE_CURSES
      werase(MyWin);
      mvwprintw(MyWin, 10, 0, "Optimal Fitness : %.3f\n", Best[P]);
#else
      printf("Optimal Fitness : %.3f\n", Best[P]);
#endif
    }

    for (j = 0; j < PopSize; j++)
    { sum += NewPop[P]->rep[j].quality;
    }

#ifdef USE_CURSES
    mvwprintw(MyWin, 0, 0, "Generation: %-5d  (Experiment: %d)",
      Generation[P], Experiment);
    if (PopNum > 1)
    { wprintw(MyWin, "    Population: %d\n", P * NProcs + MyProcID + 1);
    }
    else
    { wprintw(MyWin, "\n");
    }
    wprintw(MyWin, "-- Tour length -------\n");
    wprintw(MyWin, "Best      : %lu\n", (unsigned long) theBest->quality);
    wprintw(MyWin, "Worst     : %lu\n", (unsigned long) theWorst->quality);
    wprintw(MyWin, "Average   : %lu\n", (unsigned long) (sum / PopSize));
    wprintw(MyWin, "-- Fitness -----------\n");
    wprintw(MyWin, "Best      : %.3f\n", theBest->fitness);
    wprintw(MyWin, "Worst     : %.3f\n", theWorst->fitness);
    wprintw(MyWin, "Average   : %.3f\n", NewPop[P]->fitsum / PopSize);
    wprintw(MyWin, "=====================================================\n");
    wrefresh(MyWin);
#else
    printf("\nGeneration: %-5d  (Experiment: %d)", Generation[P],
      Experiment);
    if (PopNum > 1)
    { printf("    Population: %d\n", P * NProcs + MyProcID + 1);
    }
    else
    { printf("\n");
    }
    printf("-- Tour length -------\n");
    printf("Best      : %lu\n", (unsigned long) theBest->quality);
    printf("Worst     : %lu\n", (unsigned long) theWorst->quality);
    printf("Average   : %lu\n", (unsigned long) (sum / PopSize));
    printf("-- Fitness -----------\n");
    printf("Best      : %.3f\n", theBest->fitness);
    printf("Worst     : %.3f\n", theWorst->fitness);
    printf("Average   : %.3f\n", NewPop[P]->fitsum / PopSize);
    printf("=====================================================\n");
#endif
  }
  else
  {
#ifndef USE_CURSES
    printf("Generation: %d\r", Generation[P]);
    fflush(stdout);
#endif
  }

#ifdef USE_CURSES
  ch = (int) getch();
  if (ch == (int) ABORT_CHAR)
  { KeyFlag = TRUE;
  }
#endif

  trace("control() completed");
}


/*** end of file ***/
