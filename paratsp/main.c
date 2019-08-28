/* $Id: main.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/*                     ------ Diplomarbeit ------                            */
/* ------------------------------------------------------------------------- */
/* Betreuer: Prof. Dr. rer. nat. F. Stuchlik                                 */
/*           Dr. rer. nat. H. Heuer                                          */
/* ------------------------------------------------------------------------- */
/* University of Magdeburg (Germany), Computer Science, April 1994           */
/* ------------------------------------------------------------------------- */
/* PARATSP computes an near optimal tour of Travelling Salesman Problem.     */
/* ------------------------------------------------------------------------- */
/* Dateiname : main.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

/* ========================================================================= */
/* ParaTSP uses sources from GeneSYS 1.0                                     */
/* and from Schoeneburg, u.a.                                                */
/* ========================================================================= */
   /*************************************************************/
   /* Schoeneburg, Heinzmann, Feddersen:                        */
   /* Genetische Algorithmen und Evolutionsstrategien.          */
   /* Bonn: Addison-Wesley, 1994.                               */
   /*************************************************************/
   /*************************************************************/
   /*                                                           */
   /*  Copyright (c) 1986                                       */
   /*  John J. Grefenstette                                     */
   /*  Navy Center for Applied Research in AI                   */
   /*  Naval Research Laboratory                                */
   /*                                                           */
   /*  Permission is hereby granted to copy all or any part of  */
   /*  this program for free distribution.   The author's name  */
   /*  and this copyright notice must be included in any copy.  */
   /*                                                           */
   /*************************************************************/
   /*************************************************************/
   /*                                                          	*/
   /*  Copyright (c) 1990-1992                                  */
   /*  Thomas Baeck                                            	*/
   /*  Computer Science Department, LSXI                       	*/
   /*  University of Dortmund                                  	*/
   /*  Baroper Str. 301                                         */
   /*  D-4600 Dortmund 50                                       */
   /*                                                          	*/
   /*  e-mail: baeck@ls11.informatik.uni-dortmund.de            */
   /*								*/
   /*  Permission is hereby granted to copy all or any part of 	*/
   /*  this program for free distribution.   The author's name 	*/
   /*  and this copyright notice must be included in any copy. 	*/
   /*                                                           */
   /*************************************************************/
/* ========================================================================= */


#ifdef PARIX
#include <sys/time.h>
#else
#include <sys/times.h>
#include <sys/types.h>
#endif

#ifdef USE_CURSES
#include <curses.h>
#include <termios.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "dump.h"
#include "error.h"
#include "eval.h"
#include "fopen.h"
#include "generate.h"
#include "graphics.h"
#include "input.h"
#include "logit.h"
#include "other.h"
#include "parallel.h"
#include "readfile.h"
#include "schema.h"
#include "trace.h"
#include "main.h"


#define CMD1	"report -i in.%s"	/* report call */
#define CMD2	"mv in.%s %s"		/* input file in directory */


#ifdef USE_CURSES
static struct termios term;		/* terminal settings */
#endif

static double bestOpt, worstOpt, sumOpt;


/*****************************************************************************/
/* Main function of ParaTSP                                                  */
/*****************************************************************************/
int main(argc, argv)
  int argc;		/* number of command line arguments */
  char **argv;		/* pointer to command line arguments */
{ FILE *fp;
  time_t clock_time;
  char cmd[MAX_STR];
  long sum_gen = 0;
  double numcom, bestqual;
#ifdef PARIX
  unsigned int start_time = 0, end_time, run_time;
  double run_sec;
#else
  struct tms tmsstart, tmsend;
  clock_t start_time = 0, end_time, run_time;
  double run_sec, usr_sec;
  long clktck;
#endif

  trace("main() entered");

  init_parvars();

  if (MyProcID == DISP_PROC)
  { printf("%s is running ...\n", LOGO);
  }

  P = NO_LOGFILE;

  input(argc, argv);

#ifndef PARIX
  if ((clktck = sysconf(_SC_CLK_TCK)) < 0)
  { sprintf(Msg, "Main: sysconf error");
    critical_error(ERR_SYSCONF, Msg);
  }
#endif

  if (GraphicsFlag)
  { for (P = 0; P < MyPopNum; P++)
    { if (! send_nodes_graph())
      { sprintf(Msg, "SendNodesGraph: can't open '%s'", get_fname(GraphFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
    P = 0;
  }

  if (DisplayFlag && (MyProcID == DISP_PROC))
  { print_towns(stdout);
  }

  synchronize();

  if (OutFlag && (MyProcID == DISP_PROC))
  { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
    { time(&clock_time);
      strcpy(Msg, ctime(&clock_time));
      log_string(fp, "Started", Msg);

      log_int(fp, "Dimension", TownNum);

      numcom = facul(TownNum - 1) / 2;
      if (numcom >= 1.0)
      { sprintf(Msg, "%.10g", numcom);
      }
      else
      { sprintf(Msg, "> %e", MAXDOUBLE);
      }
      log_string(fp, "Number of combinations", Msg);
      fprintf(fp, "\n");

      fclose(fp);
    }
    else
    { sprintf(Msg, "Main: can't open '%s'", AllLogFile);
      critical_error(ERR_FILE_OPEN, Msg);
    }

    sprintf(cmd, CMD2, Suffix, Suffix);
    system(cmd);
  }

  if (DisplayFlag)
  { if (MyProcID == DISP_PROC)
    { printf("Press <ENTER> to start the GA ...\n");
      getchar();
#ifdef USE_CURSES
      initscr();
      noecho();
      cbreak();
      savetty();
      tcgetattr(STDIN_FILENO, &term);
      term.c_cc[VMIN] = 0;
      term.c_cc[VTIME] = 0;
      tcsetattr(STDIN_FILENO, TCSANOW, &term);
      MyWin = newwin(0, 0, 0, 0);
      wrefresh(MyWin);
#endif
    }
  }

  synchronize();

  if ((! OutFlag) && (MyProcID == DISP_PROC))
  { time(&clock_time);
    strcpy(Msg, ctime(&clock_time));
    log_string(stdout, "Started", Msg);
#ifdef PARIX
    start_time = TimeNowLow();
#else
    start_time = times(&tmsstart);
#endif
  }

  Running = TRUE;

  for (P = 0; P < MyPopNum; P++)
  { if (OutFlag)
    { if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
      { time(&clock_time);
        strcpy(Msg, ctime(&clock_time));
        log_string(fp, "Started", Msg);
#ifdef PARIX
        log_int(fp, "Processor", MyProcID);
#endif
        fclose(fp);
      }
      else
      { sprintf(Msg, "Main: can't open '%s'", get_fname(LogFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
    TotalOnline[P] = 0.0;
    TotalOffline[P] = 0.0;
    TotalBest[P] = 0.0;
  }

  Experiment = 1;

  bestOpt = MAXDOUBLE;
  worstOpt = MINDOUBLE;
  sumOpt = 0.0;

  do
  { if (TraceFlag)
    { log_num_int(stdout, "Experiment", Experiment, TRUE);
    }

    for (P = 0; P < MyPopNum; P++)
    { Generation[P] = 0;
      DoneFlag[P] = FALSE;
    }

    do
    { generate();
    }
    while (! DoneAllFlag);

    sum_gen += Generation[0];

    for (P = 0; P < MyPopNum; P++)
    { if (TraceFlag)
      { log_num_int(stdout, "Online", Online[P], FALSE);
        log_num_int(stdout, "Offline", Offline[P], FALSE);
        log_num_int(stdout, "Best", Best[P], FALSE);
      }
      TotalOnline[P] += Online[P];
      TotalOffline[P] += Offline[P];
      TotalBest[P] += Best[P];

      if (OutFlag)
      { if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
        { log_int(fp, "Experiment", Experiment);  
          log_int(fp, "Generation", Generation[P] - 1);  
          log_double(fp, "Converged to (in percent)",
            (double) 100.0 * Conv[P] / PopSize);
          log_int(fp, "Individuals dumped", (SaveSize) ? SaveSize : 1);
          fclose(fp);
        }
        else
        { sprintf(Msg, "Main: can't open '%s'", get_fname(LogFile));
          critical_error(ERR_FILE_OPEN, Msg);
        }
      }

      dump_pop(OldPop[P], Generation[P] - 1, (SaveSize) ? SaveSize : 1,
        LogFile);
    }

    bestqual = get_global_optimum();

    if (bestqual < bestOpt)
    { bestOpt = bestqual;
    }
    if (bestqual > worstOpt)
    { worstOpt = bestqual;
    }
    sumOpt += bestqual;

    if ((! OutFlag) && (MyProcID == DISP_PROC))
    {
#ifdef USE_CURSES
      if (DisplayFlag)
      { printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
      }
#endif
      sprintf(Msg, "Global Optimum for Experiment %d", Experiment);
      log_double(stdout, Msg, bestqual);
    }

    Experiment++;
  }
  while (Experiment <= TotalExperiments);

  for (P = 0; P < MyPopNum; P++)
  { TotalOnline[P] /= TotalExperiments;
    TotalOffline[P] /= TotalExperiments;
    TotalBest[P] /= TotalExperiments;

    if (OutFlag)
    { if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
      { log_int(fp, "Number of Experiments", TotalExperiments);
        log_double(fp, "Average Online", TotalOnline[P]);
        log_double(fp, "Average Offline", TotalOffline[P]);
        log_double(fp, "Average Best", TotalBest[P]);
        time(&clock_time);
        strcpy(Msg, ctime(&clock_time));
        log_string(fp, "Finished", Msg);
        fclose(fp);
      }
      else
      { sprintf(Msg, "Main: can't open '%s'", get_fname(LogFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
  }
  P = 0;

  synchronize();

  if (MyProcID == DISP_PROC)
  { if (OutFlag)
    { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
      { time(&clock_time);
        strcpy(Msg, ctime(&clock_time));
        log_string(fp, "Finished", Msg);
        if (TotalExperiments > 1)
        { log_double(fp, "Best Global Optimum", bestOpt);
          log_double(fp, "Worst Global Optimum", worstOpt);
          log_double(fp, "Average Global Optimum", sumOpt / TotalExperiments);
        }
        fclose(fp);
      }
      else
      { sprintf(Msg, "Main: can't open '%s'", AllLogFile);
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
    else
    {
#ifdef PARIX
      end_time = TimeNowLow();
      run_time = end_time - start_time;
      run_sec = (double) run_time / (double) CLK_TCK_LOW;
      printf("\n");
      log_int(stdout, "Runtime in Ticks", run_time);
      log_double(stdout, "Runtime in Seconds", run_sec);
#else
      end_time = times(&tmsend);
      run_time = end_time - start_time;
      run_sec = (double) run_time / (double) clktck;
      usr_sec = (double) (tmsend.tms_utime - tmsstart.tms_utime) /
        (double) clktck;
      printf("\n");
      log_double(stdout, "User time (sec)", usr_sec);
      log_double(stdout, "Runtime (sec)", run_sec);
#endif
      time(&clock_time);
      strcpy(Msg, ctime(&clock_time));
      log_string(stdout, "Finished", Msg);
      if (TotalExperiments > 1)
      { log_double(stdout, "Best Global Optimum", bestOpt);
        log_double(stdout, "Worst Global Optimum", worstOpt);
        log_double(stdout, "Average Global Optimum",
          sumOpt / TotalExperiments);
        log_double(stdout, "Average Runtime per Experiment (sec)",
          run_sec / TotalExperiments);
      }
      log_double(stdout, "Average Number of Generations",
        (double) sum_gen / (double) TotalExperiments);
    }
  }

#ifdef USE_CURSES
  if (DisplayFlag && (MyProcID == DISP_PROC))
  { resetty();
    nocbreak();
    echo();
    if (OutFlag)
    { printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
    }
  }
#endif

  free_schema();
  free_storage();
  free_line();
  free_town();
  free_tour();

  synchronize();

  end_parallel();

  Running = FALSE;

  if (OutFlag && ReportFlag)
  { if (MyProcID == DISP_PROC)
    { if (DisplayFlag)
      { printf("Report running...\n");
      }
      sprintf(cmd, CMD1, Suffix);
      system(cmd);
    }
  }

  trace("main() completed");

  return(ERR_NO);
}


/*** end of file ***/
