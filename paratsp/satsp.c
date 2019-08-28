/* $Id: satsp.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : satsp.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
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
#include <math.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "eval.h"
#include "fopen.h"
#include "getopt.h"
#include "interfac.h"
#include "logit.h"
#include "other.h"
#include "parallel.h"
#include "random.h"
#include "readfile.h"
#include "satsp.h"


#ifdef USE_CURSES
static struct termios term;
#endif

static char Algorithm;
static int Nmax;
static int Jmax;
static double TempStart;
static double TempCtrl;
static TOUR *MyTour, *OptTour;
static double MyLength, OptLength;
static BOOLEAN OptFlag, SADoneFlag;
static TOUR Cuts[4];
static int Time;
static double Temp;
static double bestOpt, worstOpt, sumOpt;


/*****************************************************************************/
/* Initialize default parameters                                             */
/*****************************************************************************/
void sa_init_values()
{
  init_values();
  OutFlag = FALSE;

  Algorithm = ALG_SAN;
  Nmax = 0;
  Jmax = 0;
  TempStart = 0.0;
  TempCtrl = 0.95;
}


/*****************************************************************************/
/* Read parameters                                                           */
/*****************************************************************************/
void sa_input(argc, argv)
  int argc;
  char **argv;
{ register int opt;

  sa_init_values();

  my_opterr = 0;
  while ((opt = get_options(argc, argv, SATSP_OPTSTR)) != EOF)
  { switch (opt)
    { case 'A':
        sscanf(my_optarg, "%s", TspFileName);
        break;
      case 'F':
        sscanf(my_optarg, "%lf", &TempCtrl);
        break;
      case 'J':
        sscanf(my_optarg, "%d", &Jmax);
        break;
      case 'N':
        sscanf(my_optarg, "%d", &Nmax);
        break;
      case 'S':
        sscanf(my_optarg, "%c", &Algorithm);
        break;
      case 'T':
        sscanf(my_optarg, "%lf", &TempStart);
        break;
      case 'd':
        DisplayFlag = TRUE;
        break;
      case 'e':
        sscanf(my_optarg, "%d", &TotalExperiments);
        break;
      case 'i':
        sscanf(my_optarg, "%d", &CommInt);
        break;
      case 'm':
        sscanf(my_optarg, "%lf", &MinQuality);
        break;
      case 'p':
        sscanf(my_optarg, "%d", &ProcNum);
        break;
      case 's':
        sscanf(my_optarg, "%d", &OrigSeed);
        break;
      case 't':
        sscanf(my_optarg, "%c", &Topology);
        break;
      case 'h':
      case '?':
        if (MyProcID == DISP_PROC)
        { printf(SATSP_USAGE);
        }
        exit(ERR_HELP);
        break;
      default:
        sprintf(Msg, "Input: illegal option -%c\n", opt);
        critical_error(ERR_OPTION, Msg);
        break;
    }
  }

  read_TSP_file(TspFileName);
  calc_all_lines();

  OrderLen = TownNum;

  PopNum = ProcNum;
  ParModel = PAR_NEI;
  init_population();

  Seed = OrigSeed;

  MyTour = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR), TRUE);
  OptTour = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR), TRUE);
}


/*****************************************************************************/
/* Calculate tour length                                                     */
/*****************************************************************************/
double sa_totallength(tour)
  TOUR *tour;
{ register int i, act, lst;
  double res;

  res = 0.0;
  for (i = 0; i < OrderLen; i++)
  { act = tour[i];
    lst = (i) ? tour[i - 1] : tour[OrderLen - 1];
    res += (CalcLine) ? calc_line(lst, act) : get_line(lst, act);
  }

  return(res);
}


/*****************************************************************************/
/* Evaluate new cuts                                                         */
/*****************************************************************************/
double sa_evaluate()
{ double profit;

  profit = (CalcLine) ? calc_line(Cuts[0], Cuts[2]) :
                        get_line(Cuts[0], Cuts[2]);
  profit += (CalcLine) ? calc_line(Cuts[3], Cuts[1]) :
                         get_line(Cuts[3], Cuts[1]);
  profit -= (CalcLine) ? calc_line(Cuts[0], Cuts[1]) :
                         get_line(Cuts[0], Cuts[1]);
  profit -= (CalcLine) ? calc_line(Cuts[2], Cuts[3]) :
                         get_line(Cuts[2], Cuts[3]);

  return(profit);
}


/*****************************************************************************/
/* Check if configuration accepts                                            */
/*****************************************************************************/
BOOLEAN sa_accept(profit, temp)
  double profit, temp;
{ register int res;
  double rnd;

  switch (Algorithm)
  { case ALG_SAN:
      rnd = equal_random();
      res = (profit < 0.0) || (rnd < exp(-profit / temp));
      break;
    case ALG_TAC:
      res = (profit < temp);
      break;
    default:
      res = 0;
      break;
  }

  return(res ? TRUE : FALSE);
}


/*****************************************************************************/
/* Reconnect tour                                                            */
/*****************************************************************************/
void sa_reconnect(cut1, cut2)
  TOUR cut1, cut2;
{ register int i, k, l, switches;
  register TOUR tmp;

  switches = ((cut2 - cut1 + OrderLen) % OrderLen) / 2;
  for (i = 0; i < switches; i++)
  { k = (1 + cut1 + i + OrderLen) % OrderLen;
    l = (1 + cut2 - i - 1 + OrderLen) % OrderLen;
    tmp = MyTour[k];
    MyTour[k] = MyTour[l];
    MyTour[l] = tmp;
  }
}


/*****************************************************************************/
/* Show configuration on display                                             */
/*****************************************************************************/
void sa_control()
{ register int i;
#ifdef USE_CURSES
  int ch;
#endif

#ifdef USE_CURSES
  werase(MyWin);
  mvwprintw(MyWin, 4, 0, "Optimum     : %lu\n", (unsigned long) OptLength);
  for (i = 0; i < OrderLen; i++)
  { wprintw(MyWin, " %3d", OptTour[i] + 1);
  }
  mvwprintw(MyWin, 0, 0, "Time        : %-5d  (Experiment: %d)\n", Time,
    Experiment);
  wprintw(MyWin, "Temperature : %.4f\n", Temp);
  wprintw(MyWin, "Tour length : %lu\n", (unsigned long) MyLength);
  wprintw(MyWin, "=====================================================\n");
  wrefresh(MyWin);
  ch = (int) getch();
  if (ch == (int) SATSP_ABORT_CHAR)
  { KeyFlag = TRUE;
  }
#else
  if (OptFlag)
  { printf("Optimum     : %lu     \n", (unsigned long) OptLength);
    for (i = 0; i < OrderLen; i++)
    { printf(" %3d", OptTour[i] + 1);
    }
    printf("\nTime        : %-5d  (Experiment: %d)\n", Time, Experiment);
    printf("Temperature : %.4f\n", Temp);
    printf("Tour length : %lu\n", (unsigned long) MyLength);
    printf("\n=====================================================\n");
  }
  else
  { printf("Time: %d\r", Time);
    fflush(stdout);
  }
#endif
}


/*****************************************************************************/
/* Initialize SA-algorithm                                                   */
/*****************************************************************************/
void sa_initialize()
{
  DoneAllFlag = FALSE;
  SADoneFlag = FALSE;
  InitSeed = Seed;
  init_rnd();
  Seed = equal_unsigned_random(MAX_SEED);

  Time = 0;
  if (TempStart == 0.0)
  { Temp = sqrt(OrderLen);
  }
  else
  { Temp = TempStart;
  }

  equal_random_int_vec(MyTour, OrderLen, OrderLen);
  MyLength = sa_totallength(MyTour);
  memcpy(OptTour, MyTour, OrderLen * sizeof(TOUR));
  OptLength = MyLength;

#ifdef USE_CURSES
  if (DisplayFlag && (MyProcID == DISP_PROC))
  { wclear(MyWin);
  }
#endif
}


/*****************************************************************************/
/* Check if temperature steps done                                           */
/*****************************************************************************/
BOOLEAN sa_temp_done(moves, succmoves)
  int moves, succmoves;
{ register int res;

  if (Jmax == 0)
  { res = ((moves >= OrderLen * 100) || (succmoves >= OrderLen * 10));
  }
  else
  { res = (moves >= Jmax);
  }

  return((res) ? TRUE : FALSE);
}


/*****************************************************************************/
/* Check if SA-algorithm done                                                */
/*****************************************************************************/
BOOLEAN sa_done(succmoves)
  int succmoves;
{ register int res;

  DoneAllFlag = ((KeyFlag) || (OptLength <= MinQuality)) ? TRUE : FALSE;
  if (Nmax == 0)
  { res = ((succmoves == 0) || (DoneAllFlag));
  }
  else
  { res = ((Time >= Nmax) || (DoneAllFlag));
  }

  return((res) ? TRUE : FALSE);
}


/*****************************************************************************/
/* Communicate with neighbour processors                                     */
/*****************************************************************************/
void sa_comm_neighbour()
{ register int i, j, m, destid, dir;
  TOUR *tour;
  double length;

  tour = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR), TRUE);

  for (i = 0; i < NProcs; i++)
  { for (j = 0; j < NeighbourNum; j++)
    { if (MyNeighbour[i][j] != NO_NEIGHBOUR)
      { destid = MyNeighbour[i][j];
        for (m = 0, dir = -1; (m < NeighbourNum) && (dir < 0); m++)
        { if (MyNeighbour[destid][m] == i)
          { dir = m;
          }
        }
        if (i == MyProcID)
        { sa_send_to_neighbour(j, MyTour);
        }
        else
        { if (destid == MyProcID)
          { sa_recv_from_neighbour(dir, tour);
            length = sa_totallength(tour);
            if (length < MyLength)
            { memcpy(MyTour, tour, OrderLen * sizeof(TOUR));
              MyLength = length;
            }
          }
        }
      }
    }
  }

  free(tour);

  if (MyLength < OptLength)
  { memcpy(OptTour, MyTour, OrderLen * sizeof(TOUR));
    OptLength = MyLength;
    OptFlag = TRUE;
  }
}


/*****************************************************************************/
/* SA/TA-algorithm                                                           */
/*****************************************************************************/
void sa_algorithm()
{ register int moves, succmoves, onpath;
  register TOUR cut1, cut2;
  double profit;

  sa_initialize();

  do
  { moves = 0;
    succmoves = 0;
    OptFlag = FALSE;
    do
    { moves++;
      do
      { cut1 = equal_unsigned_random(OrderLen);
        cut2 = equal_unsigned_random(OrderLen);
        onpath = (cut2 - cut1 + OrderLen) % OrderLen;
      }
      while ((onpath < 2) || (onpath >= OrderLen - 2));
      Cuts[0] = MyTour[cut1];
      Cuts[1] = MyTour[(cut1 + 1) % OrderLen];
      Cuts[2] = MyTour[cut2];
      Cuts[3] = MyTour[(cut2 + 1) % OrderLen];
      profit = sa_evaluate();
      if (sa_accept(profit, Temp))
      { succmoves++;
        sa_reconnect(cut1, cut2);
        MyLength += profit;
        if (MyLength < OptLength)
        { memcpy(OptTour, MyTour, OrderLen * sizeof(TOUR));
          OptLength = MyLength;
          OptFlag = TRUE;
        }
      }
    }
    while (! sa_temp_done(moves, succmoves));
    Time++;
    if ((NProcs > 1) && (CommInt > 0) && (Time % CommInt == 0))
    { sa_comm_neighbour();
    }
    if (DisplayFlag && (MyProcID == DISP_PROC))
    { sa_control();
    }
    Temp *= TempCtrl;
    SADoneFlag = sa_done(succmoves);
    DoneAllFlag = sa_sync_done(SADoneFlag, DoneAllFlag);
  }
  while (! DoneAllFlag);
}


/*****************************************************************************/
/* SATSP for ParaTSP                                                         */
/*****************************************************************************/
int main(argc, argv)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
{ time_t clock_time;
  long sum_temp = 0, sum_time = 0;
  double bestqual;
#ifdef PARIX
  unsigned int start_time = 0, end_time, run_time;
  double run_sec;
#else
  struct tms tmsstart, tmsend;
  clock_t start_time = 0, end_time, run_time;
  double run_sec, usr_sec;
  long clktck;
#endif

  init_parvars();

  if (MyProcID == DISP_PROC)
  { printf("SATSP is running ...\n");
  }

  sa_input(argc, argv);

#ifndef PARIX
  if ((clktck = sysconf(_SC_CLK_TCK)) < 0)
  { sprintf(Msg, "Main: sysconf error");
    critical_error(ERR_SYSCONF, Msg);
  }
#endif

  if (DisplayFlag && (MyProcID == DISP_PROC))
  { print_towns(stdout);
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

  if (MyProcID == DISP_PROC)
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

  Experiment = 1;

  bestOpt = MAXDOUBLE;
  worstOpt = MINDOUBLE;
  sumOpt = 0.0;

  do
  { sa_algorithm();

    sum_temp += Temp;
    sum_time += Time;

    bestqual = sa_get_global_optimum(OptLength);

    if (bestqual < bestOpt)
    { bestOpt = bestqual;
    }
    if (bestqual > worstOpt)
    { worstOpt = bestqual;
    }
    sumOpt += bestqual;

    if (MyProcID == DISP_PROC)
    {
#ifdef USE_CURSES
      if (DisplayFlag)
      { printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
      }
#endif
      sprintf(Msg, "Global Optimum for Experiment %d", Experiment);
      log_double(stdout, Msg, bestqual);
    }

    Experiment++;
  }
  while (Experiment <= TotalExperiments);

  synchronize();

  if (MyProcID == DISP_PROC)
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
    log_double(stdout, "Average End Temperature",
      (double) sum_temp / (double) TotalExperiments);
    log_double(stdout, "Average Simulation Time",
      (double) sum_time / (double) TotalExperiments);
  }

#ifdef USE_CURSES
  if (DisplayFlag && (MyProcID == DISP_PROC))
  { resetty();
    nocbreak();
    echo();
  }
#endif

  free_line();
  free_town();

  synchronize();
  end_parallel();

  Running = FALSE;

  return(ERR_NO);
}


/*** end of file ***/
