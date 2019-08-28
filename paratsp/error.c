/* $Id: error.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : error.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <sys/sys_rpc.h>
#include <sys/logerror.h>
#endif

#ifdef USE_CURSES
#include <curses.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include "define.h"
#include "global.h"
#include "fopen.h"
#include "trace.h"
#include "error.h"


/*****************************************************************************/
/* Critical error function                                                   */
/*****************************************************************************/
void critical_error(err_num, s)
  int err_num;		/* error number */
  char *s;		/* error message */
{ FILE *fp;
  time_t clock_time;

  trace("critical_error() entered");

  if (OutFlag && (P != NO_LOGFILE) &&
     ((fp = file_open(AllLogFile, "a", FALSE)) != NULL))
  { fprintf(fp, "Error:       %s\n", s);
    fprintf(fp, "Exit-Code:   %d\n", err_num);
    time(&clock_time);
    fprintf(fp, "Terminating! %s\n", ctime(&clock_time));
    fclose(fp);
    if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
    { fprintf(fp, "Error:       %s\n", s);
      fprintf(fp, "Exit-Code:   %d\n", err_num);
      time(&clock_time);
      fprintf(fp, "Terminating! %s\n", ctime(&clock_time));
      fclose(fp);
    }
#ifndef PARIX
    if (DisplayFlag)
    { fprintf(stderr, "Error #%d: %s\n", err_num, s);
    }
#endif
  }
#ifndef PARIX
  else
  { fprintf(stderr, "Error #%d: %s\n", err_num, s);
  }
#endif

  if (DisplayFlag && (MyProcID == DISP_PROC) && (! Report) && Running)
  {
#ifdef USE_CURSES
    resetty();
    nocbreak();
    echo();
#endif
  }

#ifdef PARIX
  printf("Proc.#%d --> Error #%d: %s\n", MyProcID, err_num, s);
#endif

  trace("critical_error() completed");

#ifdef PARIX
  AbortServer(err_num);
#else
  exit(err_num);
#endif
}


/*** end of file ***/
