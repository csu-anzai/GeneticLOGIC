/* $Id: init.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : init.c                                                        */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "define.h"
#include "global.h"
#include "brselect.h"
#include "dbuff.h"
#include "error.h"
#include "fopen.h"
#include "parallel.h"
#include "popinit.h"
#include "random.h"
#include "trace.h"
#include "init.h"


#define EPS_CROSS	0.001		/* epsilon value */


/*****************************************************************************/
/* Initialize generated files                                                */
/*****************************************************************************/
void init_gen_files(ext)
  char *ext;		/* file suffix */
{ register int i;
  char s[MAX_STR];

  trace("init_gen_files() entered");

  if (MyProcID == DISP_PROC)
  { if (crt_dir(ext))
    { sys_perror(LOGO);
      exit_parallel(ERR_MAKE_DIR);
    }
  }

  synchronize();

  sprintf(InFile,     "%s%s", "in.", ext);
  sprintf(Extension,  "%s%s%s", ext, DIRSEP, "pop");
  sprintf(AllLogFile, "%s%s%s", ext, DIRSEP, "logall");
  sprintf(OutFile,    "%s", "out");
  sprintf(CkptFile,   "%s", "ckpt");
  sprintf(MinFile,    "%s", "min");
  sprintf(LogFile,    "%s", "log");
  sprintf(SchemaFile, "%s", "schema");
  sprintf(PopFile,    "%s", "pop");
  sprintf(PgmFile,    "%s", "pgm");
  sprintf(ValFile[0], "%s", "vavg");
  sprintf(ValFile[1], "%s", "vvar");
  sprintf(ValFile[2], "%s", "vskw");
  sprintf(GraphFile,  "%s", "graph");
  strcpy(BestFile, MinFile);

  if (PopNum > 1)
  { for (i = 0; i < MyPopNum; i++)
    { sprintf(s, "%s%d", Extension, i * NProcs + MyProcID + 1);
      if (crt_dir(s))
      { sys_perror(LOGO);
        exit_parallel(ERR_MAKE_DIR);
      }
    }
  }

  trace("init_gen_files() completed");
}


/*****************************************************************************/
/* Create a directory                                                        */
/*****************************************************************************/
int crt_dir(dir)
  char *dir;		/* directory name */
{ register int rc, res = 0;
  struct stat sbuf;
  char *p;

  trace("crt_dir() entered");

  if (OutFlag)
  { while (stat(dir, &sbuf) < 0)
    { switch(errno)
      { case ENOENT:	/* dir does not exist */
        case ENOTDIR:	/* uncomplete path to dir */
          if ((p = strrchr(dir, '/')) != NULL)
          { *p= '\0';
            rc = crt_dir(dir);
            *p = '/';
            if (rc != 0)
            { res = -1;
              goto mark_ret;
            }
          }
          if (mkdir(dir, 0775) < 0)
          { sprintf(Msg, "Cannot mkdir(%s)", dir);
            sys_perror(Msg);
            res = -1;
            goto mark_ret;
          }
          break;
        default:		/* should not happen */
          sprintf(Msg, "Cannot stat(%s)", dir);
          sys_perror(Msg);
          res = -1;
          goto mark_ret;
          break;
      }
    }

    if (! S_ISDIR(sbuf.st_mode))
    { sprintf(Msg, "%s: not a directory", dir);
      sys_perror(Msg);
      res = -1;
      goto mark_ret;
    }
  }

mark_ret:
  trace("crt_dir() completed");

  return(res);
}


/*****************************************************************************/
/* Initialize files if number experiments > 1                                */
/*****************************************************************************/
void init_files()
{ register int i;
  char s[MAX_STR];

  trace("init_files() entered");

  if (TotalExperiments > 1)
  { sprintf(BestFile, "%s.%d", MinFile, Experiment);
    for (i = 0; i < BUFCNT; i++)
    { sprintf(s, "%s.%d", ValFile[i], Experiment);
      change_buffer(ValBuf[P][i], s);
    }
  }

  trace("init_files() completed");
}


/*****************************************************************************/
/* Initialize for a new experiment                                           */
/*****************************************************************************/
void initialize()
{ register int i;

  trace("initialize() entered");

  init_files();

  DoneAllFlag = FALSE;

  NumOff = num_offsprings();
  NumCross = (int) ((double) NumOff * CrossRate + EPS_CROSS);

  CurrDump[P] = 0;
  BestSize[P] = 0;
  Trials[P] = 0;
  Lost[P] = 0;
  Conv[P] = 0;
  Plateau[P] = 0;
  Spin[P] = 0;
  OnSum[P] = 0.0;
  OffSum[P] = 0.0;

  for (i = 0; i < WindowSize; i++)
  { Win[P][i] = 0.0;
  }

  InitSeed = Seed;
  init_rnd();
  Seed = equal_unsigned_random(MAX_SEED);

  init_pop();

  trace("initialize() completed");
}


/*** end of file ***/
