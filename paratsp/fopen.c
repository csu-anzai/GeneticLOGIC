/* $Id: fopen.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : fopen.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <sys/time.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include <unistd.h>
#include "define.h"
#include "global.h"
#include "trace.h"
#include "fopen.h"


#define SLEEP_TIME	600		/* Sleep time if error */


/*****************************************************************************/
/* Open a file                                                               */
/*****************************************************************************/
FILE *file_open(fname, mode, ext)
  char *fname;		/* file name */
  char *mode;		/* file mode */
  BOOLEAN ext;		/* file with suffix */
{ register int Dly = 1;
  FILE *fp;
  char s[MAX_STR];

  trace("file_open() entered");

  if (ext)
  { strcpy(s, get_fname(fname));
  }
  else
  { strcpy(s, fname);
  }

  while (((fp = fopen(s, mode)) == NULL) && (! DisplayFlag) &&
	 (Dly < SLEEP_TIME))
  { sprintf(Msg, "%s: fOpen(%s, %s) error (errno = %d)",
      LOGO, s, mode, errno);
    sys_perror(Msg);
#ifdef PARIX
    TimeWaitLow(TimeNowLow() + CLK_TCK_LOW * Dly);
#else
    sleep(Dly);
#endif
    Dly <<= 1;
  }

  trace("file_open() completed");

  return(fp);
}


/*** end of file ***/
