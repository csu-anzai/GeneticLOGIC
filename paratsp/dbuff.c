/* $Id: dbuff.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : dbuff.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "other.h"
#include "trace.h"
#include "dbuff.h"


/*****************************************************************************/
/* Create a buffer                                                           */
/*****************************************************************************/
BUFFER *create_buffer(file, num)
  char *file;		/* file name */
  unsigned num;		/* size of buffer */
{ register int i;
  BUFFER *buf = NULL;

  trace("create_buffer() entered");

  if (OutFlag)
  { buf = (BUFFER *) emalloc((unsigned long) sizeof(BUFFER), TRUE);
    buf->fmt = (char **) emalloc((unsigned long) (num + 1) * sizeof(char *),
      TRUE);

    buf->size = 0;
    buf->cols = num;
    strcpy(buf->file, file);

    for (i = 0; i <= num; i++)
    { buf->fmt[i] = (char *) emalloc((unsigned long) MAX_STR, TRUE);
      strcpy(buf->fmt[i], "% 1.2f ");
    }

    for (i = 0; i < ROWS; i++)
    { buf->val[i] = (double *) emalloc((unsigned long) (num + 1) *
        sizeof(double), TRUE);
    }
  }

  trace("create_buffer() completed");

  return(buf);
}


/*****************************************************************************/
/* Delete a buffer                                                           */
/*****************************************************************************/
void delete_buffer(buf)
  BUFFER *buf;		/* pointer to buffer */
{ register int i;

  trace("delete_buffer() entered");

  if (OutFlag)
  { for (i = 0; i < ROWS; i++)
    { free(buf->val[i]);
    }

    for (i = 0; i < buf->cols; i++)
    { free(buf->fmt[i]);
    }

    free(buf->fmt);
    free(buf);
  }

  trace("delete_buffer() completed");
}


/*****************************************************************************/
/* Change the print format of buffer                                         */
/*****************************************************************************/
void change_fmt(buf, idx, fmt)
  BUFFER *buf;		/* pointer to buffer */
  int idx;		/* index of format */
  char *fmt;		/* print format */
{
  trace("change_fmt() entered");

  if (OutFlag)
  { strcpy(buf->fmt[idx], fmt);
  }

  trace("change_fmt() completed");
}


/*****************************************************************************/
/* Change the file name for a buffer                                         */
/*****************************************************************************/
void change_buffer(buf, file)
  BUFFER *buf;		/* pointer to buffer */
  char *file;		/* file name */
{
  trace("change_buffer() entered");

  if (OutFlag)
  { strcpy(buf->file, file);
  }

  trace("change_buffer() completed");
}


/*****************************************************************************/
/* Enter values in a buffer and flush it if full                             */
/*****************************************************************************/
void enter_buffer(val, buf, n, mode)
  double *val;		/* pointer to values */
  BUFFER *buf;		/* pointer to buffer */
  int n;		/* number of values */
  int mode;		/* write mode */
{
  trace("enter_buffer() entered");

  if (OutFlag)
  { store_buffer(val, buf, n);
    if (full_buffer(buf))
    { flush_buffer(buf, n, mode);
    }
  }

  trace("enter_buffer() completed");
}


/*****************************************************************************/
/* Check if buffer full                                                      */
/*****************************************************************************/
BOOLEAN full_buffer(buf)
  BUFFER *buf;		/* pointer to buffer */
{ BOOLEAN res = FALSE;

  trace("full_buffer() entered");

  if (OutFlag)
  { res = (buf->size >= ROWS - 1) ? TRUE : FALSE;
  }

  trace("full_buffer() completed");

  return(res);
}


/*****************************************************************************/
/* Store values in a buffer                                                  */
/*****************************************************************************/
void store_buffer(val, buf, n)
  double *val;		/* pointer to values */
  BUFFER *buf;		/* pointer to buffer */
  int n;		/* number of values */
{ register int i, idx;

  trace("store_buffer() entered");

  if (OutFlag)
  { for (i = 1, idx = buf->size; i <= n; i++)
    { buf->val[idx][i] = val[i - 1];
    }
    buf->val[idx][0] = Generation[P];
    buf->size++;
  }

  trace("store_buffer() completed");
}


/*****************************************************************************/
/* Flush buffer in file                                                      */
/*****************************************************************************/
void flush_buffer(buf, n, mode)
  BUFFER *buf;		/* pointer to buffer */
  int n;		/* number of values */
  int mode;		/* write mode */
{ register int i, j;
  FILE *fp;

  trace("flush_buffer() entered");

  if (OutFlag)
  { if ((fp = file_open(buf->file, "a", TRUE)) != NULL)
    { if (mode == SGLLINE)
      { for (i = 0; i < buf->size; i++)
        { for (j = 0; j <= n; j++)
          { fprintf(fp, buf->fmt[j], buf->val[i][j]);
          }
          fprintf(fp, "\n");
        }
      }
      else
      { for (i = 0; i < buf->size; i++)
        { for (j = 1; j <= n; j++)
          { fprintf(fp, buf->fmt[0], buf->val[i][0]);
            fprintf(fp, buf->fmt[j], buf->val[i][j]);
            fprintf(fp, "\n");
          }
        }
      }
      buf->size = 0;
      fclose(fp);
    }
    else
    { sprintf(Msg, "FlushBuf: can't open '%s'", get_fname(buf->file));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }

  trace("flush_buffer() completed");
}


/*****************************************************************************/
/* Write performance buffer                                                  */
/*****************************************************************************/
void write_pfmbuf(i, mode)
  int *i;		/* pointer to values */
  int mode;		/* write mode */
{ double hlp[DATACOL];

  trace("write_pfmbuf() entered");

  if (OutFlag)
  { *i = 0;
    change_fmt(PfmBuf[P], *i, "%4.0f  ");
    hlp[(*i)++] = Trials[P];
    change_fmt(PfmBuf[P], *i, "%6.0f  ");
    hlp[(*i)++] = Lost[P];
    change_fmt(PfmBuf[P], *i, "%4.0f  ");
    hlp[(*i)++] = Conv[P];
    change_fmt(PfmBuf[P], *i, "%4.0f  ");
    hlp[(*i)++] = Online[P];
    change_fmt(PfmBuf[P], *i, "%9.2f  ");
    hlp[(*i)++] = Offline[P];
    change_fmt(PfmBuf[P], *i, "%9.2f  ");
    hlp[(*i)++] = BestCurrPerf[P];
    change_fmt(PfmBuf[P], *i, "%9.2f  ");
    hlp[(*i)++] = AvgCurrPerf[P];
    change_fmt(PfmBuf[P], *i, "%9.2f  ");
    hlp[(*i)++] = WorstCurrPerf[P];
    change_fmt(PfmBuf[P], *i, "%9.2f ");

    enter_buffer(hlp, PfmBuf[P], *i, mode);
  }

  trace("write_pfmbuf() completed");
}


/*** end of file ***/
