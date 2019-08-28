/* $Id: logit.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : logit.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "global.h"
#include "trace.h"
#include "logit.h"


/*****************************************************************************/
/* Write string in logfile                                                   */
/*****************************************************************************/
void log_string(fp, s, val)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  char *val;		/* string */
{ register int i, k;

  trace("log_string() entered");

  k = LOGLEN - strlen(s);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, " %s\n", val);

  trace("log_string() completed");
}


/*****************************************************************************/
/* Write integer value in logfile                                            */
/*****************************************************************************/
void log_int(fp, s, val)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  int val;		/* integer value */
{ register int i, k;

  trace("log_int() entered");

  k = LOGLEN - strlen(s);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, "% d\n", val);

  trace("log_int() completed");
}


/*****************************************************************************/
/* Write double value in logfile                                             */
/*****************************************************************************/
void log_double(fp, s, val)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  double val;		/* double value */
{ register int i, k;

  trace("log_double() entered");

  k = LOGLEN - strlen(s);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, " %-10.3f\n", val);

  trace("log_double() completed");
}


/*****************************************************************************/
/* Write string in logfile with number                                       */
/*****************************************************************************/
void log_num_string(fp, s, val, proc)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  char *val;		/* string */
  BOOLEAN proc;		/* processor number flag */
{ register int i, k;
  char p[MAX_STR];

  trace("log_num_string() entered");

  if (proc)
  { sprintf(p, "Proc[#%d] ", MyProcID);
  }
  else
  { sprintf(p, "Pop[#%d] ", P * NProcs + MyProcID + 1);
  }

  k = LOGLEN - strlen(s) - strlen(p);
  fprintf(fp, p);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, " %s\n", val);

  trace("log_num_string() completed");
}


/*****************************************************************************/
/* Write integer value in logfile with number                                */
/*****************************************************************************/
void log_num_int(fp, s, val, proc)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  int val;		/* integer value */
  BOOLEAN proc;		/* processor number flag */
{ register int i, k;
  char p[MAX_STR];

  trace("log_num_int() entered");

  if (proc)
  { sprintf(p, "Proc[#%d] ", MyProcID);
  }
  else
  { sprintf(p, "Pop[#%d] ", P * NProcs + MyProcID + 1);
  }

  k = LOGLEN - strlen(s) - strlen(p);
  fprintf(fp, p);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, "% d\n", val);

  trace("log_num_int() completed");
}


/*****************************************************************************/
/* Write double value in logfile with number                                 */
/*****************************************************************************/
void log_num_double(fp, s, val, proc)
  FILE *fp;		/* file pointer */
  char *s;		/* description */
  double val;		/* double value */
  BOOLEAN proc;		/* processor number flag */
{ register int i, k;
  char p[MAX_STR];

  trace("log_num_double() entered");

  if (proc)
  { sprintf(p, "Proc[#%d] ", MyProcID);
  }
  else
  { sprintf(p, "Pop[#%d] ", P * NProcs + MyProcID + 1);
  }

  k = LOGLEN - strlen(s) - strlen(p);
  fprintf(fp, p);
  fprintf(fp, s);
  for (i = 0; i < k; i++)
  { fprintf(fp, " ");
  }
  fprintf(fp, ": ");
  fprintf(fp, " %-10.3f\n", val);

  trace("log_num_double() completed");
}


/*** end of file ***/
