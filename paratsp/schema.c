/* $Id: schema.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : schema.c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "other.h"
#include "trace.h"
#include "schema.h"


/*****************************************************************************/
/* Allocate memory for schema tour                                           */
/*****************************************************************************/
void alloc_schema()
{
  trace("alloc_schema() entered");

  if (OutFlag)
  { SchemaTour = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR),
      TRUE);
  }

  trace("alloc_schema() completed");
}


/*****************************************************************************/
/* Free memory of schema tour                                                */
/*****************************************************************************/
void free_schema()
{
  trace("free_schema() entered");

  if (OutFlag && SchemaFlag)
  { free(SchemaTour);
  }

  trace("free_schema() completed");
}


/*****************************************************************************/
/* Write data in schema file                                                 */
/*****************************************************************************/
void schema()
{ static int firstflag = 1;
  static int firstcount = 1;
  static int lastcount = 1;
  register int i, j, count, ok;
  FILE *fp;
  TOUR tmp;
  double expected, perf;
 
  trace("schema() entered");
 
  if (OutFlag)
  { if (firstflag)
    { alloc_schema();

      if ((fp = file_open(SchemaFile, "r", FALSE)) == NULL)
      { sprintf(Msg, "Schema: can't open '%s'", SchemaFile);
        critical_error(ERR_FILE_OPEN, Msg);
      }

      for (i = 0; i < OrderLen; i++)
      { ok = fscanf(fp, "%d", &tmp);
        if (ok == 1)
        { if ((tmp >= 1) && (tmp <= OrderLen))
          { SchemaTour[i] = tmp;
          }
          else
          { if (tmp == 0)
            { SchemaTour[i] = 0;
            }
            else
            { sprintf(Msg, "Schema: town index (%d) in '%s' invalid", tmp,
                SchemaFile);
              critical_error(ERR_SCHEMA_INDEX, Msg);
            }
          }
        }
        else
        { sprintf(Msg, "Schema: town index in '%s' expected", SchemaFile);
          critical_error(ERR_SCHEMA_INDEX, Msg);
        }
      }

      fclose(fp);

      if ((fp = file_open(SchemaFile, "w", TRUE)) == NULL)
      { sprintf(Msg, "Schema: can't open '%s'", get_fname(SchemaFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }

      for (i = 0; i < OrderLen; i++)
      { if (SchemaTour[i] == 0)  
        { fprintf(fp, "# ");
        }
        else 
        { fprintf(fp, "%d ", SchemaTour[i]);
        }
      }

      fprintf(fp, "\n");
      fprintf(fp, " Gen  Count  Incr  Expct  ");
      fprintf(fp, "Schema Avg    Pop. Avg\n");

      fclose(fp);

      firstflag = 0;
    }
 
    expected = 0.0;
    perf = 0.0;
    count = 0;

    for (i = 0; i < PopSize; i++)
    { for (ok = 1, j = 0; ok && (j < OrderLen); j++)
      { ok = (SchemaTour[j] == 0) || (SchemaTour[j] == 1 +
          NewPop[P]->rep[i].myrep->job[j]);
      }
      if (ok)
      { count++;
        expected += (Worst[P] - NewPop[P]->rep[i].fitness) /
          (Worst[P] - AvgCurrPerf[P]);
        perf += NewPop[P]->rep[i].fitness;
      }
    }
 
    if (firstcount && count)
    { lastcount  = count;
      firstcount = 0;
    }

    if (count)
    { if ((fp = file_open(SchemaFile, "a", TRUE)) != NULL)
      { fprintf(fp, "%4d  %4d ", Generation[P], count);
        fprintf(fp, " %5.3f ", (count * 1.0) / lastcount);
        lastcount = count;
        fprintf(fp, " %5.3f ", expected / count);
        fprintf(fp, " %10.3e ", perf / count);
        fprintf(fp, " %10.3e ", AvgCurrPerf[P]);
        fprintf(fp, "\n");
        fclose(fp);
      }
      else
      { sprintf(Msg, "Schema: can't open '%s'", get_fname(SchemaFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
  }
 
  trace("schema() completed");
}
 

/*** end of file ***/
