/* $Id: readfile.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : readfile.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "other.h"
#include "readpars.h"
#include "trace.h"
#include "readfile.h"


extern FILE *yyin;
extern yyparse();

#ifdef USE_PROTO
extern int read_line(char *s);
#else
extern int read_line();
#endif


static int file_type = TYP_UNKNOWN;	/* type of TSP input file */


/*****************************************************************************/
/* Allocate memory for town coordinates                                      */
/*****************************************************************************/
void alloc_town(n)
  TOUR n;		/* number of towns */
{
  trace("alloc_town() entered");

  Town = (TOWN *) emalloc((unsigned long) n * sizeof(TOWN), TRUE);

  trace("alloc_town() completed");
}


/*****************************************************************************/
/* Allocate memory for tour                                                  */
/*****************************************************************************/
void alloc_tour(n)
  TOUR n;		/* length of tour */
{
  trace("alloc_tour() entered");

  InitTour = (TOUR *) emalloc((unsigned long) n * sizeof(TOUR), TRUE);

  trace("alloc_tour() completed");
}


/*****************************************************************************/
/* Free memory of town coordinates                                           */
/*****************************************************************************/
void free_town()
{ register TOUR i;

  trace("free_town() entered");

  if (Town != NULL)
  { for (i = 0; i < TownNum; i++)
    { free(Town[i].name);
    }
    free(Town);
  }

  trace("free_town() completed");
}


/*****************************************************************************/
/* Free memory of tour                                                       */
/*****************************************************************************/
void free_tour()
{
  trace("free_tour() entered");

  if (InitTour != NULL)
  { free(InitTour);
  }

  trace("free_tour() completed");
}


/*****************************************************************************/
/* Read town coordinates from tsp-file                                       */
/*****************************************************************************/
void read_nodes()
{ register int len, fend;
  register TOUR i = 0;
  BOOLEAN all_towns = FALSE;
  TOWN_COORD x, y, z;
  long k;
  char s[MAX_ZLEN], name[MAX_ZLEN];

  trace("read_nodes() entered");

  if (file_type != FDat.typ)
  { sprintf(Msg, "ReadNodes: TSP file '%s' has invalid type (%d)",
      ParserFile, FDat.typ);
    critical_error(ERR_FILE_TYPE, Msg);
  }
  if ((FDat.dim > MAX_TOWN) || (FDat.dim < 2))
  { sprintf(Msg, "ReadNodes: TSP file '%s' has invalid dimension (%d)",
      ParserFile, FDat.dim);
    critical_error(ERR_FILE_DIMENSION, Msg);
  }

  TownNum = FDat.dim;
  alloc_town(TownNum);

  name[0] = '\0';
  z = 0;

  do
  { fend = read_line(s);
    if (s[0] != ':')
    { switch (FDat.ewt) 
      { case EWT_HOTO:
          if (sscanf(s, "%ld %f %f %s\n", &k, &x, &y, name) != 4)
          { sprintf(Msg, "ReadNodes: Syntax error in file '%s', line %ld",
              ParserFile, ParserLine - 1);
            critical_error(ERR_FILE_SYNTAX, Msg);
          }
          break;
        case EWT_EUC_2D:
        case EWT_MAN_2D:
        case EWT_MAX_2D:
        case EWT_GEO:
        case EWT_ATT:
          if (sscanf(s, "%ld %f %f\n", &k, &x, &y) != 3)
          { sprintf(Msg, "ReadNodes: Syntax error in file '%s', line %ld",
              ParserFile, ParserLine - 1);
            critical_error(ERR_FILE_SYNTAX, Msg);
          }
          break;
        case EWT_EUC_3D:
        case EWT_MAN_3D:
        case EWT_MAX_3D:
          if (sscanf(s, "%ld %f %f %f\n", &k, &x, &y, &z) != 4)
          { sprintf(Msg, "ReadNodes: Syntax error in file '%s', line %ld",
              ParserFile, ParserLine - 1);
            critical_error(ERR_FILE_SYNTAX, Msg);
          }
          break;
        default:
          return;
          break;
      }

      if (k != ++i)
      { sprintf(Msg, "ReadNodes: Town index (%ld) invalid, %d expected \
([%ld] '%s')", k, i, ParserLine - 1, ParserFile);
        critical_error(ERR_FILE_INDEX, Msg);
      }
      if (k > TownNum)
      { sprintf(Msg, "ReadNodes: Town index (%ld) invalid, > dim. %d \
([%ld] '%s')", k, TownNum, ParserLine - 1, ParserFile);
        critical_error(ERR_FILE_DIMINDEX, Msg);
      }

      Town[i - 1].x = x;
      Town[i - 1].y = y;
      Town[i - 1].z = z;
      len = strlen(name) + 1;
      Town[i - 1].name = (char *) emalloc((unsigned long) len, TRUE);
      strcpy(Town[i - 1].name, name);
      if (i == FDat.dim)
      { all_towns = TRUE;
      }
    }
  }
  while ((! all_towns) && (fend == 0));

  trace("read_nodes() completed");
}


/*****************************************************************************/
/* Read tsp-file                                                             */
/*****************************************************************************/
void read_TSP_file(file_name)
  char *file_name;	/* file name */
{ FILE *fp;

  trace("read_TSP_file() entered");

  fp = file_open(file_name, "r", FALSE);
  if (fp == NULL)
  { sprintf(Msg, "ReadTSP: can't open '%s'", file_name);
    critical_error(ERR_FILE_OPEN, Msg);
  }

  yyin = fp;
  file_type = TYP_TSP;
  ParserFile = file_name;
  ParserLine = 1;

  yyparse();

  fclose(fp);

  trace("read_TSP_file() completed");
}


/*****************************************************************************/
/* Read tour from tour-file                                                  */
/*****************************************************************************/
void read_tour()
{ register int fend, x;
  register TOUR i = 0;
  BOOLEAN all_towns = FALSE;
  long k;
  char s[MAX_ZLEN];

  trace("read_tour() entered");

  if (file_type != FDat.typ)
  { sprintf(Msg, "ReadTour: TOUR file '%s' has invalid type (%d)",
      ParserFile, FDat.typ);
    critical_error(ERR_FILE_TYPE, Msg);
  }
  if ((FDat.dim > MAX_TOWN) || (FDat.dim < 2))
  { sprintf(Msg, "ReadNodes: TOUR file '%s' has invalid dimension (%d)",
      ParserFile, FDat.dim);
    critical_error(ERR_FILE_DIMENSION, Msg);
  }

  InitTourLen = FDat.dim;
  alloc_tour(InitTourLen);
  
  do
  { fend = read_line(s);
    if (sscanf(s, "%ld\n", &k) != 1)
    { sprintf(Msg, "ReadTour: Syntax error in file '%s', line %ld",
        ParserFile, ParserLine - 1);
      critical_error(ERR_FILE_SYNTAX, Msg);
    }

    if ((k < 1) || (k > InitTourLen))
    { sprintf(Msg, "ReadTour: Town index (%ld) invalid ([%ld] '%s')", k,
        ParserLine - 1, ParserFile);
      critical_error(ERR_FILE_INDEX, Msg);
    }

    InitTour[i++] = k - 1;
    if (i == InitTourLen)
    { all_towns = TRUE;
    }
  }
  while ((! all_towns) && (fend == 0));

  fend = read_line(s);
  x = sscanf(s, "%ld\n", &k);
  if ((x != 1) || (k != -1))
  { sprintf(Msg, "ReadTour: Syntax error in file '%s', line %ld",
      ParserFile, ParserLine - 1);
    critical_error(ERR_FILE_SYNTAX, Msg);
  }

  trace("read_tour() completed");
}


/*****************************************************************************/
/* Read tour-file                                                            */
/*****************************************************************************/
void read_TOUR_file(file_name)
  char *file_name;	/* file name */
{ FILE *fp;

  trace("read_TOUR_file() entered");

  fp = file_open(file_name, "r", FALSE);
  if (fp == NULL)
  { sprintf(Msg, "ReadTOUR: can't open '%s'", file_name);
    critical_error(ERR_FILE_OPEN, Msg);
  }

  yyin = fp;
  file_type = TYP_TOUR;
  ParserFile = file_name;
  ParserLine = 1;

  yyparse();

  fclose(fp);

  trace("read_TOUR_file() completed");
}


/*****************************************************************************/
/* Print town coordinates                                                    */
/*****************************************************************************/
void print_towns(fp)
  FILE *fp;		/* file pointer */
{ register TOUR i;

  trace("print_towns() entered");

  for (i = 0; i < TownNum; i++)
  { fprintf(fp, "%-5d %12.2f %12.2f   %s\n", i + 1, Town[i].x, Town[i].y,
      Town[i].name);
  }

  trace("print_towns() completed");
}


/*** end of file ***/
