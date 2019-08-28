/* $Id: report.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : report.c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h> 
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "fopen.h"
#include "getopt.h"
#include "interfac.h"


#define REPROWS 	5000		/* maximal rows */
#define COLUMNS 	20		/* maximal columns */
#define BUFSZ		1024		/* size of buffer */
#define FILESEP		"."		/* file seperator */

#define REPORT_USAGE	"ParaTSP 1.0\t\t\t(c) 1994 by Holger Totzke\n\n\
`report' is usually not explicitly called, but invoked automatically\n\
 by `paratsp'\n\
Options:\n\
\t-i suffx\t\t# File suffix of the infile for later report\n\
\t        \t\t# generation\n\
\t-h,-?   \t\t# This help information\n\
\n"

#define	REPORT_OPTSTR	"i:h?"		/* report options */


double average[REPROWS][COLUMNS];	/* average values */
double variance[REPROWS][COLUMNS];	/* variance values */
double line[COLUMNS];			/* input line values */
 
 
/*****************************************************************************/
/* Get source file                                                           */
/*****************************************************************************/
int src_file(file, ext, sfx)
  char *file, *ext, *sfx;
{ struct stat sbuf;
  char path[MAX_STR];

  strcpy(path, ext);
  strcat(path, FILESEP);
  strcat(path, sfx);

  if (stat(path, &sbuf) == 0)
  { if (S_ISREG(sbuf.st_mode))
    { strcpy(file, path);
      return(0);
    }
    else
    { printf("SrcFile: file '%s' is not a regular one", path);
      return(-1);
    }
  }

  strcpy(path, sfx);
  strcat(path, DIRSEP);
  strcat(path, ext);
  strcat(path, FILESEP);
  strcat(path, sfx);

  if (stat(path, &sbuf) < 0)
  { switch (errno)
    { case ENOENT:
        printf("SrcFile: file %s does not exist\n", path);
        return(-1);
        break;
      default:
        break;
    }
  }
  else
  { strcpy(file, path);
    return(0);
  }

  return(-1);
}


/*****************************************************************************/
/* Report for ParaTSP                                                        */
/*****************************************************************************/
int main(argc, argv)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
{ register int i, j, row, col;
  register int opt, expn, columns, lines, eof, cutoff, colflag;
  FILE *fp;
  char outfile[MAX_STR], infile[MAX_STR], avgfile[MAX_STR], varfile[MAX_STR];
  char str[BUFSZ], *s;
  double tmp, oldgens;
 
  Report = TRUE;

  NProcs = 1;
  PopNum = 0;
  P = 0;
 
  sprintf(LogFile, "%s", "log");
  sprintf(outfile, "%s", "out");
  sprintf(avgfile, "%s", "rep");
  sprintf(varfile, "%s", "var");
  sprintf(Extension, "%s%s%s", ".", DIRSEP, "pop");
  strcpy(Suffix, "in");

  while ((opt = get_options(argc, argv, REPORT_OPTSTR)) != EOF)
  { switch (opt)
    { case 'i':
        sscanf(my_optarg, "in.%s", Suffix);
        if (src_file(infile, "in", Suffix) < 0)
        { exit(ERR_FILE_NAME);
        }
        sprintf(Extension, "%s%s%s", Suffix, DIRSEP, "pop");
        break;
      case 'h':
      case '?':
        printf(REPORT_USAGE);
        exit(ERR_HELP);
        break;
      default:
        printf(REPORT_USAGE);
        exit(ERR_OPTION);
        break;
    }
  }

  if ((fp = file_open(infile, "r", FALSE)) != NULL)
  { get_data(fp);
    fclose(fp);
  }
  else
  { sprintf(Msg, "Report: can't open '%s'", infile);
    critical_error(ERR_FILE_OPEN, Msg);
  }

  MyPopNum = PopNum;

  for (P = 0; P < MyPopNum; P++)
  { columns = 0;
    lines = 0;
    eof = 0;
    cutoff = 0;
    colflag = 0;
    oldgens = -1.0;

    for (row = 0; row < REPROWS; row++)
    { for (col = 0; col < COLUMNS; col++)
      { average[row][col] = 0.0;
        variance[row][col] = 0.0;
      }
    }

    if ((fp = file_open(outfile, "r", TRUE)) == NULL)
    { sprintf(Msg, "Report: can't open '%s'", get_fname(outfile));
      critical_error(ERR_FILE_OPEN, Msg);
    }
 
    if (fgets(str, BUFSZ, fp) == NULL)
    { sprintf(Msg, "Report: unexpected EOF on '%s'", get_fname(outfile));
      critical_error(ERR_UNEXPECTED_EOF, Msg);
    }
    else
    { i = 0;
      s = strtok(str, " ");
      do
      { sscanf(s, "%lf", &line[i]);
        s = strtok(NULL, " ");
        i++;
      }
      while ((s != NULL) && (i < COLUMNS)) ;
      columns = i - 1;
    }

    for (expn = 0; (! eof); expn++)
    { row = 0;
 
      while ((! eof) && (oldgens <= line[0]))
      { if (oldgens < line[0])
        { for (col = 0; col < COLUMNS; col++)
          { average[row][col] += line[col];
            variance[row][col] += line[col] * line[col];
          }
          row++;
        }
        oldgens = line[0];

        if (fgets(str, BUFSZ, fp) == NULL)
        { eof = 1;
        }
        else
        { i = 0;
          s = strtok(str, " ");
          do
          { sscanf(s, "%lf", &line[i]);
            s = strtok(NULL, " ");
            i++;
          }
          while ((s != NULL) && (i < COLUMNS)) ;
          if (i - 1 != columns)
          { colflag = 1;
          }
        }
      }
   
      oldgens = -1.0;

      if (expn == 0)
      { lines = row;
      }
      else
      { if (row < lines)
        { lines  = row;
          cutoff = 1;
        }
      }
    }

    fclose(fp);
 
    for (row = 0; row < REPROWS; row++)
    { for (col = 0; col < COLUMNS; col++)
      { tmp  = average[row][col] * average[row][col];
        tmp /= expn;
        variance[row][col] -= tmp;
        if (expn > 1)
        { variance[row][col] /= (expn - 1);
        }
        average[row][col] /= expn;
      }
    }

    if ((fp = file_open(avgfile, "w", TRUE)) != NULL)
    { fprintf(fp, "# MEAN\n");
      fprintf(fp, "# Gens Trials Lost  Conv    Online    ");
      fprintf(fp, "Offline      Best     Average    Worst");
      fprintf(fp, "\n");

      for (i = 0; i < lines; i++)
      { fprintf(fp, "%4.0f  %6.0f  ", average[i][0], average[i][1]);
        fprintf(fp, "%4.0f  %4.0f", average[i][2], average[i][3]);
        for (j = 4; j < columns; j++)
        { fprintf(fp, "  %9.2f", average[i][j]);
        }
        fprintf(fp, "\n");
      }

      fclose(fp);
    }
    else
    { sprintf(Msg, "Report: can't open '%s'", get_fname(avgfile));
      critical_error(ERR_FILE_OPEN, Msg);
    }

    if (expn > 1)
    { if ((fp = file_open(varfile, "w", TRUE)) != NULL)
      { fprintf(fp, "# VARIANCE\n");
        fprintf(fp, "# Gens Trials Lost  Conv");
        fprintf(fp, "    Online    ");
        fprintf(fp, "Offline      Best     Average    Worst");
        fprintf(fp, "\n");
        for (i = 0; i < lines; i++)
        { fprintf(fp, "%4.0f  %6.0f  ", average[i][0], variance[i][1]);
          fprintf(fp, "%4.0f  %4.0f", variance[i][2], variance[i][3]);
          for (j = 4; j < columns; j++)
          { fprintf(fp, "  %9.2f", variance[i][j]);
          }
          fprintf(fp, "\n");
        }
        fclose(fp);
      }
      else
      { sprintf(Msg, "Report: can't open '%s'", get_fname(varfile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }

    if ((fp = file_open(LogFile, "a", TRUE)) != NULL)
    { if (cutoff)
      { fprintf(fp, "\nNOTE: Some Experiments produced more");
        fprintf(fp, " data than others.\n");
        fprintf(fp, "Extra data was not Reported above.\n");
      }
      if (colflag)
      { fprintf(fp, "\nNOTE: Some data rows in the outfile ");
        fprintf(fp, "included more columns than others.\n");
      }
      if (expn < TotalExperiments)
      { fprintf(fp, "\nWARNING: Too little data for given ");
        fprintf(fp, "number of Experiments.\n");
      }
      if (expn > TotalExperiments)
      { fprintf(fp, "\nWARNING: Too much data for given ");
        fprintf(fp, "number of Experiments.\n");
      }
      fclose(fp);
    }
    else
    { sprintf(Msg, "Report: can't open '%s'", get_fname(LogFile));
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }

  return(ERR_NO);
}


/*** end of file ***/
