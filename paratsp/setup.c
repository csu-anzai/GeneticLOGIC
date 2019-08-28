/* $Id: setup.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : setup.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "fopen.h"
#include "getopt.h"
#include "interfac.h"


#ifdef PARIX_HOST

#define SETUP_USAGE	"ParaTSP 1.0\t\t\t(c) 1994 by Holger Totzke\n\n\
`setup' provides an interactive way for parameterization of a run\n\
 of `paratsp'\n\
Options:\n\
\t-p nproc\t\t# Number of processors\n\
\t-h,-?   \t\t# This help information\n\n"

#define	SETUP_OPTSTR	"p:h?"

#define SETUP_CMD	"run -ap%d paratsp.px -i %s"

#define DEFAULT_PARTITION	16

#else

#define SETUP_USAGE	"ParaTSP 1.0\t\t\t(c) 1994 by Holger Totzke\n\n\
`setup' provides an interactive way for parameterization of a run\n\
 of `paratsp'\n\
Options:\n\
\t-h,-?   \t\t# This help information\n\n"

#define	SETUP_OPTSTR	"h?"

#define SETUP_CMD	"/usr/bin/nice -19 paratsp -i %s &"
#define SETUP_CMD1	"paratsp -i %s"
#define SETUP_CMD2	"/usr/bin/nice -19 paratsp -i %s"

#define DEFAULT_PARTITION	1

#endif

#define QFILDEF		"GA-queue"
#define	DRUNQST		"Execute PARATSP ?                [%s] : "


/*****************************************************************************/
/* Setup for ParaTSP                                                         */
/*****************************************************************************/
int main(argc, argv)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
{ register int opt;
  FILE *fp;
  char str[MAX_STR], cmd[MAX_STR], infile[MAX_STR], qufile[MAX_STR];

  NProcs = DEFAULT_PARTITION;

  while ((opt = get_options(argc, argv, SETUP_OPTSTR)) != EOF)
  { switch (opt)
    {
#ifdef PARIX_HOST
      case 'p':
        sscanf(my_optarg, "%d", &NProcs);
        break;
#endif
      case 'h':
      case '?':
        printf(SETUP_USAGE);
        exit(0);
        break;
      default:
        printf(SETUP_USAGE);
        exit(1);
        break;
    }
  }

  strcpy(qufile, QFILDEF);

  get_data(stdin);
  sprintf(infile, "in.%s", Suffix);

  if ((fp = file_open(infile, "w", FALSE)) != NULL)
  { write_data(fp);
    fclose(fp);
  }
  else
  { printf("Setup: can't open '%s' (aborted)\n", infile);
    exit(2);
  }

  write_data(stdout);
  printf(DRUNQST, "Yes");
  gets(str);
  
  if ((strlen(str) == 0) || is_yes(str))
  {
#ifdef PARIX_HOST
    sprintf(cmd, SETUP_CMD, NProcs, infile);
#else
    if (DisplayFlag)
    { sprintf(cmd, SETUP_CMD1, infile);
    }
    else
    { sprintf(cmd, SETUP_CMD, infile);
    }
#endif
    printf("ParaTSP command \"%s\" started\n", cmd);
    system(cmd);
    if (! DisplayFlag)
    { printf("ParaTSP command \"%s\" executed\n", cmd);
    }
  }
  else
  { if ((fp = file_open(qufile, "a", FALSE)) != NULL)
    {
#ifdef PARIX_HOST
      sprintf(cmd, SETUP_CMD, NProcs, infile);
#else
      sprintf(cmd, SETUP_CMD2, infile);
#endif
      fprintf(fp, cmd);
      fclose(fp);
    }
    else
    { printf("Setup: can't open '%s' (aborted)\n", qufile);
      exit(2);
    }
    printf("ParaTSP command \"%s\" queued to file '%s'\n", cmd, qufile);
  }

  return(0);
}


/*** end of file ***/
