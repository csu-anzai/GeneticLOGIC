/* $Id: getopt.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : getopt.c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "getopt.h"


#define OPT_CHAR '-'		/* option character */


int my_optind = 1;		/* option index */
int my_opterr = 1;		/* error message flag */
char *my_optarg = NULL;		/* pointer to option argument */
 
static char *actpos;		/* actual position */
static char str[200];		/* temporary string */


/*****************************************************************************/
/* Get command line options                                                  */
/*****************************************************************************/
int get_options(argc, argv, optstr)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
  char *optstr;		/* available options */
{ register unsigned char ch, c1;
  char *param;

  if (argc > my_optind)
  { if (actpos == NULL)
    { actpos = argv[my_optind];
      if ((actpos == NULL) || (*(actpos++) != OPT_CHAR))
      { goto optEOF;
      }
      if (*actpos == OPT_CHAR)
      {	my_optind++;
        goto optEOF;
      }
    }
    ch = *(actpos++);
    if (ch == 0)
    { my_optind++;
      goto optEOF;
    }
    /* param = index(optstr, ch); */
    param = strchr(optstr, ch);
    if ((ch == ':') || (ch == '#') || (param == NULL))
    { goto optError;
    }
    if (*(++param) == ':')
    { my_optind++;
      if (*actpos == 0)
      { if (argc <= my_optind)
        { goto optError;
        }
        actpos = argv[my_optind++];
      }
      my_optarg = actpos;
      actpos = NULL;
    }
    else
    { if (*param == '#')
      { my_optind++;
        if (*actpos == 0)
        { goto optError;
        }
        c1 = *(actpos++);
        if (*actpos == 0)
        { if (argc < my_optind)
          { goto optError;
          }
          actpos = argv[my_optind++];
          str[0] = c1;
          strcpy(&str[1], actpos);
          actpos = str;
        }
        else
        { actpos--;
        }
        my_optarg = actpos;
        actpos = NULL;
      }
      else
      { if (*actpos == 0)
        { my_optind++;
          actpos = NULL;
        }
        my_optarg = NULL;
      }
    }
    return(ch);
  }

optEOF:
  my_optarg = NULL;
  actpos = NULL;
  return(EOF);
 
optError:
  my_optarg = NULL;
  if (my_opterr)
  { fprintf(stderr, "Error: False option in command line\n");
  }
  return(ch);
}


/*** end of file ***/
