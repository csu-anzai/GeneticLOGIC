/* $Id: graphics.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : graphics.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */
/*                                                                           */
/* Publics :                                                                 */
/*                                                                           */
/* ========================================================================= */

#ifndef PARIX
#include <malloc.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "fopen.h"
#include "trace.h"
#include "graphics.h"


static FILE *fp;	/* file pointer */


/*****************************************************************************/
/* Write the coordinates of towns in graph file                              */
/*****************************************************************************/
BOOLEAN send_nodes_graph()
{ register TOUR i;
  BOOLEAN ok = TRUE;

  trace("send_nodes_graph() entered");

  if (OutFlag)
  { if ((fp = file_open(GraphFile, "w", TRUE)) != NULL)
    { fprintf(fp, "%d\n", TownNum);
      for (i = 0; i < TownNum; i++)
      { fprintf(fp, "%f %f\n", Town[i].x, Town[i].y);
      }
      fclose(fp);
    }
    else
    { ok = FALSE;
    }
  }

  trace("send_nodes_graph() completed");

  return(ok);
}


/*****************************************************************************/
/* Write a tour in graph file                                                */
/*****************************************************************************/
BOOLEAN send_tour_graph(quality, o)
  double quality;	/* tour length */
  MYREP *o;		/* pointer to tour */
{ register TOUR i;
  BOOLEAN ok = TRUE;

  trace("send_tour_graph() entered");

  if (OutFlag)
  { if ((fp = file_open(GraphFile, "a", TRUE)) != NULL)
    { fprintf(fp, "%d %f\n", Generation[P], quality);
      for (i = 0; i < TownNum; i++)
      { fprintf(fp, "%d ", o->job[i]);
      }
      fprintf(fp, "\n");
      fclose(fp);
    }
    else
    { ok = FALSE;
    }
  }

  trace("send_tour_graph() completed");

  return(ok);
}


/*****************************************************************************/
/* Open graph file                                                           */
/*****************************************************************************/
void init_X_graph(file)
  char *file;		/* file name */
{
  if ((fp = file_open(file, "r", FALSE)) == NULL)
  { sprintf(Msg, "Error: can't open '%s'", file);
    fprintf(stderr, "%s\n", Msg);
    exit(1);
  }
}


/*****************************************************************************/
/* Close graph file                                                          */
/*****************************************************************************/
void close_X_graph()
{
  fclose(fp);
}


/*****************************************************************************/
/* Read the coordinates of towns from graph file                             */
/*****************************************************************************/
TOUR receive_nodes(coord)
  TOWN **coord;		/* pointer to coordinates */
{ register TOUR i;
  TOUR num;
  TOWN_COORD x, y;

  fscanf(fp, "%d\n", &num);

  *coord = (TOWN *) malloc((unsigned long) num * sizeof(TOWN));
  if (*coord == NULL)
  { fprintf(stderr, "Error: Out of Memory ... (No space available)");
    exit(2);
  }

  for (i = 0; i < num; i++)
  { fscanf(fp, "%f %f\n", &x, &y);

    (*coord)[i].x = x;
    (*coord)[i].y = y;
    (*coord)[i].z = (TOWN_COORD) 0;
    (*coord)[i].name = NULL;
  }

  return(num);
}


/*****************************************************************************/
/* Read a tour from graph file                                               */
/*****************************************************************************/
int receive_tour(num, gen, quality, o)
  TOUR num;		/* number of towns */
  int *gen;		/* pointer to generation */
  double *quality;	/* pointer to tour length */
  MYREP *o;		/* pointer to tour */
{ register TOUR i;

  if (feof(fp))
  { *gen = SEND_EOF;
  }
  else
  { fscanf(fp, "%d %lf\n", gen, quality);
  }

  if (*gen != SEND_EOF)
  { for (i = 0; i < num; i++)
    { fscanf(fp, "%d ", &o->job[i]);
    }
    fscanf(fp, "\n");
  }

  return(*gen);
}


/*** end of file ***/
