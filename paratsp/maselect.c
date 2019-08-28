/* $Id: maselect.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : maselect.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "random.h"
#include "trace.h"
#include "maselect.h"


/*****************************************************************************/
/* Random individual mates selection                                         */
/*****************************************************************************/
int ms_random()
{ register int res, i = 0, j = 0;
  register unsigned rnd;

  trace("ms_random() entered");

  rnd = 1 + equal_unsigned_random(NumMates--);

  do
  { if (MatePool[j++] != MAX_MATES)
    { i++;
    }
  }
  while (i < rnd);

  res = MatePool[--j];
  MatePool[j] = MAX_MATES;

  trace("ms_random() completed");

  return(res);
}


/*****************************************************************************/
/* Next individual mates selection                                           */
/*****************************************************************************/
int ms_position()
{ register int res, i = 0;
  BOOLEAN search = TRUE;

  trace("ms_position() entered");

  do
  { if (MatePool[i++] != MAX_MATES)
    { search = FALSE;
    }
  }
  while (search);

  res = MatePool[--i];
  MatePool[i] = MAX_MATES;

  trace("ms_position() completed");

  return(res);
}


/*****************************************************************************/
/* Mates selection                                                           */
/*****************************************************************************/
int mate_select()
{ register int res;

  trace("mate_select() entered");

  switch (MateSelect)
  { case MAS_RND:
      res = ms_random();
      break;
    case MAS_POS:
      res = ms_position();
      break;
    default:
      res = 0;
      break;
  }

  trace("mate_select() completed");

  return(res);
}


/*** end of file ***/
