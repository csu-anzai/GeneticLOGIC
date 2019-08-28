/* $Id: fitscale.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : .c                                                      */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "trace.h"
#include "fitscale.h"


/*****************************************************************************/
/* Linear fitness scaling                                                    */
/*****************************************************************************/
double fi_linear(d)
  double d;		/* tour length */
{ double res;

  trace("fi_linear() entered");

  res = ((double) FitParA * d / FitParB) + ((double) FitParC / FitParD);

  trace("fi_linear() completed");

  return(res);
}


/*****************************************************************************/
/* Exponential fitness scaling                                               */
/*****************************************************************************/
double fi_expo(d)
  double d;		/* tour length */
{ double res;

  trace("fi_expo() entered");

  res = (double) FitParA / ((double) FitParB *
    pow(d, (double) FitParC / FitParD));

  trace("fi_expo() completed");

  return(res);
}


/*****************************************************************************/
/* Fitness scaling                                                           */
/*****************************************************************************/
double fitness_scale(quality)
  double quality;	/* tour length */
{ double res;

  trace("fitness_scale() entered");

  switch (FitnessScale)
  { case FIT_NOP:
      res = quality;
      break;
    case FIT_LIN:
      res = fi_linear(quality);
      break;
    case FIT_EXP:
      res = fi_expo(quality);
      break;
    default:
      res = 0.0;
      break;
  }

  trace("fitness_scale() completed");

  return(res);
}


/*** end of file ***/
