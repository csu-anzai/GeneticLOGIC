/* $Id: bit.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : bit.c                                                         */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <string.h>
#else
#include <memory.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "other.h"
#include "trace.h"
#include "bit.h"


/*****************************************************************************/
/* Allocate memory for a bit field                                           */
/*****************************************************************************/
BIT *alloc_bit(num)
  unsigned num;		/* size of bit field */
{ BIT *bit;

  trace("alloc_bit() entered");

  bit = (BIT *) emalloc((unsigned long) sizeof(BIT), TRUE);
  bit->field = (BITFIELD *) emalloc((unsigned long) num * sizeof(BITFIELD),
    TRUE);
  bit->len = num;

  trace("alloc_bit() completed");

  return(bit);
}


/*****************************************************************************/
/* Free memory of bit field                                                  */
/*****************************************************************************/
void free_bit(bit)
  BIT *bit;		/* pointer to bit field */
{
  trace("free_bit() entered");

  free(bit->field);
  free(bit);

  trace("free_bit() completed");
}


/*****************************************************************************/
/* Clear a bit field                                                         */
/*****************************************************************************/
void clear_bits(bit)
  BIT *bit;		/* pointer to bit field */
{
  trace("clear_bits() entered");

  memset(bit->field, 0, bit->len);

  trace("clear_bits() completed");
}


/*****************************************************************************/
/* Invert a bit field                                                        */
/*****************************************************************************/
void invert_bits(bit)
  BIT *bit;		/* pointer to bit field */
{ register unsigned i;

  trace("inverted_bits() entered");

  for (i = 0; i < bit->len; i++)
  { bit->field[i] ^= 1;
  }

  trace("inverted_bits() completed");
}


/*****************************************************************************/
/* Set a bit in a bit field                                                  */
/*****************************************************************************/
void set_bit(bit, b)
  BIT *bit;		/* pointer to bit field */
  unsigned b;		/* bit to set */
{
  trace("set_bit() entered");

  bit->field[b] = 1;

  trace("set_bit() completed");
}


/*****************************************************************************/
/* Get a bit in a bit field                                                  */
/*****************************************************************************/
BOOLEAN get_bit(bit, b)
  BIT *bit;		/* pointer to bit field */
  unsigned b;		/* bit to get */
{ BOOLEAN res;

  trace("get_bit() entered");

  res = (((int) bit->field[b]) == 1) ? TRUE : FALSE;

  trace("get_bit() completed");

  return(res);
}


/*****************************************************************************/
/* Set the next cleared bit in a bit field                                   */
/*****************************************************************************/
unsigned set_next_bit(bit, b)
  BIT *bit;		/* pointer to bit field */
  unsigned b;		/* next bit to set */
{ register unsigned i = 0, j = 0;
 
  trace("set_next_bit() entered");

  do
  { if (! get_bit(bit, i++))
    { j++;
    }
  }
  while (j < b);
  set_bit(bit, --i);

  trace("set_next_bit() completed");

  return(i);
}


/*****************************************************************************/
/* Get the next 0-bit in a bit field                                         */
/*****************************************************************************/
unsigned get_next_0_bit(bit, b)
  BIT *bit;		/* pointer to bit field */
  unsigned b;		/* next 0-bit to get */
{ register unsigned i = 0, j = 0;
 
  trace("get_next_0_bit() entered");

  do
  { if (! get_bit(bit, i++))
    { j++;
    }
  }
  while (j < b);

  trace("get_next_0_bit() completed");

  return(--i);
}


/*****************************************************************************/
/* Get the next 1-bit in a bit field                                         */
/*****************************************************************************/
unsigned get_next_1_bit(bit, b)
  BIT *bit;		/* pointer to bit field */
  unsigned b;		/* next 1-bit to get */
{ register unsigned i = 0, j = 0;
 
  trace("get_next_1_bit() entered");

  do
  { if (get_bit(bit, i++))
    { j++;
    }
  }
  while (j < b);

  trace("get_next_1_bit() completed");

  return(--i);
}


/*** end of file ***/
