
/*
 *  file:	done.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	test experiment-termination conditions.
 *
 *  modified:	7 feb 86
 */

#include "extern.h"

int Done()
{
  
  if (Interflag)  /* user will indicate when to quit */
    return(0);
    
  return ((Trials >= Totaltrials)  || ( Lost >= Length)
	  || (Spin >= Maxspin) || (Conv >= (Popsize * 0.9)));
  
}

/* end of file */
