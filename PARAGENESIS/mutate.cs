
/*
 *  file:	mutate.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	Perform mutation on the current population.
 *
 *	The global variable Mu_next indicates the position of
 *	next mutation, treating the entire population as a linear
 *	array of positions.  
 *
 *  modified:	7 feb 98
 *
 *		12 nov 86: pass length to Pack() and Unpack()
 */

#include "Pextern.h"


PMutate()
{
  register int i;		/* index of the Mutated structure */
  char:physical k;	 /* a random allele */
  bool:physical b,c;
  
  Trace("Mutate entered");
  Dtrace("mutation");
  Time(0,"");
    
  if (M_rate > 0.0)
    {
      with(physical) {

	for(i=0;i<Length;i++) {
	  b = (PRand() <= M_rate);
	  where(b) {
	    
	    /* choose a random allele */
	    where(PRandint(0,1))
	      k = '1';
	    else
	      k = '0';
	    
	    c = (k != PBitstring[i]);
	    where(c)  /* it's an effective mutation */
	      {
		PBitstring[i] = k;
	      }
	  }
	}
	/*  Don't repack, instead leave everything unpacked for crossover */
	/*  the repack is done in crossover */
      }
    }
  
  Time(1,"Mutate");
  Trace("Mutate completed");
}


/** end of file **/
