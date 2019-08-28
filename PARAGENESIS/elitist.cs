
/*
 *  file:	elitist.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	The elitist policy stipulates that the best individual
 *		always survives into the new generation.  The elite
 *		individual is placed in the last position in New pop,
 *		and is not changed through crossover or mutation.
 *
 *  modified:	24 mar 86
 */

#include "Pextern.h"

PElitist()
{
  int curr_index;
  register int k;
  register int found;
  int:physical untested;
  bool:physical b;
  
  Trace("Elitist entered");
  Time(0,"");
  
  /* is any element in the current population		*/
  /* identical to the Best guy in the last Generation?	*/

  with(physical) {
    untested = 1;
    found = 0;

    b = (ParallelNew.Gene[0] == [Best_guy]ParallelOld.Gene[0]) &
        (ParallelNew.Gene[1] == [Best_guy]ParallelOld.Gene[1]);
    where (b)
      while ((+= untested) && (!found))
	where(untested) {
	  curr_index = (int)Index;
	  where(curr_index == Index) {
	    for (k=1, found=1; (k<Bytes) && (found); k++)
	      found = (int)(ParallelNew.Gene[k] == 
			    [Best_guy]ParallelOld.Gene[k]);
	    untested = 0;
	  }
	}
    
  
    if (!found)	/* elite one was not present */
      {
	/* replace last guy with the elite one */
	for (k=0; k<Bytes; k++)
	  [Popsize-1]ParallelNew.Gene[k] = [Best_guy]ParallelOld.Gene[k];
	[Popsize-1]ParallelNew.Perf = [Best_guy]ParallelOld.Perf;
	if (Traceflag)
	  {
	    printf("perf: %e\n", [Popsize-1]ParallelNew.Perf);
	  }
      }
  }
  Time(1,"Elitist");
  Trace("Elitist completed");
}


/*** end of file ***/
