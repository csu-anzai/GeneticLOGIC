
/*
 *  file:	prob-select.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	choose a new population
 *
 *  modified:	10 sep 90: include ranking option, handle max/min option
 */

#include "Pextern.h"

PProb_Select()
{
  int sample[Popsize];	/* pointers to Selected structures	*/
  int:physical parallel_sample;
  int:physical position;
  int rank[Popsize];
  double old_perf[Popsize];
  double perf;		/* next best perf (for ranking)		*/
  int sum,total;             /* control for selection loop           */
  int best;		/* index of next best structure		*/
  register int i;		/* loop control				*/
  register int j;		/* loop control				*/
  register int temp;	/* used for swapping pointers		*/
  
  Trace("Select entered");
  Dtrace("select");
  Time(0,"");
  
  with(physical) {

    read_from_pvar(old_perf,ParallelOld.Perf);
    total = 0;

    if (Rankflag)
      {
	/* Assign each structure its rank within the population. */
	/* rank = Popsize for best, rank = 1 for worst		*/
	/* Use the Needs_evaluation field to store the rank	*/
	
	/* clear the rank fields */
	for (i=0; i<Popsize; i++)
	  rank[i] = 0;
		
	for (i=0; i<Popsize; i++)
	  {
	    /* find the ith best structure */
	    best = -1;
	    perf = 0.0;
	    for (j=0; j<Popsize; j++)
	      {
		if (rank[j] == 0 &&
		    (best == -1 || BETTER(old_perf[j],perf)))
		  {
		    perf = old_perf[j];
		    best = j;
		  }
	      }
	    /* mark best structure with its rank */
	    rank[best] = Popsize - i;
	  }
	total = ((Popsize + 1) * Popsize)/2;
      }
    else
      {
	if (Maxflag)
	  total += ((ParallelOld.Perf - Best) * 100);
	else
	  total += ((Worst - ParallelOld.Perf) * 100);
      }
    

    /* Probabilistic Selection Algorithm */
    position = PRandint(0,total);
    parallel_sample = 0;
    
    sum = 0;
    
    for(i=0;i<Popsize;i++) {
      if (Rankflag)
	sum += rank[i];
      else
	if (Maxflag)
	  sum += ((old_perf[i] - Best) * 100);
	else
	  sum += ((Worst - old_perf[i]) * 100);
      where (!parallel_sample)
	where (position < sum) {
	  parallel_sample = i+1;
	}
    }
    
    parallel_sample--;

    read_from_pvar(sample,parallel_sample);

    if (Gapsize<1.0)		/* Generation Gap */ 
      Gap(sample);
        
    /* randomly shuffle pointers to new structures */
    for (i=0; i<Popsize; i++)
      {
	j = Randint(i,Popsize-1);
	temp = sample[j];
	sample[j] = sample[i];
	sample[i] = temp;
      }
    
    /* finally, form the new population */
    parallel_sample = write_to_pvar(sample);
    for(j=0;j<Bytes;j++)
      ParallelNew.Gene[j] = [parallel_sample]ParallelOld.Gene[j];
    ParallelNew.Perf = [parallel_sample]ParallelOld.Perf;
  }
  
  Time(1,"Select");
  Trace("Select completed");
}

/*** end of file ***/















