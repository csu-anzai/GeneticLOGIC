
/*
 *  file:	local-select.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	choose a new population
 *
 *  modified:	10 sep 90: include ranking option, handle max/min option
 */

#include "Pextern.h"

PLocal_Select()
{
  int sample[Popsize];	/* pointers to Selected structures	*/
  double:physical store[5];
  int:physical parallel_sample;
  int:physical position;
  int:physical source;
  int:physical rank[5];
  double:physical perf;		/* next best perf (for ranking)		*/
  int sum,total;             /* control for selection loop           */
  int best;		/* index of next best structure		*/
  register int i;		/* loop control				*/
  register int j;		/* loop control				*/
  register int temp;	/* used for swapping pointers		*/
  
  Trace("Local Select entered");
  Dtrace("select");
  Time(0,"");
  
  with(physical) {

    total = 0;

    store[0] = ParallelOld.Perf;
    to_torus_dim(&store[1],ParallelOld.Perf,0,128);
    where (!(Index % 128))
      to_torus_dim(&store[2],ParallelOld.Perf,0,127);
    else
      to_torus_dim(&store[2],ParallelOld.Perf,0,-1);
    to_torus_dim(&store[3],ParallelOld.Perf,0,-128);
    where (!((Index+1) % 128))
      to_torus_dim(&store[4],ParallelOld.Perf,0,-127);
    else
      to_torus_dim(&store[4],ParallelOld.Perf,0,1);
    

    if (Rankflag)
      {
	/* Assign each structure its rank within the population. */
	/* rank = Popsize for best, rank = 1 for worst		*/
	/* Use the Needs_evaluation field to store the rank	*/
	
	/* clear the rank fields */
	for (i=0; i<5; i++)
	  rank[i] = 0;
		
	for (i=0; i<5; i++)
	  {
	    /* find the ith best structure */
	    best = -1;
	    perf = 0.0;
	    for (j=0; j<5; j++)
	      {
		where (rank[j] == 0)
		  where (best == -1 || BETTER(store[j],perf))
		    {
		      perf = store[j];
		      best = j;
		    }
	      }
	    /* mark best structure with its rank */
	    rank[best] = Popsize - i;
	  }
	total = 15;
      }
    else
      {
	for (i=0;i<5;i++) {
	  if (Maxflag)
	    total += ((store[i] - Best) * 100);
	  else
	    total += ((Worst - store[i]) * 100);
	}
      }

    /* Probabilistic Selection Algorithm */
    position = PRandint(0,total);
    parallel_sample = 0;
    
    sum = 0;
    
    for(i=0;i<5;i++) {
      if (Rankflag)
	sum += rank[i];
      else
	if (Maxflag)
	  sum += ((store[i] - Best) * 100);
	else
	  sum += ((Worst - store[i]) * 100);
      where (!parallel_sample)
	where (position < sum) {
	  parallel_sample = i+1;
	}
    }
    
    parallel_sample--;

    where(parallel_sample == 0)
      source = Index;
    where(parallel_sample == 1)
      source = Index - 128;
    where(parallel_sample == 2)
      where (!((Index-1) % 128))
	source = Index - 127;
      else
	source = Index + 1;
    where(parallel_sample == 3)
      source = Index + 128;
    where(parallel_sample == 4)
      where(!(Index % 128))
	source = Index + 127;
      else
	source = Index - 1;

    read_from_pvar(sample,source);

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
  Trace("Local Select completed");
}

/*** end of file ***/















