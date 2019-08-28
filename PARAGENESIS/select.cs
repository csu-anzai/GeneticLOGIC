
/*
 *  file:	select.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	choose a new population
 *
 *  modified:	10 sep 90: include ranking option, handle max/min option
 */

#include "Pextern.h"

PSelect()
{
  int sample[Popsize];	/* pointers to Selected structures	*/
  int:physical parallel_sample;
  int rank[Popsize];
  double old_perf[Popsize];
  double:physical parallel_expected;
  double factor;		/* normalizer for expected value        */
  double perf;		/* next best perf (for ranking)		*/
  double ptr;		/* determines fractional selection	*/
  double rank_max;	/* max number of offspring under ranking */
  double sum;             /* control for selection loop           */
  int best;		/* index of next best structure		*/
  register int i;		/* loop control				*/
  register int j;		/* loop control				*/
  register int k;		/* loop control				*/
  register int temp;	/* used for swapping pointers		*/
  
  Trace("Select entered");
  Dtrace("select");
  Time(0,"");

  with(physical) {
    if (Rankflag)
      {
	/* Assign each structure its rank within the population. */
	/* rank = Popsize for best, rank = 1 for worst		*/
	/* Use the Needs_evaluation field to store the rank	*/
	
	for (i=0; i<Popsize; i++)
	  rank[i] = 0;
	
	read_from_pvar(old_perf,ParallelOld.Perf);
	
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
	/* normalizer for ranking selection probabilities */
	rank_max = 2.0 - Rank_min;
	factor = (rank_max - Rank_min) / (double) Popsize;
	
	ParallelOld.Needs_evaluation = write_to_pvar(rank);
      }
    else
      {
	/* normalizer for proportional selection probabilities */
	factor = Maxflag ? 1.0/(Ave_current_perf - Worst) :
	                   1.0/(Worst - Ave_current_perf);
      }
    /* Stochastic universal sampling algorithm by James E. Baker */
    
    k=0;		/* index of next Selected structure */
    
    if (Rankflag)
      {
	parallel_expected = Rank_min + ParallelOld.Needs_evaluation * factor;
      }
    else
      {
	if (Maxflag) {
	  where(ParallelOld.Perf > Worst)
	    parallel_expected = (ParallelOld.Perf - Worst) * factor;
	  else 
	    parallel_expected = 0.0;
	}
	else {
	  where(ParallelOld.Perf < Worst)
	    parallel_expected = (Worst - ParallelOld.Perf) * factor;
          else 
	    parallel_expected = 0.0;
	}
      }

    read_from_pvar(old_perf,parallel_expected);

    ptr = Rand();   /* spin the wheel one time */
    
    for (sum=i=0; i < Popsize; i++)
      {
	for (sum += old_perf[i]; sum > ptr; ptr++)
	  sample[k++] = i;
      }
    
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

  

/* Choose survivors from old population uniformly, without replacement	*/

Gap(sample)
int sample[];
{
	int samp2[Popsize];	/* a random permutation of 0 .. Popsize-1 */
	register int i,j;	/* loop control				*/
	int temp;		/* for swapping				*/

	/* randomly shuffle the new structures */
	for (i=0; i<Popsize; i++)
	{
		j = Randint(i,Popsize-1);
		temp = sample[j];
		sample[j] = sample[i];
		sample[i] = temp;
	}

	/* construct a uniform shuffle */
	for (j=0; j<Popsize; j++) samp2[j]=j;
	for (j=0; j<Popsize; j++)
	{
		i = Randint(j, Popsize-1);
		temp = samp2[i];
		samp2[i] = samp2[j];
		samp2[j] = temp;
	}

	/* now choose survivors */
	for (i=Gapsize*Popsize; i<Popsize; i++)
		sample[i] = samp2[i];
}


/*** end of file ***/
