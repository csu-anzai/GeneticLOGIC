
/*
 *  file:	checkpoint.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	save global variables in a file for later restart
 *
 *  modified:	18 apr 86
 *		12 nov 86: pass Length to Unpack()
 */

#include "Pextern.h"

PCheckpoint(ckptfile)
char ckptfile[];
{
  FILE *fp, *fopen();
  int i,j;
 
  Trace("Checkpoint entered");
  Dtrace("checkpointing");
  Time(0,"");
  
  fp = fopen(ckptfile, "w");
  fprintf(fp, "Experiment %d\n", Experiment);
  fprintf(fp, "Totonline %12.6e\n", Totonline);
  fprintf(fp, "Totoffline %12.6e\n", Totoffline);
  fprintf(fp, "Gen %d\n", Gen);
  fprintf(fp, "Onsum  %12.6e\n", Onsum);
  fprintf(fp, "Offsum %12.6e\n", Offsum);
  fprintf(fp, "Trials %d\n", Trials);
  fprintf(fp, "Plateau %d\n", Plateau);
  fprintf(fp, "Best %12.6e\n", Best);
  fprintf(fp, "Worst  %12.6e\n", Worst);
  fprintf(fp, "Spin %d\n", Spin);
  fprintf(fp, "Curr_dump %d\n", Curr_dump);
  fprintf(fp, "Mu_next %d\n", Mu_next);
  fprintf(fp, "Random Seed %lu\n", Seed);
  fprintf(fp, "Initialization Seed %lu\n", Initseed);
  
  fprintf(fp,"\n");
  fprintf(fp, "Window\n");
  for (i=0; i<Windowsize; i++) fprintf(fp, "%12.6e\n", Window[i]);
  fprintf(fp,"\n");
  
  with(physical) {
    PUnpack(ParallelNew.Gene, PBitstring, Length);

    for (i=0; i<Popsize; i++)
      {
	for(j=0; j < Length; j++)
	  fprintf(fp, "%c", [i]PBitstring[j]);
	fprintf(fp, " %12.8e ", [i]ParallelNew.Perf);
	fprintf(fp, "%1d", [i]ParallelNew.Needs_evaluation);
	fprintf(fp, "\n");
      }
    
    
  
    if (Floatflag)	/* print floating point representation */
      {
	fprintf(fp, "\n");
	PFloatRep(PBitstring, PVector, Genes);
	for (i=0; i<Popsize; i++)
	  {
	    for (j=0; j < Genes; j++)
	      {
		fprintf(fp, Gene[j].format, [i]PVector[j]);
		fprintf(fp, " ");
	      }
	    fprintf(fp, " %10.4f\n", [i]ParallelNew.Perf);
	  }
      }
  }
  fclose(fp);
  
  /*  save the best structures */
  if (Savesize)
    Printbest();

  Time(1,"Checkpoint");
  Trace("Checkpoint completed");
}

/** end of file **/
