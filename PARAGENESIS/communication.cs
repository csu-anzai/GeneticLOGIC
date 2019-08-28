#include "Pextern.h"

void PDistribute() {

  register int i,j;
  double NewPerf[Popsize];
  int NewGene[Popsize];

  Trace("Distribute entered");
  Time(0,"");

  with(physical) {

    for(i=0;i<Popsize;i++) 
      NewPerf[i] = New[i].Perf;
  
    ParallelNew.Perf = write_to_pvar(NewPerf);
    ParallelNew.Needs_evaluation = 0;

    for(j=0;j<Bytes;j++) {
      for(i=0;i<Popsize;i++)
	NewGene[i] = New[i].Gene[j];
      ParallelNew.Gene[j] = write_to_pvar(NewGene);
    }
  }

  Time(1,"Distribute");
  Trace("Distribute finished");
}

void PCollect() {

  register int i,j;
  double NewPerf[Popsize];
  int NewGene[Popsize];

  Trace("Collect entered");
  Time(0,"");

  with(physical) {
    read_from_pvar(NewPerf,ParallelNew.Perf);
    
    for(i=0;i<Popsize;i++) {
      New[i].Perf = NewPerf[i];
      New[i].Needs_evaluation = 0;
    }
    
    for(j=0;j<Bytes;j++) {
      read_from_pvar(NewGene,ParallelNew.Gene[j]);
      for(i=0;i<Popsize;i++)
	New[i].Gene[j] = NewGene[i];
    }
  }

  Time(1,"Collect");
  Trace("Collect finished");
}



