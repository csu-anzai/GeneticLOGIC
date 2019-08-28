
/*
 *  file:	evaluate.cs
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	evaluate the current population by
 *		calling the user-defined function "eval"
 *
 *  modified:	13 feb 86
 *		12 nov 86: pass Length to Unpack()
 *		15 sep 90: handle floating pt representation,
 *		           change args to eval
 */

#include "Pextern.h"
extern double:physical Peval(); 


PEvaluate()
{

  Trace("Evaluate entered");
  Dtrace("evaluate");
  Time(0,"");

  with(physical) {
    where(Index < Popsize) {

	PUnpack(ParallelNew.Gene, PBitstring, Length);
	if (Floatflag)
	  PFloatRep(PBitstring, PVector, Genes);

	Trace("Starting Peval");
	ParallelNew.Perf = Peval (PBitstring, Length,
				  PVector, Genes);
	Trace("Finishing Peval");
	
	if (Trials == 0)
	  Best = (double)ParallelNew.Perf;
	
	Trials += (int:physical)1;
	Spin = 0;   /* we're making progress */
	
	if (Maxflag)
	  Best >?= ParallelNew.Perf;
	else
	  Best <?= ParallelNew.Perf;
	
	if (Savesize)  PSavebest(ParallelNew); 
	
	Onsum += ParallelNew.Perf;
	Offsum += Best * (+=(int:physical)1);
      }
  }
  

  if (Dumpflag) PCheckpoint(Ckptfile);

  Time(1,"Evaluate");
  Trace("Evaluate completed");
}


/** end of file **/
