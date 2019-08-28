
/*
 *  file:	generate.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	One generation consists of
 *			(1) forming a new population of structures.
 *			(2) evaluating the population.
 *			(3) gathering performance statistics.
 *
 *  modified:	7 feb 86
 *
 *		2 dec 86: call Measure() before Done() so that
 *		we can quit upon convergence.  Measure() nows
 *		calls Converge().
 */

#include "Pextern.h"
extern int  Done();

POperators()
{
  with(physical) {
    if (Uniflag) {
      PUnpack(ParallelNew.Gene, PBitstring, Length);
      PMutate();
      PUniform();
      PPack(PBitstring, ParallelNew.Gene, Length);
    }
    else {
      PUnpack(ParallelNew.Gene, PBitstring, Length);
      PMutate();
      PPack(PBitstring, ParallelNew.Gene, Length);
      PCrossover();
    }
  }
}

Generate()
{
  static int rsflag = 1;	/* flag cleared after restart		*/
    
  if (Traceflag)
    printf("                    Gen %d\n",Gen);
  Trace("Generate entered");
  Time(0,"");
  
  /* create a new population */
  
  if (Restartflag && rsflag)
    {
      /* this is a restart so read checkpoint file */
      Restart();
      rsflag = 0;	/* disable local restart flag. */
      PDistribute();
      PConverge();
    }
  
  else if (Gen == 0)
    /* this is a fresh experiment */
    {
      PInitialize();	/* form an initial population */
      Spin++;	
    }
  
  else
    /* form a new population from */
    /* the old one via genetic operators */
    {
      if (Probflag)
	PProb_Select();
      else
	if (Localflag)
	  PLocal_Select();
	else
	  PSelect();
      POperators();
      if (Eliteflag)
	PElitist();
      Spin++;
    }
    
  /* evaluate the newly formed population */
  PEvaluate();
  
  /* gather performance statistics */
  PMeasure();
  
  /* check termination condition for this experiment	*/
  Doneflag = Done();
  
  
  /* checkpoint if appropriate */
  if (Num_dumps && Dump_freq && Gen % Dump_freq == 0)
    {
      if (Num_dumps > 1)
	{
	  sprintf(Dumpfile, "dump.%d", Curr_dump);
	  Curr_dump = (Curr_dump + 1) % Num_dumps;
	  PCheckpoint(Dumpfile);
	}
      PCheckpoint(Ckptfile);
    }
  else
    {
      if (Doneflag)
	{
	  if (Lastflag)
	    PCheckpoint(Ckptfile);
	  else
	    if (Savesize) Printbest();
	}
    }
  
    
  /* swap pointers for next generation */

  with(physical)
    ParallelOld = ParallelNew;

  /* update generation counter */
  Gen++;

  Time(1,"Generate");
  Trace("Generate completed");
}

/*  end of file */
