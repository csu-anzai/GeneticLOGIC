
/*
 *  file:	uni-cross.cs
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	perform uniform crossover on entire population
 *
 *  modified:	8 apr 86
 *
 *		13 nov 86:  perform crossover on packed structures.
 *
 *              4 august 87: add second crossover point
 *
 *              11 july 89: fix bug when xbyte1 == xbyte2 
 *                   (bug found by Jon Richardson.)
 */

#include "Pextern.h"

PUniform()
{
 char:physical Parent2;
 bool:physical do_cross;
 register int i;
 static int last;	/* last element to undergo Crossover */
 static int firstflag = 1;
   
   Trace("Crossover entered");
   Dtrace("crossover");
   Time(0,"");
   
   if (firstflag)
     {
       last = (C_rate*Popsize) - 0.5 ;
       firstflag = 0;
     }
   
   with(physical) {
     where(Index < last) {

       /* perform crossover */
       for(i=0;i<Length;i++) {
	 do_cross = PRandint(0,1);
	 where (Index % 2) do_cross = [Index-1]do_cross;
	 where (do_cross) {
	   where (Index % 2)
	     Parent2 = [Index-1]PBitstring[i];
           else
	     Parent2 = [Index+1]PBitstring[i];
	   PBitstring[i] = Parent2;
	 }
       }
     }
   }
   Time(1,"Uniform Crossover");
   Trace("Crossover completed");
 }

/** end of file **/
