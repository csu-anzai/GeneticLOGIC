
/*
 *  file:	cross.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	perform two-point crossover on entire population
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

char:physical premask[CHARSIZE] = { '\000', '\200', '\300', '\340',
				      '\360', '\370', '\374', '\376' };

char:physical postmask[CHARSIZE] = { '\377', '\177', '\077', '\037',
				       '\017', '\007', '\003', '\001'};


PCrossover()
{
 STRUCTURE:physical Parent2;
 int:physical xpoint1;	/* first crossover point w.r.t. structure */
 int:physical xpoint2;	/* second crossover point w.r.t. structure */
 int:physical xbyte1;	/* first crossed byte */
 int:physical xbit1;	/* first crossed bit in xbyte1 */
 int:physical xbyte2;	/* last crossed byte */
 int:physical xbit2;	/* last crossed bit in xbyte2 */
 register int i;
 int:physical j;
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
       where (Index % 2)
	 Parent2 = [Index-1]ParallelNew;
       else
	 Parent2 = [Index+1]ParallelNew;
       
       
       /* choose two Crossover points */
       xpoint1 = PRandint(0,Length);
       xpoint2 = PRandint(0,Length-1);
       
       /* guarantee that xpoint1 < xpoint2 */
       where (xpoint2 >= xpoint1)
	 xpoint2++;
       else
	 {
	   j = xpoint1;
	   xpoint1 = xpoint2;
	   xpoint2 = j;
	 }
       
       xbyte1 = xpoint1 / CHARSIZE;
       xbit1 = xpoint1 % CHARSIZE;
       xbyte2 = xpoint2 / CHARSIZE;
       xbit2 = xpoint2 % CHARSIZE;

       /* perform crossover */
       ParallelNew.Gene[xbyte1] = (ParallelNew.Gene[xbyte1] & 
				   premask[xbit1]) |
				     (Parent2.Gene[xbyte1] & 
				      postmask[xbit1]);
	   
       for (i=0; i < Bytes; i++) {
	 where ((i>= xbyte1+1) && (i < xbyte2))
	   {
	     ParallelNew.Gene[i] = Parent2.Gene[i];
	   }
       }	   
       
       where (xbyte1 < xbyte2)
	 ParallelNew.Gene[xbyte2] = (ParallelNew.Gene[xbyte2] & 
				     postmask[xbit2]) |
				       (Parent2.Gene[xbyte2] & 
					premask[xbit2]);
       else
	 ParallelNew.Gene[xbyte2] = (ParallelNew.Gene[xbyte2] & 
				     premask[xbit2]) |
				       (Parent2.Gene[xbyte2] & 
					postmask[xbit2]);
     }
   }
   Time(1,"Crossover");
   Trace("Crossover completed");
 }

/** end of file **/
