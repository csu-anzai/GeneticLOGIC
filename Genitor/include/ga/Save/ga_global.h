/*********************************************************************
 * These REQUIRED and OPTIONAL parameters can be chosen by the user. *
 *********************************************************************/

 /* REQUIRED */
 int     PoolSize;
 int     StringLength;
 int     NumberTrials;

 char    NodeFile[80];       /* contains coordinates of tsp nodes */

 /* OPTIONAL */
 long    RandomSeed;
 float   SelectionBias;
 float   MutateRate = 0.0;
 int     StatusInterval = 0; /* dump status every nth generation */
 int     DumpInterval = 0;   /* save state of every nth generation */
 char    SeedPool[80];       /* file containing init data */
 char    FinalPool[80];      /* file containing final results */
 char    DumpBase[80];       /* basename of file(s) into
							    which to dump population */
/***************************************************/

 int     CurrentGeneration = 0;  
 POOLPTR Pool;
