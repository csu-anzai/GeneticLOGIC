
/*
 *  file:	init.cs
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	create an initial population of structures,
 *		and initalize some performance variables.
 *		This is called at the start of each experiment.
 *
 *  modified:	7 feb 86
 *		12 nov 66:  pass Length to Pack()
 *		23 sep 90:  handle floating point representation in initfile
 */


#include "Pextern.h"

PInitialize()
{
  FILE *fp, *fopen();
  register int i, j;	/* loop control				*/
  int status;		/* indicates end of file in initfile	*/
  char temp_bit;
  double temp_vector;
 
  Trace("Initialize entered");
  Dtrace("initialize");
  Time(0,"");
  
  if (Totalexperiments > 1)
    sprintf(Bestfile, "%s.%d", Minfile, Experiment+1);
  
  /* prepare for new experiment */
  Doneflag = 0;
  Curr_dump = 0;
  Bestsize = 0;
  Mu_next = 0;
  Trials = Gen = 0;
  
  Lost = Conv = 0;
  Plateau = 0;
  Spin = 0;
  Onsum = Offsum = 0.0;
  for (i=0; i<Windowsize; i++) Window[i] = 0.0;
  
  /* set up initial population */
  
  i = 0;			/* current structure */
  
  with(physical) {
    if (Initflag)		/* get some structures from Initfile */
      {
	if ((fp = fopen(Initfile, "r")) == NULL)
	  {
	    char msg[40];
	    sprintf(msg, "Initialize: can't open %s", Initfile);
	    Error(msg);
	  }
	
	status = 1;
	if (Floatflag)
	  {
	    for (j = 0; j < Genes && status != EOF; j++)
	      {
		status = fscanf(fp, "%lf", &temp_vector);		
		[i]PVector[j] = temp_vector;
	      }
	  }
	else
	  for(j=0;j<Length;j++) {
	    status = fscanf(fp, "%c", &temp_bit);
	    [i]PBitstring[j] = temp_bit;
	  }
	
	while (status != EOF && i < Popsize)
	  {
	    i++;
	    
	    /* get the next structure */
	    if (Floatflag)
	      for (j = 0; j < Genes && status != EOF; j++) {
		status = fscanf(fp, "%lf", &temp_vector);	
		[i]PVector[j] = temp_vector;
	      }
	    else
	      for(j=0;j < Length;j++) {
		status = fscanf(fp, "%c", &temp_bit);
		[i]PBitstring[j] = temp_bit;
	      }
	  }
	fclose(fp);
	
	
	if (Floatflag) {
	  PStringRep(PVector, PBitstring, Genes);
	}
	
	PPack(PBitstring, ParallelNew.Gene, Length);
	
      }
    
    /********************************************************/
    /* The seed for the random number generator is saved	*/
    /* after the initialization of the first population	*/
    /* in each experiment.  The saved value is used as the	*/
    /* Seed in subsequent experiments.  The reason is:	*/
    /* often we will run several experiments with one set	*/
    /* of parameters, and compare the results with several	*/
    /* experiments run with a different set of parameters.	*/
    /* This means that for all runs which use the		*/
    /* same population size, the initial populations for	*/
    /* corresponding experiments will be the same.		*/
    /********************************************************/
    if ( Experiment > 0 ) Seed = Initseed;
    
    for (j = 0; j < Length; j++)
      {
	where((PRandint(0,1)) == 1)
	  PBitstring[j] = '1';
        else
	  PBitstring[j] = '0';
      }
    PPack(PBitstring , ParallelNew.Gene , Length);
    
    Initseed = Seed;

    Time(1,"Initialize");
    Trace("Initialize completed");
  }
}
/** end of file **/


