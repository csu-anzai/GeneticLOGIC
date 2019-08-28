/*----------------------------------------------------------------------------*/
/* initial.c - functions to get things set up and initialized                 */
/*----------------------------------------------------------------------------*/

#include "external.h"

initialize()
/* Initialization Coordinator */
{
    /* get basic problem values from input file */
    initdata();

    /* define chromosome size in terms of machine bytes, ie  */
    /* length of chromosome in bits (lchrom)/(bits-per-byte) */
    /* chromsize must be known for malloc() of chrom pointer */
    chromsize = (lchrom/UINTSIZE);
    if(lchrom%UINTSIZE) chromsize++;

    /* malloc space for global data structures */
    initmalloc();

    /* initialize application dependent variables*/
    app_init();

    /* initialize random number generator */
    randomize();

    /* initialize global counters/values */
    nmutation = 0;
    ncross = 0;
#ifdef NCUBE
    nexchange = 0;
#endif
    bestfit.fitness = 0.0;
    bestfit.generation = 0;

    /* initialize the populations and report statistics */
    initpop();
    statistics(oldpop);
    initreport();
}
  
  
initdata()
/* data inquiry and setup */
{
    char  answer[2];
#ifdef NCUBE
    float exchngperc;
#endif

    if(numfiles == 0)
    {
        fprintf(outfp,"\n ------- SGA Data Entry and Initialization -------\n");
#ifdef NCUBE
        fprintf(outfp," Enter the population size per node ---> "); 
#else
        fprintf(outfp," Enter the population size ------------> "); 
#endif
    }
    fscanf(infp,"%d", &popsize);
    if((popsize%2) != 0)
      {
	fprintf(outfp, "Sorry! only even population sizes are allowed. \n Incrementing popsize by one.\n");
	popsize++;
      };

    if(numfiles == 0)
        fprintf(outfp," Enter chromosome length --------------> "); 
    fscanf(infp,"%d", &lchrom);

    if(numfiles == 0)
        fprintf(outfp," Print chromosome strings? (y/n) ------> ");
    fscanf(infp,"%s",answer);
    if(strncmp(answer,"n",1) == 0) printstrings = 0;

    if(numfiles == 0)
        fprintf(outfp," Enter maximum number of generations --> "); 
    fscanf(infp,"%d", &maxgen);

#ifdef NCUBE
    if(numfiles == 0)
        fprintf(outfp," Percent population exchange per node -> ");
    fscanf(infp,"%f", &exchngperc);
    exchngperc /= 100.0;
    if(exchngperc > 0.5)
        fprintf(outfp,"WARNING: Exchange percent>50%% not recommended!\n");
    exchngnum = (int) (exchngperc * (float) popsize);
    if(exchngnum > popsize)
    {
        fprintf(outfp,"FATAL: Exchange number (%d) > popsize (%d)\n",
                exchngnum,popsize);
        exit(-1);
    }

    if(numfiles == 0)
        fprintf(outfp," Enter # of generations per exchange --> ");
    fscanf(infp,"%d", &exchnggen);
#endif

    if(numfiles == 0)
        fprintf(outfp," Enter crossover probability ----------> "); 
    fscanf(infp,"%f", &pcross);

    if(numfiles == 0)
        fprintf(outfp," Enter mutation probability -----------> "); 
    fscanf(infp,"%f", &pmutation);

    /* any application-dependent global input */
    app_data();
}


initpop()
/* Initialize a population at random */
{
    int j, j1, k, stop;
    unsigned mask = 1;

    for(j = 0; j < popsize; j++)
    {
        for(k = 0; k < chromsize; k++)
        {
            oldpop[j].chrom[k] = 0;
            if(k == (chromsize-1))
                stop = lchrom - (k*UINTSIZE);
            else
                stop = UINTSIZE;

            /* A fair coin toss */
            for(j1 = 1; j1 <= stop; j1++)
            {
               oldpop[j].chrom[k] = oldpop[j].chrom[k]<<1;
               if(flip(0.5))
                  oldpop[j].chrom[k] = oldpop[j].chrom[k]|mask;
            }
        }
        oldpop[j].parent[0] = 0; /* Initialize parent info. */
        oldpop[j].parent[1] = 0;
        oldpop[j].xsite = 0;
        objfunc(&(oldpop[j]));  /* Evaluate initial fitness */
    }
}


initreport()
/* Initial report */
{
    void   skip();
#ifdef NCUBE
    double globmin, globmax, globsum, globavg;
    double dminn(), dmaxn(), dsumn();
    int    globpop, isumn();

    globpop   = isumn(popsize);
    globmin   = dminn(min);
    globmax   = dmaxn(max);
    globsum   = dsumn(sumfitness);
    globavg   = globsum/(double) globpop;
#endif

    skip(1);
    fprintf(outfp," SGA Parameters\n");
    fprintf(outfp," -------------------------------------------------\n");
#ifdef NCUBE
    fprintf(outfp," Total Population size on %3d nodes =   %d\n", 
                   maxnodes,globpop);
#else
    fprintf(outfp," Total Population size              =   %d\n",popsize);
#endif
    fprintf(outfp," Chromosome length (lchrom)         =   %d\n",lchrom);
    fprintf(outfp," Maximum # of generations (maxgen)  =   %d\n",maxgen);
#ifdef NCUBE
    fprintf(outfp," Number of individuals exchanged    =   %d\n",exchngnum);
    fprintf(outfp," Generations between exchanges      =   %d\n",exchnggen);
#endif
    fprintf(outfp," Crossover probability (pcross)     = %f\n", pcross);
    fprintf(outfp," Mutation  probability (pmutation)  = %f\n", pmutation);
    skip(1);

#ifdef NCUBE
    fprintf(outfp," Global Initial Generation Statistics on all nodes\n");
    fprintf(outfp," -------------------------------------------------\n");
    fprintf(outfp," Initial population maximum fitness = %f\n", globmax);
    fprintf(outfp," Initial population average fitness = %f\n", globavg);
    fprintf(outfp," Initial population minimum fitness = %f\n\n", globmin);
#endif

    /* application dependant report */
    app_initreport();

    fflush(outfp);
}
