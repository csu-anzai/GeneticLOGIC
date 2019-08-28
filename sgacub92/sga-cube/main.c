/*******************************************/
/*      Simple Genetic Algorithm - SGA     */
/*           Haploid Version               */
/* (c)   David Edward Goldberg  1986       */
/*          All Rights Reserved            */
/*      C translation by R.E. Smith        */
/*      nCUBE port by Jeff Earickson       */
/*******************************************/

#include "sga.h"

main(argc,argv)
int argc;
char *argv[];
{
#ifdef UNIX
    long   clock();
#endif
#ifdef NCUBE
    double amicclk();
    void   whoami();
    int    fitcompare();
    int    proc, host;
#endif
    struct individual *temp;
    double starttime, timegen, endtime;
    FILE   *fopen();
    void   copyright();
    char   *malloc();

#ifdef UNIX
    starttime = (double) clock();
#endif
#ifdef NCUBE
    /* find out mynode, maxnodes, and hypercube dim */
    /* then compute array of nearest neighbors for each node */
    starttime = amicclk();
    nglobal();
    whoami(&mynode,&proc,&host,&dimcube);
    maxnodes = 1 << dimcube;
    neighbors = (int *) malloc(dimcube*sizeof(int));
    nearest(mynode,dimcube,neighbors);
#endif

    /* determine input and output from program args */
    numfiles = argc - 1;
    switch(numfiles)
    {
        case 0:
            infp = stdin;
            outfp = stdout;
            break;
        case 1:
            if((infp = fopen(argv[1],"r")) == NULL)
            {
                fprintf(stderr,"Input file %s not found\n",argv[1]);
                exit(-1);
            }
            outfp = stdout;
            break;
        case 2:
            if((infp = fopen(argv[1],"r")) == NULL)
            {
                fprintf(stderr,"Cannot open input file %s\n",argv[1]);
                exit(-1);
            }
            if((outfp = fopen(argv[2],"w")) == NULL)
            {
                fprintf(stderr,"Cannot open output file %s\n",argv[2]);
                exit(-1);
            }
            break;
        default:
            fprintf(stderr,"Usage is: sga [input file] [output file]\n");
            exit(-1);
    }

    /* open a file for plotting output */
    if((plotfp = fopen("plot.output","w")) == NULL)
    {
        fprintf(stderr,"Cannot open plot file\n");
        exit(-1);
    }

    /* print the author/copyright notice */
    copyright();
 
    if(numfiles == 0)
        fprintf(outfp," Number of GA runs to be performed-> ");
    fscanf(infp,"%d",&maxruns);

    for(run=1; run<=maxruns; run++)
    {
        /* Set things up */
        initialize();
        
        for(gen=0; gen<maxgen; gen++)
        {
#ifdef NCUBE
            timegen = amicclk();
#endif
#ifdef UNIX
            timegen = (double) clock();
#endif
            fprintf(outfp,"\nRUN %d of %d: GENERATION %d->%d\n",
                           run,maxruns,gen,maxgen);

            /*application dependent routines*/
            application();

            /* create a new generation */
            generation();

#ifdef NCUBE
            /* if population exchange is to occur, then sort the new */ 
            /* population by fitness, best to worst, and exchange... */
            /* only sort for exchange, because qsort is expensive    */
            /* for very large populations (popsize) per node!!!      */
            if(maxnodes>1 && exchngnum>0 && gen%exchnggen==0)
            {
                qsort((char *)newpop, popsize, sizeof(*newpop), fitcompare);
                exchange(newpop);
            }
#endif

            /* compute fitness statistics on new populations */
            statistics(newpop);

            /* report results for new generation */
            report(timegen);

            /* advance the generation */
            temp = oldpop;
            oldpop = newpop;
            newpop = temp;
        }
        freeall();
    }

    /* print out the total run time */
#ifdef NCUBE
    endtime = (amicclk() - starttime)/1.0e6;
#endif
#ifdef UNIX
    endtime = ((double) clock() - starttime)/1.0e6;
#endif
    fprintf(outfp,"TOTAL CPU TIME = %f\n",endtime);
}


#ifdef NCUBE
int fitcompare(a,b)
struct individual *a, *b;
{
    float value;

    value = b->fitness - a->fitness;
    return((int) value);
}
#endif
