/*----------------------------------------------------------------------------*/
/* report.c - generation report files                                         */
/*----------------------------------------------------------------------------*/

#include "external.h"

report(timegen)
/* Write the population report */
double timegen;
{
#ifdef NCUBE
    double globmax, globmin, globavg, globsum;
    double dmaxn(), dminn(), dsumn(), amicclk();
    int    globcross, globmut, globpop, globexchg;
#endif
#ifdef UNIX
    long   clock();
#endif
    void   repchar(), skip();
    int    writepop(), writestats();

    repchar("-",LINELENGTH); 
    skip(1);
    if(printstrings == 1)
    {
        repchar(" ",((LINELENGTH-17)/2));
        fprintf(outfp,"Population Report\n");
        fprintf(outfp, "Generation %3d", gen);
        repchar(" ",(LINELENGTH-28)); 
        fprintf(outfp, "Generation %3d\n", (gen+1));
#ifdef NCUBE
        fprintf(outfp,"num node  string ");
#else
        fprintf(outfp,"num   string ");
#endif
        repchar(" ",lchrom-5);
        fprintf(outfp,"fitness    parents xsite  ");
        fprintf(outfp,"string ");
        repchar(" ",lchrom-5);
        fprintf(outfp,"fitness\n");
        repchar("-",LINELENGTH);
        skip(1);

        /* write out string info from all nodes         */
        /* in order from lowest to highest node number  */
#ifdef NCUBE
        printnodes(writepop);
#else
        writepop();
#endif
        repchar("-",LINELENGTH); 
        skip(1);

#ifdef NCUBE
        /* Generation statistics and accumulated values */
        /* in order from lowest to highest node number  */
        printnodes(writestats);
        repchar("-",LINELENGTH);
        skip(1);
#endif
    }

    /* write the summary statistics in global mode  */
#ifdef NCUBE
    timegen = (amicclk() - timegen)/1.0e6;
#endif
#ifdef UNIX
    timegen = ((double) clock() - timegen)/1.0e6;
#endif
    fprintf(outfp,"Generation %d Accumulated Statistics: CPU Time = %f\n",
            gen,timegen);

#ifdef NCUBE
    /* compute global sum of crossovers and mutations */
    /* and other global max and min variables         */
    thebest();
    globcross = isumn(ncross);
    globmut   = isumn(nmutation);
    globpop   = isumn(popsize);
    globmin   = dminn(min);
    globmax   = dmaxn(max);
    globsum   = dsumn(sumfitness);
    globavg   = globsum/(double) globpop;

    fprintf(outfp,"Total Crossovers = %d, Total Mutations = %d\n",
                   globcross,globmut);
    if(maxnodes > 1)
    {
        globexchg = isumn(nexchange);
        fprintf(outfp,"Total Exchanges = %d, Exchange Direction = %d\n",
                 globexchg,exchngdir);
    }
    fprintf(outfp,"min = %f   max = %f   avg = %f   sum = %f\n",
                 globmin,globmax,globavg,globsum);
    fprintf(outfp,"Global Best Individual so far, Node %d, Generation %d:\n",
                 bestfit.node,bestfit.generation);
    fprintf(outfp,"Fitness = %f: ", bestfit.fitness);
    writechrom((&bestfit)->chrom);
#else
    fprintf(outfp,"Total Crossovers = %d, Total Mutations = %d\n",
                   ncross,nmutation);
    fprintf(outfp,"min = %f   max = %f   avg = %f   sum = %f\n",
                 min,max,avg,sumfitness);
    fprintf(outfp,"Global Best Individual so far, Generation %d:\n",
                 bestfit.generation);
    fprintf(outfp,"Fitness = %f: ", bestfit.fitness);
    writechrom((&bestfit)->chrom);
#endif
    skip(1);
    repchar("-",LINELENGTH);
    skip(1);

    /* write out plotting information for each generation */
    /* output is separated with tabs for input into spreadsheets */
#ifdef NCUBE
    fprintf(plotfp,"%d\t%f\t%f\t%f",gen,globmin,globavg,globmax);
    fprintf(plotfp,"\t%d\t%d",globcross,globmut);
    if(maxnodes > 1)
    {
        fprintf(plotfp,"\t%d\t%d",globexchg,exchngdir);
    }
#else
    fprintf(plotfp,"%d\t%f\t%f\t%f",gen,min,avg,max);
    fprintf(plotfp,"\t%d\t%d",ncross,nmutation);
#endif
    fprintf(plotfp,"\n");

    /* application dependent report */
    app_report(); 
}


#ifdef NCUBE
printnodes(printfunc)
/* execute printfunc sequentially from zero to maxnodes */
/* uses pointer to function, see K&R, p. 115 for details */
int (*printfunc)();
{
    char *buf;
    int  send, recv, flag;
    int  msgtype = 0xfd02;
    int  lastnode;

    /* if only one node, print and return */
    if(maxnodes == 1)
    {
        (*printfunc)(outfp);
        return(0);
    }

    /* for multiple nodes: print in local mode */
    nlocal();
    lastnode = maxnodes - 1;

    /* node zero prints first, then waits for last */
    /* to get finished printing, before continuing */
    if(mynode == 0) 
    {
        (*printfunc)(outfp);
        send = mynode + 1;
        nwrite(buf,sizeof(*buf),send,msgtype,&flag);
        recv = lastnode;
        nread(buf,sizeof(*buf),&recv,&msgtype,&flag);
    }
    /* all other nodes wait for previous node to   */
    /* finish before printing.  Last node informs  */
    /* node zero that everybody is finished...     */
    else
    {
        recv = mynode - 1;
        nread(buf,sizeof(*buf),&recv,&msgtype,&flag);
        (*printfunc)(outfp);
        if(mynode == lastnode)
            send = 0;
        else
            send = mynode + 1;
        nwrite(buf,sizeof(*buf),send,msgtype,&flag);
    }
    nglobal();
    return(0);
}


int thebest()
{
    struct bestever globbest, fromnode;
    void nbrdcst();
    int  send, recv, nbytes;
    int  i, flag;
    int  msgtype = 0xfd03;

    /* node zero reads everybody else's best */
    /* and finds the global best individual  */
    nbytes = sizeof(struct bestever);
    if(mynode == 0)
    {
        globbest = bestfit;
        for(i=1; i<maxnodes; i++)
        {
            recv = i;
            nread(&fromnode,nbytes,&recv,&msgtype,&flag);
            if(fromnode.fitness > globbest.fitness) 
            {
                globbest = fromnode;
            }
        }
        bestfit = globbest;
    }
    /* all other nodes report their current best */
    else
    {
        send = 0;
        nwrite(&bestfit,nbytes,send,msgtype,&flag);
    }

    /* now broadcast node zero's result to everybody */
    nbrdcst(&bestfit,nbytes);
}
#endif


writepop()
{
    struct individual *pind;
    int j;

    for(j=0; j<popsize; j++)
    {
#ifdef NCUBE
        fprintf(outfp,"%3d,%3d)  ",j+1,mynode);
#else
        fprintf(outfp,"%3d)  ",j+1);
#endif
   
        /* Old string */
        pind = &(oldpop[j]);
        writechrom(pind->chrom);
        fprintf(outfp,"  %8f | ", pind->fitness);
   
        /* New string */
        pind = &(newpop[j]);
        fprintf(outfp,"(%2d,%2d)   %2d   ",
        pind->parent[0], pind->parent[1], pind->xsite);
        writechrom(pind->chrom);
        fprintf(outfp,"  %8f\n", pind->fitness);
    }
}


writechrom(chrom)
/* Write a chromosome as a string of ones and zeroes            */
/* note that the most significant bit of the chromosome is the  */
/* RIGHTMOST bit, not the leftmost bit, as would be expected... */
unsigned *chrom;
{
    int j, k, stop;
    unsigned mask = 1, tmp;

    for(k = 0; k < chromsize; k++)
    {
        tmp = chrom[k];
        if(k == (chromsize-1))
            stop = lchrom - (k*UINTSIZE);
        else
            stop = UINTSIZE;

        for(j = 0; j < stop; j++)
        {
            if(tmp&mask)
                fprintf(outfp,"1");
            else
                fprintf(outfp,"0");
            tmp = tmp>>1;
        }
    }
}


#ifdef NCUBE
writestats()
/* statistics per node */
{
    fprintf(outfp,"Node %3d: Crossovers = %d, Mutations = %d, Exchanges = %d\n",
            mynode,ncross,nmutation,nexchange);
    fprintf(outfp,"Node %3d: Min = %f, Max = %f, Avg = %f, Sum = %f\n",
            mynode,min,max,avg,sumfitness);
}
#endif
