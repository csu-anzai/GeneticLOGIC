/*----------------------------------------------------------------------------*/
/* app.c - application dependent routines, change these for different problem */
/*----------------------------------------------------------------------------*/

#include <math.h>
#include "external.h"

application()
/* this routine should contain any application-dependent computations */
/* that should be performed before each GA cycle. called by main()    */
{
}


app_data()
/* application dependent data input, called by init_data() */
/* ask your input questions here, and put output in global variables */
{
}


app_free()
/* application dependent free() calls, called by freeall() */
{
}


app_init()
/* application dependent initialization routine called by intialize() */
{
}


app_initreport()
/* Application-dependent initial report called by initialize() */
{
}


app_malloc()
/* application dependent malloc() calls, called by initmalloc() */
{
    char *malloc();
}


app_report()
/* Application-dependent report, called by report() */
{
}


app_stats(pop)
/* Application-dependent statistics calculations called by statistics() */
struct individual *pop;
{
}


objfunc(critter)
/* objective function used in Goldberg's book */
/* fitness function is f(x) = x**n, normalized */
struct individual *critter;
{
    unsigned mask=1;   /* mask for current bit */
    unsigned bitpos;   /* current bit position */
    unsigned tp;
    double pow(), bitpow, coef;
    int j, k, stop;
    int n = 40;

    critter->fitness = 0.0;
    coef = pow(2.0,(double) lchrom) - 1.0;

    /* loop thru number of bytes holding chromosome */
    for(k = 0; k < chromsize; k++)
    {
        if(k == (chromsize-1))
            stop = lchrom-(k*UINTSIZE);
        else
            stop = UINTSIZE;

        /* loop thru bits in current byte */
        tp = critter->chrom[k];
        for(j = 0; j < stop; j++)
        {
            bitpos = j + UINTSIZE*k; 
            bitpow = pow(2.0,(double) bitpos);
            /* test for current bit 0 or 1 */
            if((tp&mask) == 1)
            {
                bitpow = pow(2.0,(double) bitpos);
                critter->fitness = critter->fitness + bitpow;
            }
            tp = tp>>1;
        }
    }

    /* normalize the fitness */
    critter->fitness = critter->fitness/coef;
}
