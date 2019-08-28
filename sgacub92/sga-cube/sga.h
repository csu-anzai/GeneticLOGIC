/*----------------------------------------------------------------------------*/
/* sga.h - global declarations for main(), all variable declared herein must  */
/*         also be defined as extern variables in external.h !!!              */
/*----------------------------------------------------------------------------*/

#define LINELENGTH 80                                    /* width of printout */
#define BITS_PER_BYTE 8            /* number of bits per byte on this machine */
#define UINTSIZE (BITS_PER_BYTE*sizeof(unsigned))    /* # of bits in unsigned */
#define TOURNAMENT 0                    /* tournament selection mating method */
#define STOCHASTIC 1                    /* stochastic remainder mating method */
#define ROULETTE   2                          /* roulette wheel mating method */

#include <stdio.h>

/* input and output files */
FILE *infp, *outfp, *plotfp;


/* Global structures and variables */
struct individual 
{
    unsigned *chrom;                  /* chromosome string for the individual */
    double   fitness;                            /* fitness of the individual */
    int      xsite;                               /* crossover site at mating */
    int      parent[2];                  /* who the parents of offspring were */
    int      *utility;           /* utility field can be used as pointer to a */
                /* dynamically allocated, application-specific data structure */
};
struct bestever
{
    unsigned *chrom;        /* chromosome string for the best-ever individual */
    double   fitness;                  /* fitness of the best-ever individual */
    int      node;                         /* node from whence came best-ever */
    int      generation;                      /* generation which produced it */
};

struct individual *oldpop;                  /* last generation of individuals */
struct individual *newpop;                  /* next generation of individuals */
struct bestever bestfit;                         /* fittest individual so far */
double sumfitness;                    /* summed fitness for entire population */
double max;                                  /* maximum fitness of population */
double avg;                                  /* average fitness of population */
double min;                                  /* minumum fitness of population */
float  pcross;                                    /* probability of crossover */
float  pmutation;                                  /* probability of mutation */
int    numfiles;                                      /* number of open files */
int    popsize;                                            /* population size */
int    lchrom;                     /* length of the chromosome per individual */
int    chromsize;            /* number of bytes needed to store lchrom string */
int    gen;                                      /* current generation number */
int    maxgen;                                   /* maximum generation number */
int    run;                                             /* current run number */
int    maxruns;                             /* maximum number of runs to make */
int    printstrings = 1;     /* flag to print chromosome strings (default on) */
int    nmutation;                             /* number of mutations per node */
int    ncross;                               /* number of crossovers per node */

#ifdef NCUBE
/* Global declarations for NCUBE parallel port of sga code */
int    nexchange;                             /* number of exchanges per node */
int    exchngnum;          /* number of individuals to exchange between nodes */
int    exchnggen;               /* exchange individuals this many generations */
int    exchngdir;                /* direction (dimension) for exchanges to go */
int    mynode;                            /* the node number of the processor */
int    maxnodes;              /* the maximum number of nodes in the hypercube */
int    dimcube;                                 /* dimension of the hypercube */
int    *neighbors;           /* pointer to array of dimcube nearest neighbors */
#endif

/* Application-dependent declarations go after here... */






