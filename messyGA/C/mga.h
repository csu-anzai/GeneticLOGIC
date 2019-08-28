/*===================================================
      file : mga.h
 
   purpose : global variables
 
 developed : 1991
  
    author : Kalyanmoy Deb
=====================================================*/

#include <stdio.h>
#include <math.h>
#include "mga.def"


typedef int INDV_ID, GENE_ID, ALLELES, BOOLEAN;

struct GENE {               /* a gene */
	GENE_ID     genenumber;     /* gene number */
	ALLELES     allele;     /* allele value */
	struct GENE  *nextgene;     /* pointer to the next gene */
};

struct INDIVIDUAL {         /* an individual */
	struct GENE  *firstgene;    /* pointer to the first gene */
	struct GENE   *lastgene;    /* pointer to the last gene */
	unsigned     *fullchrom;   /* an array of filled-up chromosome */
	unsigned      *fullgene;   /* an array of genes */
	int            chromlen;    /* chromosome length */
	int             genelen;    /* gene length */
	double          fitness;    /* objective function value */
};

struct INDIVIDUAL *oldpop,*newpop,best_indv;  /* oldpop, newpop, and best
						 individual */

/* system variables */
int era;                    /* current era */
int max_era;                /* maximum order */
int order;                  /* current order */
int problem_length;         /* problem size */
int *copies;      /* number of copies in initial pop. */
long popsize;               /* population size */
int bytesize;               /* number of bytes to store raw chrom */
int *bytelimit;             /* bits required for each byte */
unsigned *template;         /* template array */
double function_evaluations = 0.0;  /* number of function evaluations */
float stopfactor;           /* termination factor */
double prop_bestindv;       /* proportion of best individual */
double templatefitness;     /* template fitness */


/* objective function related variables */
double **chromfitness; /* fitness array */
GENE_ID **genesets;  /* genes in a subfunction */
int *str_length;                   /* size of subfunctions */
int *table_id;               /* table id of subfunctions */
float *scale;                /* scale for each subfucntion */
BOOLEAN *taborfunc;           /* subfunction is a table or a function */
int numsubfunc;                /* number of subfunctions */
double avgfitness, maxfitness, minfitness, avgstrlen;   /* statistics */

/* operator related variables */
double cut_prob;               /* probability of cut */
double splice_prob;            /* probability of splice */
double allelic_mut_prob;       /* prob. of allelic mutation */
double genic_mut_prob;         /* prob. of genic mutation */
int init_select_gen;           /* initial selection duration */
int cutpop_gen;                /* duration till the population is cut */
int prim_gen;                  /* duration of primordial phase */
int maxgen;                    /* maximum generation */
INDV_ID *shuffle;                  /* shuffle array for selection */
int gen;                       /* generation counter */
INDV_ID pick;                      /* selection counter */
int juxtpopsize;               /* popsize in juxtapositional phase */
int cut_every_other_gen;       /* duration until cut at every other gen */
long allelicmutation = 0;      /* counter for allelic mutation */
long genicmutation = 0;        /* counter for genic mutation */
int thres;                     /* threshold value */
int shuffle_num = 0;           /* shuffle number */

/* partition variables */
int numpartition;              /* number of partitions */
int *partition_len;     /* size of each partition */
GENE_ID **partition_genes;  /* genes in each partition */
							
/* variables for recording population history */
int nextpopstatgen;            /* next generation to be printed */
int countpopstatgen = 0;       /* counter for population */
int *sortpopstatgen;           /* sorted generations */

int gencount=0;                /* counter for total generations */

double *oldrand;               /* array used in random no. generator */
int jrand;                     /* counter in random number generator */
double randomseed;             /* random seed */

struct STACKTYPE {             /* stack for cut and splice operation */
	struct INDIVIDUAL genefirst;   /* an individual */
	struct STACKTYPE   *nextmem;   /* pointer to the next individual */
};
typedef struct STACKTYPE *STACKPTR;    /* pointer to the stack STACKTYPE */
STACKPTR chrom_stack = NIL;            /* stack after cut only */
STACKPTR newchrom_stack = NIL;         /* stack after cut and splice */ 

FILE *fin;                     /* pointer to the input file, era */

/*  initialize flags */
BOOLEAN cutpopflag = 0;           /* population size needs to be changed */
BOOLEAN thresholdingflag = 0;     /* thresholding is active */
BOOLEAN tiebreakingflag = 0;      /* tie-breaking is active */
BOOLEAN traceflag = 1;            /* intermediate printing is on */
BOOLEAN levelmgaflag = 0;         /* level-wise mGA is on */
BOOLEAN partitionflag = 0;        /* partition proportions is required */
BOOLEAN maximizationflag = 1;     /* a maximization problem */
BOOLEAN popprintflag = 0;         /* population printing is on */ 
BOOLEAN plotstatflag = 1;         /* file for plotting data is required */
BOOLEAN stopmgaflag = 0;          /* stopping criteria is reached */
BOOLEAN r_initpop_flag = 0;       /* if reduced population is desired */
BOOLEAN extrapopflag = 0;

/*  File names */
char Inputfilename[30];       /*  file of input parameters  */
char Erafilename[30];       /*  file of individual order info */
char Objfilename[30];         /*  file of objective function information  */
char Templatefilename[30];    /*  file of template */
char Partinfilename[30];       /*  file of structures to be printed */
char Outputfilename[30];      /*  file of output */
char Poprinfilename[30];         /*  file of population history */
char Poproutfilename[30];         /*  file of population history */
char Partoutfilename[30];        /*  file of partition proportions */
char Plotfilename[30];    /*  file of population statistics */
char Extrafilename[30]; 

/* problem specific global parameters */
