
/*
   The Following defines definitions will need to be adjusted for each
  Application
*/

#define maxpopsize  200

/* Equivalent number of bits for each gene */
#define nbits 8

/* (2^nbits)-1 */   /* The max value a gene can take. */
#define maxgeneval 255

/* History of worst generation scores */
#define MaxWindow   5


/* Biggest population we will permit */
typedef int poprange;

/* Point on the chromosome for genes */
typedef int rglocus;

/* Range of solution space parameter indexs */
typedef int rgorganism;

/* range of gene ids */
typedef int geneidrange;

 /* Range of values for a gene. */
typedef int genevalrange;

typedef int bitrange;

typedef struct tpga {
	poprange popsize;       /* Population size */
	double mutrate;         /* Gene prob. mutation per generation */
	double invrate;         /* Prob of chrom. inversion per gen. */
	double prsave;          /* Proportion allowed to reproduce */
	int Window;             /* for fittness calculation */
	int costexmax;          /* Max number of cost executions */
	int r1,r2,r3;           /* Seeds for random number generator */
	int debug;
		/* 1 -> Print chrom when new best organism */
		/* 2 -> Print COSTINFO every generation    */
		/* 4 -> Print best chrom every gen.        */
		/* 8 -> Print gaga version number.         */
} tpGAparams;


typedef genevalrange *tporganism;

/*
 * These things are to do with the pascal origins of this program
 */
#define true -1
#define false 0

#define odd(i) (i & 0x01)

#define round(x) (int)rint((double)x)

#define assign(a, b, l)    bcopy((char *)a,(char *)b, l)

#define NO(t) (t)0

#define lose(p) 	free((char *)p)

extern char *calloc();

#define any(p,t)	if (p == NO(t)) { \
				(void)fprintf(stderr, "Null p\n"); \
				exit(-1); \
			} 

#ifndef sun
extern double rint();
#endif sun
