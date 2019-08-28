
/*
 * Bulk of types and Global variables for internal use
 * by GAGA, declared here
 */

typedef struct somechrome {
	genevalrange value;
	geneidrange  geneid;
} 			tpgene;
typedef tpgene 		*tpchromosome;
typedef tpchromosome 	*tppop;

typedef struct inf {
	poprange individual;
	double   cost;     /* From application, low => good */
	double   fittness; /* High => good */
} info;
typedef info 	*tppopcosts;

typedef int 	pinteger;


extern unsigned int norggenes; /* # parameters in solution space */
extern unsigned int nmetagenes; /* # genes used internally - unseen by appl.  */
extern unsigned int chromlength; /* organismsize + metagens */

poprange        popsize;

poprange        savenum;
pinteger        mutnum, invertnum, genmax;

double          BestCostSoFar;
double          MaxCostHistory[MaxWindow+1];
double          Cmax;

