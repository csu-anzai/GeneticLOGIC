/***************************************************************************/
/*	GA.h																   */
/*	defines, typedefs, and externs needed by genetic algorithm			   */
/***************************************************************************/

#define MAX_CHROM_SIZE	30		/* A chromosome can have up to 30 genes 	*/
#define MAX_POP			40		/* Population can be up to 40				*/
#define	INIT_POP		(ORG_X*ORG_Y)			/* Initial population size	*/
#define ORG_X			4		/* # orgs horizontally						*/
#define ORG_Y			4		/* # orgs vertically						*/


typedef	double		Gene;

typedef	struct org {
	int		name;					/* Keep track of names, just for fun	*/
	int		size_chrom;				/* Actual # genes in this chromosome	*/
	Gene	X_Chrom[MAX_CHROM_SIZE];/* The genetic material itself			*/
	Gene	Y_Chrom[MAX_CHROM_SIZE];
	double	fitness;				/* Fitness of organism					*/
	int		mom;					/* Keep track of parents just for fun	*/
	int		dad;
	} Organism;


typedef	Organism	Population;



extern	Population	G_Population[MAX_POP];	/* The population				*/
extern	Population	G_Kids_Pop[MAX_POP];	/* The next generation			*/
extern	int			G_size_pop;				/* Actual size of population	*/
extern	int			G_size_breeding_pop;	/* Size of breeding sub-pop		*/
extern	double		G_fit_thresh;			/* Actual fitness threshold		*/
extern	double		G_pCross;				/* Probability of crossover		*/
extern	double		G_pMutation;			/* Probability of mutation		*/
extern	double		G_mutation_std;			/* Size of mutations			*/
extern	double		G_weight[MAX_CHROM_SIZE];	/* How imp. each term is	*/
extern	double		G_sum_weights;			/* Used in scaling				*/
extern	int			G_generation;			/* Generation #					*/

extern	double		drand48();
extern	double		Fitness();
extern	Grow();
