
#if !defined(BW_HDR)
#define BW_HDR

/*
 * bugworld.h
 *
 * Main BugWorld header file
 *
 * Author:
 *	Martyn Amos (martyn@dcs.warwick.ac.uk)
 *	Parallel Computation Group
 *	Department of Computer Science
 *	University of Warwick, UK
 */

/*
 * Misc. definitions
 */

#define	C_ARGS		3
#define	UNDEFINED	-1
#define TRUE		0
#define	FALSE		1

/*
 * Selection mechanism identifying codes
 */

#define	PUREROULETTE	1
#define	RANDOM		2
#define IMPUREROULETTE	3
#define TOURNAMENT	4

/*
 * Error messages
 */

#define USG_MSG         "usage: bugworld -c configuration_file [-t]\n"
#define ERR_MALLOC	"memory allocation fault\n"
#define	ERR_CFG_FILE	"can't open configuration file\n"
#define	ERR_POP_FILE	"can't open initial population file\n"
#define	ERR_CFG_NAME 	"invalid configuration name\n"
#define	ERR_TOO_SMALL	"configuration value too small\n"
#define	ERR_TOO_BIG	"configuration value too big\n"
#define	ERR_NO_POPSIZE	"must specify a population size\n"
#define	ERR_NO_GEN	"must specify number of generations\n"
#define	ERR_POP_ODD	"population size must be an even number\n"

/*
 * Number of configuration names-1
 */

#define NO_CFG_ENTRIES	15

/*
 * Configuration name string/value string lengths
 */

#define	MAX_NAME_LEN	30
#define	MAX_VALUE_LEN	30

/*
 * Configuration name identification codes
 */

#define	POPSIZE		0
#define	GENERATIONS	1
#define	GENOMESIZE	2
#define	BUGVISION	3
#define	BULLYVISION	4
#define	SEED		5
#define	XBOUNDARY	6
#define	YBOUNDARY	7
#define	FOODAMOUNT	8
#define	LITTERAMOUNT	9
#define	SELECTION	10
#define	MUTATIONRATE	11
#define	MOVES		12
#define	FITTESTCHANCE	13
#define TRACE		14

/*
 * Configuration defaults
 */

#define	D_POPSIZE	UNDEFINED	
#define	D_GENERATIONS	UNDEFINED	
#define D_GENOMESIZE	15
#define	D_BUGVISION	5
#define	D_BULLYVISION	5
#define	D_SEED		getpid ()	
#define	D_XBOUNDARY	150
#define	D_YBOUNDARY	60
#define	D_FOODAMOUNT	300	
#define	D_LITTERAMOUNT	900
#define	D_SELECTION	IMPUREROULETTE
#define	D_MUTATIONRATE	1		/* ie. 1% */
#define	D_MOVES		3000	
#define	D_FITTESTCHANCE	75		/* ie. 75% */
#define D_TRACE		UNDEFINED

/*
 * Configuration record structure
 */

typedef struct ct
{
	long	popsize;
	char * 	initial_pop;
	long	generations;
	long	genome_size;
	long 	bug_vision;
	long 	bully_vision;
	long	seed;
	long	x_boundary;
	long	y_boundary;
	long	food_amount;
	long	litter_amount;
	long	food_dist;
	long	litter_dist;
	long	regeneration;
	long	selection;
	long	mutation_rate;
	long	elitist;
	long	unaltered;
	long	moves;
	long	fittest_chance;
	long	stat_gap;
	char * 	final_file;
	int	trace;
} config_struct;

/*
 * Landscape object definitions
 */

#define	FOOD		1
#define	LITTER		2
#define	LITTERBUG	3
#define	BULLY		4
#define	BLANK		5

/*
 * Compass point definitions
 */

#define	NORTH		0
#define	SOUTH		1
#define	EAST		2
#define	WEST		3

/*
 * Landscape object record structure
 */

typedef struct lo
{
	long	x_pos;
	long	y_pos;
	int	type;
	int	exists;
} object_struct;

/*
 * Genome record structure
 */

typedef struct gr
{
	int 	field1;
	int	field2;
	int	field3;
} genome_struct;


/*
 * Organism record default values
 */

#define D_MEMORY        0
#define D_ENERGY        200
#define D_RESTING      	0 
#define D_LITTER        0

/*
 * Organism record structure
 */

typedef struct or
{
	genome_struct *genome;
	int	program_counter;
	int	memory;
	int	energy;
	int	current_direction;
	int	resting;
	int	litter;
	int	type;
	long	x_pos;
	long	y_pos;
} organism_struct;

/*
 * Gene identifiers
 */

#define	LOOK		0
#define	MEMUP		1
#define	MEMDOWN		2
#define	RESTART		3
#define	MOVE		4
#define	TURN		5
#define	JUMP		6
#define	REMEMBER	7
#define	REST		8
#define	ENERGY		9
#define	LESS		10
#define	MORE		11
#define	EQUAL		12

/*
 * Maximum number of genes +1
 */

#define	MAX_GENE	13

/*
 * Fields per gene
 */

#define	GENE_FIELDS	3

/*
 * Size of standard Bully genome
 */

#define	B_GENOME_SIZE	7

/*
 * Maximum value for the x component of a gene (not including
 * MOVE and TURN, where the x component is a % value)
 */

#define	MAX_X_VALUE	7

/*
 * Certainty expressed as a % value (must add one because  
 * random_number (100) returns a value in the range 0..99)
 */

#define	CERTAINTY	101

/*
 * Function prototypes
 */

void bw_error (char *);
long random_number (long);
FILE *open_config_file (char *);
config_struct *get_configuration (FILE *);
genome_struct *generate_gene (config_struct *);
genome_struct *generate_bully_genome (config_struct *);
genome_struct *generate_genome (config_struct *);
organism_struct *create_initial_population (config_struct *);
int check_location (long, long, int *, object_struct *, organism_struct *, config_struct *);
organism_struct *look (int, object_struct *, organism_struct *,config_struct *);
object_struct *randomly_place_object (int, long, object_struct *, config_struct *);
object_struct *initialise_landscape (config_struct *);
organism_struct *move_organism (int, object_struct *, organism_struct *, config_struct *);
void print_genome (genome_struct *, config_struct *);
void print_long_genome (genome_struct *, config_struct *);
organism_struct *interpret_gene (int, object_struct *, organism_struct *, config_struct *, FILE *);
organism_struct *do_energy (int, organism_struct *);
genome_struct *crossover (int, int, organism_struct *, config_struct *);
organism_struct *selection (organism_struct *, config_struct *);

#endif
