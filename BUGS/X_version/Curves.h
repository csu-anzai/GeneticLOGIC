/****************************************************************************/
/*	Curves.h															 	*/
/*	defines, typedefs, and externs needed by genetic algorithm				*/
/*	This file should contain almost all the parameters which are interesting*/
/*	to play with.  Do a 'make' after changing them, and only the breed.c	*/
/*	file should need to be recompiled.										*/
/****************************************************************************/

#define	PI				 3.14159265348979
#define	WEIGHT_BASE		 1.4	/* How much of curve we see					*/
#define ORG_T_MIN	     0.0	/* Length of organism						*/
#define	ORG_T_MAX		 (8*PI)
#define	ORG_X_MIN		-5.0	/* Coordinate range for organism window		*/
#define	ORG_X_MAX		 5.0
#define	ORG_Y_MIN		-5.0
#define	ORG_Y_MAX		 5.0
#define ORG_SEGMENTS	 (150.0)/* Number of segments used in drawing org	*/

#define	CHROM_WIND		 0.1	/* Fraction of org window used to draw genes*/
#define CHROM_OFFSET	 10		/* How much to offset bargraphs				*/

#define INIT_CHROM		 8		/* Initial chromosome size					*/

#define	pCROSS			 1.0	/* Probability of Crossover					*/
#define pMUTATION		 0.067	/* Probability of Mutation					*/
#define MUTATION_STD	 0.01	/* Controls size of mutations				*/

#define	INIT_SWITCH_DEF	 0		/* Whether switches start on or off			*/
#define	INIT_SHOW_GENES	 1		/* Whether gene window is initially on		*/
#define	INIT_PRINT_OUT	 0		/* Whether print out is initially enabled	*/

/* Not interesting to play with if breeder is running interactively			*/
#define INIT_FIT_THRESH	0.5		/* Default fitness threshold				*/
