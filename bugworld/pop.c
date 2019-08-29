
/*
 * pop.c
 *
 * Routines to create the population members
 *
 * Contents:
 *	generate_gene ()
 *	generate_bully_genome ()
 *	generate_genome ()
 *	create_initial_population ()
 *
 * Dependencies:
 *	sys.c
 *
 * Author:
 *	Martyn Amos (martyn@dcs.warwick.ac.uk)
 *	Parallel Computation Group
 *	Department of Computer Science
 *	University of Warwick, UK
 */

#include <stdio.h>
#include "bugworld.h"


/*
 * Function:
 *	generate_gene ()
 *
 * Description:
 *	Generate a pseudo-random gene
 */

genome_struct * generate_gene (config_struct * config)
{
    genome_struct * temp_g;
    int     operator;

    /* 
     * Allocate memory for gene
     */

    temp_g = (genome_struct *) malloc (sizeof (genome_struct) * config -> genome_size);

    if (temp_g == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    /* 
     * Generate operator
     */

    operator = random_number (MAX_GENE);
    temp_g -> field1 = operator;

    /* 
     * Generate y value (always a gene index)
     */

    temp_g -> field3 = random_number (config -> genome_size);

    /* 
     * Generate x value
     */

    if ((operator == MOVE) || (operator == TURN))

	/* 
	 * Need a % chance
	 */

    {
	temp_g -> field2 = random_number (CERTAINTY);
    }
    else
	if (operator >= REMEMBER)

	    /* 
	     * Need an x value
	     */

	{
	    temp_g -> field2 = random_number (MAX_X_VALUE);
	} /* end if */

    return (temp_g);

} /* end generate_gene() */


/*
 * Function:
 *	generate_bully_genome ()
 *
 * Description:
 *	Generate the standard Bully genome
 */

genome_struct * generate_bully_genome (config_struct * config)
{

    /*
     * Standard Bully genome definition
     */

    static long bully_genome[GENE_FIELDS * B_GENOME_SIZE] =
    {
	LOOK, 0, 0,		/* LOOK */
	EQUAL, 3, 5,		/* EQUAL 3 5 */
	TURN, 75, 0,		/* TURN 75% */
	MOVE, 75, 0,		/* MOVE 75% */
	RESTART, 0, 0,		/* RESTART */
	MOVE, 100, 0,		/* MOVE 100% */
	RESTART, 0, 0		/* RESTART */
    };
    genome_struct * temp_g;
    int     loop;
    int     gene;

    /* 
     * Allocate memory for genes
     */

    temp_g = (genome_struct *) malloc (sizeof (genome_struct) * config -> genome_size);

    if (temp_g == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    /* 
     * Copy standard Bully genome in
     */

    loop = 0;
    for (gene = 0; gene < B_GENOME_SIZE; gene++)
    {
	temp_g[gene].field1 = bully_genome[loop];
	loop++;
	temp_g[gene].field2 = bully_genome[loop];
	loop++;
	temp_g[gene].field3 = bully_genome[loop];
	loop++;
    } /* end for */

    return (temp_g);

} /* end copy_bully_genome() */


/*
 * Function:
 *	generate_genome ()
 *
 * Description:
 * 	Generate a pseudo-random genome 
 */

genome_struct * generate_genome (config_struct * config)
{
    genome_struct * temp_g;
    genome_struct * generated;
    int     loop;

    /* 
     * Allocate memory for genes
     */

    temp_g = (genome_struct *) malloc (sizeof (genome_struct) * config -> genome_size);

    if (temp_g == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    for (loop = 0; loop < config -> genome_size; loop++)
    {

	/* 
	 * Initialise each gene
	 */

	generated = generate_gene (config);
	temp_g[loop].field1 = generated -> field1;
	temp_g[loop].field2 = generated -> field2;
	temp_g[loop].field3 = generated -> field3;
    } /* end for */

    free (generated);

    return (temp_g);

} /* end generate_genome() */


/*
 * Function:
 *	create_initial_population ()
 *
 * Description:
 *	Create initial population of LitterBugs and Bullies
 */

organism_struct * create_initial_population (config_struct * config)
{
    FILE * initial_pop_fp;
    organism_struct * temp_i;
    int     loop;
    int     test;

    /*
     * Allocate memory for population
     */
    
    temp_i = (organism_struct *) malloc (sizeof (organism_struct) * config -> popsize);

    if (temp_i == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    /* 
     * Create one record for each individual
     */

    for (loop = 0; loop < config -> popsize; loop++)
    {
	/* 
	 * Generate genome and initialise misc stuff
	 */

	if (loop % 2 == 0)
	{

	    /* 
	     * Organism is a LitterBug
	     */

	    temp_i[loop].type = LITTERBUG;
	    temp_i[loop].genome = generate_genome (config);
	    temp_i[loop].program_counter = 0;
	    temp_i[loop].current_direction = random_number (4);
	    temp_i[loop].x_pos = random_number (config -> x_boundary);
	    temp_i[loop].y_pos = random_number (config -> y_boundary);
	    temp_i[loop].memory = D_MEMORY;
	    temp_i[loop].energy = D_ENERGY;
	    temp_i[loop].resting = D_RESTING;
	    temp_i[loop].litter = D_LITTER;
	}
	else
	{

	    /* 
	     * Organism is a Bully
	     */

	    temp_i[loop].type = BULLY;
	    temp_i[loop].genome = generate_bully_genome (config);
	    temp_i[loop].program_counter = 0;
	    temp_i[loop].current_direction = random_number (4);
	    temp_i[loop].x_pos = random_number (config -> x_boundary);
	    temp_i[loop].y_pos = random_number (config -> y_boundary);
	    temp_i[loop].memory = D_MEMORY;
	} /* end if */
    } /* end for */

    return (temp_i);

} /* end create_initial_population () */
