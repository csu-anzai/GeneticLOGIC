
/*
 * breed.c
 *
 * Routines to breed population members
 *
 * Contents:
 *	crossover ()
 *	roulette ()
 *	selection ()
 *
 * Dependencies:
 *	sys.c
 *	pop.c
 *
 * Author:
 *	Martyn Amos (martyn@dcs.warwick.ac.uk)
 *	Parallel Computation Group
 *	Department of Computer Science
 * 	University of Warwick, UK
 */

#include <stdio.h>
#include "bugworld.h"

/*
 * Function:
 *	crossover ()
 *
 * Description:
 *	Performs crossover on two genomes specified, yielding a new
 *	genome.
 */

genome_struct * crossover (int father, int mother, organism_struct * population, config_struct * config)
{
    int     loop;
    int     crossover_point;
    genome_struct * temp_g;
    genome_struct * generated;
    genome_struct working;
    genome_struct * dad_g;
    genome_struct * mam_g;

    /*
     * Allocate memory for temporary gene 
     */

    temp_g = (genome_struct *) malloc (sizeof (genome_struct) * config -> genome_size);

    if (temp_g == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    /*
     * Copy genomes of parents
     */

    dad_g = population[father].genome;
    mam_g = population[mother].genome;

    /*
     * Choose an arbitrary crossover point
     */

    crossover_point = random_number (config -> genome_size);

    /*
     * Perform the crossover - father's genome
     */

    for (loop = 0; loop < crossover_point; loop++)
    {
        /*
         * Mutate gene?
         */

	if (random_number (CERTAINTY) < config -> mutation_rate)
	{
            /*
             * Yes
             */

	    generated = generate_gene (config);
	    temp_g[loop].field1 = generated -> field1;
	    temp_g[loop].field2 = generated -> field2;
	    temp_g[loop].field3 = generated -> field3;
	}
	else
	{

	    /* 
             * No mutation - just copy it
             */
          
	    working = dad_g[loop];
	    temp_g[loop].field1 = working.field1;
	    temp_g[loop].field2 = working.field2;
	    temp_g[loop].field3 = working.field3;
	} /* end if */
    } /* end for */

    /*
     * Perform the crossover - mother's genome
     */

    for (loop = crossover_point; loop < config -> genome_size; loop++)
    {

        /*
         * Mutate gene?
         */

	if (random_number (CERTAINTY) < config -> mutation_rate)
	{

	    /*
             * Yes
             */

	    generated = generate_gene (config);
	    temp_g[loop].field1 = generated -> field1;
	    temp_g[loop].field2 = generated -> field2;
	    temp_g[loop].field3 = generated -> field3;
	}
	else
	{

	    /*
             * No - just copy it
             */

	    working = mam_g[loop];
	    temp_g[loop].field1 = working.field1;
	    temp_g[loop].field2 = working.field2;
	    temp_g[loop].field3 = working.field3;
	} /* end if */
    } /* end for */

    return (temp_g);

} /* end crossover () */


/*
 * Function:
 *	roulette ()
 *
 * Description:
 *	Returns a code corresponding to the individual chosen by
 * 	Roulette Wheel
 */

int     roulette (organism_struct * population, config_struct * config)
{
    int     loop;
    int     bounce;
    int     roulette_hole;
    int     running_total;
    int     total_fitness;
    int     min_fitness;

    /*
     * Check for negative fitness(es)
     */

    min_fitness = population[0].litter;
    for (loop = 0; loop < config -> popsize; loop++)
    {
	if (population[loop].type == LITTERBUG)
	{
	    if (population[loop].litter < min_fitness)
	    {
		min_fitness = population[loop].litter;
	    } /* end if */
	} /* end if */
    } /* end for */

    /*
     * If any fitnesses negative, scale them all up so they are all >= 0
     */

    if (min_fitness < 0)
    {
	for (loop = 0; loop < config -> popsize; loop++)
	{
	    if (population[loop].type == LITTERBUG)
	    {
		population[loop].litter += (0 - min_fitness);
	    } /* end if */
	} /* end for */
    } /* end if */

    /* 
     * Calculate TOTAL fitness
     */

    total_fitness = 0;
    for (loop = 0; loop < config -> popsize; loop++)
    {
	if (population[loop].type == LITTERBUG)
	{
	    total_fitness += population[loop].litter;
	} /* end if */
    } /* end for */

    /*
     * Choose a "wedge" of the wheel to land in
     */

    bounce = 1;

    while ((bounce % 2 != 0) && bounce < config -> popsize)
    {
	bounce = random_number (config -> popsize);
    } /* end while */

    roulette_hole = random_number (total_fitness);
    running_total = 0;
    for (;;)
    {
	running_total += population[bounce].litter;

	if (running_total > roulette_hole)
	{
	    return (bounce);
	} /* end if */

	bounce++;
	bounce++;

	if (bounce > config -> popsize)
	{
	    bounce = 0;
	} /* end if */
    } /* end for */

} /* end roulette () */


/*
 * Function:
 *	selection ()
 *
 * Description:
 *	Performs selection on the given
 *	population, returning a pointer to a brand new population
 */

organism_struct * selection (organism_struct * population, config_struct * config)
{
    organism_struct * temp_pop;
    int     loop;
    int     father;
    int     mother;
    int     tourn_a;
    int     tourn_b;
    int     chosen;

    
    /*
     * Allocate memory for a temporary population
     */

    temp_pop = (organism_struct *) malloc (sizeof (organism_struct) * config -> popsize);
    if (temp_pop == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */

    /* 
     * Create new population
     */

    for (loop = 0; loop < config -> popsize; loop++)
    {

	if (loop % 2 == 0)
	{

	    /* 
	     * Organism is a LitterBug
	     */

	    /* 
	     * Pick the parent organisms
	     */

	    /*
             * RANDOM SELECTION
             */

	    father = 1;

	    while ((father % 2 != 0) && father < config -> popsize)
	    {
		father = random_number (config -> popsize);
	    } /* end while */

	    mother = 1;

	    while ((mother % 2 != 0) && mother < config -> popsize)
	    {
		mother = random_number (config -> popsize);
	    } /* end while */

	    /*
	     * IMPURE-ROULETTE SELECTION
             */

	    if (config -> selection == IMPUREROULETTE)
	    {
		father = roulette (population, config);
	    }
	    else

	    /*
             * PURE-ROULETTE SELECTION
             */

		if (config -> selection == PUREROULETTE)
		{
		    father = roulette (population, config);
		    mother = roulette (population, config);
		}
		else

		    /*
                     * TOURNAMENT SELECTION
                     */

		    if (config -> selection == TOURNAMENT)
		    {
			tourn_a = 1;
			while ((tourn_a % 2 != 0) && tourn_a <
				 config -> popsize)
			{
			    tourn_a = random_number (config -> popsize);
			} /* end while */

			tourn_b = 1;
			while ((tourn_b % 2 != 0) && tourn_b <
				 config -> popsize)
			{
			    tourn_b = random_number (config -> popsize);
			} /* end while */

			if((population[tourn_a].litter>population[tourn_b] .litter) && (random_number (CERTAINTY)<=config->fittest_chance))
			{
			    father = tourn_a;
			}
			else
			{
			    father = tourn_b;
			} /* end if */


			tourn_a = 1;
			while ((tourn_a % 2 != 0) && tourn_a < config -> popsize)
			{
			    tourn_a = random_number (config -> popsize);
			} /* end while */

			tourn_b = 1;
			while ((tourn_b % 2 != 0) && tourn_b < config -> popsize)
			{
			    tourn_b = random_number (config -> popsize);
			} /* end while */

			if ((population[tourn_a].litter > population[tourn_b].litter) && (random_number (CERTAINTY) <= config -> fittest_chance))
			{
			    mother = tourn_a;
			}
			else
			{
			    mother = tourn_b;
			} /* end if */
		    } /* end if */


	    temp_pop[loop].type = LITTERBUG;
	    temp_pop[loop].genome = crossover (father, mother, population, config);
	    temp_pop[loop].program_counter = 0;
	    temp_pop[loop].current_direction = random_number (4);
	    temp_pop[loop].x_pos = random_number (config -> x_boundary);
	    temp_pop[loop].y_pos = random_number (config -> y_boundary);
	    temp_pop[loop].memory = D_MEMORY;
	    temp_pop[loop].energy = D_ENERGY;
	    temp_pop[loop].resting = D_RESTING;
	    temp_pop[loop].litter = D_LITTER;
	}
	else
	{

	    /* 
	     * Organism is a Bully
	     */

	    temp_pop[loop].type = BULLY;
	    temp_pop[loop].genome = generate_bully_genome (config);
	    temp_pop[loop].program_counter = 0;
	    temp_pop[loop].current_direction = random_number (4);
	    temp_pop[loop].x_pos = random_number (config -> x_boundary);
	    temp_pop[loop].y_pos = random_number (config -> y_boundary);
	    temp_pop[loop].memory = D_MEMORY;
	} /* end if */
    } /* end for */

    return (temp_pop);
}
