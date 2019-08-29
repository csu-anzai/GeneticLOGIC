
/*
 * bugworld.c
 *
 * Main BugWorld program.
 *
 * Contents:
 *	main ()
 *
 * Dependencies:
 *	sys.c
 *	pop.c
 *	land.c
 *	comp.c
 *	breed.c
 *
 * Author:
 *	Martyn Amos (martyn@dcs.warwick.ac.uk)
 *	Parallel Computation Group
 *	Department of Computer Science
 *	University of Warwick, UK
 */

#include <stdio.h>
#include "bugworld.h"


void main (int argc, char *argv[])
{
    int     c;
    int     errflag;
    int     terse;
    int     loop;
    int     gens;
    int     moves;
    int     direction;
    int     best_org;
    int     top_fitness;
    float   av_fitness;
    FILE * config_file;
    FILE * trace_file;
    config_struct * configuration;
    organism_struct * population;
    object_struct * landscape;

    /* 
     * Process command line options
     * Done like this for flexibility (someone might want to add
     * extra options later on)
     */

    errflag = 0;
    terse = FALSE;

    while ((c = getopt (argc, argv, "c:t")) != EOF)
    {
	switch (c)
	{
	    case 'c': 
		if (argv[2] == NULL)
		{
		    errflag++;
		}
		continue;

	    case 't': 
		terse = TRUE;
		continue;

	    case '?': 
		errflag++;
		break;
	 } /* end case */
    } /* end while */

    /*
     * If any problems, print usage message and quit
     */

    if (errflag || argc < C_ARGS)
    {
	bw_error (USG_MSG);
	exit (1);
    } /* end if */


    /* 
     * Open specified configuration file
     */

    config_file = open_config_file (argv[2]);

    /* 
     * Read configuration from specified file, validating it as
     * we go 
     */

    configuration = get_configuration (config_file);

    /* 
     * Open organism trace file
     */

    if (configuration -> trace != UNDEFINED)
    {
	trace_file = fopen ("trace", "w");
	fprintf (trace_file, "Trace of organism %d\n\n", configuration -> trace);
	fprintf (trace_file, "PC	Memory	Energy	Litter	Dir	x	y	Field1	Field2 Field3\n\n");
    } /* end if */

    /* 
     * Set random number generator seed
     */

    srandom (configuration -> seed);

    /* 
     * Create initial population
     */

    population = create_initial_population (configuration);

    /*
     * Print header information (if requested)
     */

    if (terse == FALSE)
    {
	printf ("\nBugWorld -- By Martyn Amos (");
	printf ("Dept. of Computer Science, University of Warwick)\n\n");
	printf ("Generations: %d\n", configuration -> generations);
	printf ("Population size: %d\n", configuration -> popsize);
	printf ("Random number seed: %d\n", configuration -> seed);
	printf ("Selection mechanism: ");

	switch (configuration -> selection)
	{
	    case PUREROULETTE: 
		printf ("pure roulette\n");
		break;

	    case RANDOM: 
		printf ("random\n");
		break;

	    case IMPUREROULETTE: 
		printf ("impure-roulette\n");
		break;

	    case TOURNAMENT: 
		printf ("tournament\n");
		break;
	 } /* end case */

	printf ("Mutation rate: %d%\n\n", configuration -> mutation_rate);
    } /* end if */

    for (gens = 0; gens < configuration -> generations; gens++)
    {

	/* 
	 * Initialise landscape
	 */

	landscape = initialise_landscape (configuration);

	for (moves = 0; moves < configuration -> moves; moves++)
	{
	    for (loop = 0; loop < configuration -> popsize; loop++)
	    {
		/* 
		 * Interpret next genetic instruction
		 */

		population = interpret_gene (loop, landscape, population, configuration, trace_file);

		/* 
		 * Deal with energy level
		 */

		if (population[loop].type == LITTERBUG)
		{
		    population = do_energy (loop, population);
		} /* end if */
	    } /* end for */
	} /* end for */

        /*
         * Calculate statistics 
         */

	best_org = 0;
	top_fitness = population[best_org].litter;
	av_fitness = 0;
	for (loop = 0; loop < configuration -> popsize; loop++)
	{
	    if (population[loop].type == LITTERBUG)
	    {
		av_fitness += population[loop].litter;
		if (population[loop].litter > top_fitness)
		{
		    best_org = loop;
		    top_fitness = population[loop].litter;
		} /* end if */
	    } /* end if */
	} /* end for */

	av_fitness = av_fitness / (configuration -> popsize / 2);

        /* 
         * Print stats
         */

	if (terse == FALSE)
	{
	    printf ("\nGENERATION : %d\n\n", gens);
	    printf ("Average fitness of population: ");
	} /* end if */

	printf ("%f\n", av_fitness);

        /*
         * Print genome of best performer (if required)
         */

	if (terse == FALSE)
	{
	    printf ("Best organism : %d, with a fitness of %d\n\n", best_org, top_fitness);
	    printf ("\nBest organism's genome: \n");
	    print_long_genome (population[best_org].genome, configuration);
	    printf ("\n");
	} /* end if */

        /*
         * Select next generation
         */

	population = selection (population, configuration);

    } /* end for */

    exit (0);

} /* end main */
