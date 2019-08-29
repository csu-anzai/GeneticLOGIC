
/*
 * sys.c
 *
 * BugWorld "system" routines
 *
 * Contents:
 *	bw_error ()
 *	random_number ()
 *	open_config_file ()
 *	get_configuration ()
 *	print_genome ()
 * 	print_long_genome ()
 *
 * Dependencies:
 *	None
 *
 * Author:
 *	Martyn Amos (martyn@dcs.warwick.ac.uk)
 *	Parallel Computation Group
 *	Department of Computer Science
 *	University of Warwick, UK
 */

#include <stdio.h>
#include <fcntl.h>
#include "bugworld.h"

/*
 * Function:
 *	bw_error ()
 *
 * Description:
 *	Print the specified error message and exit
 */


void bw_error (char *err_msg)
{
    fprintf (stderr, "%s%s", "bugworld: ", err_msg);
    exit (1);
} /* end bw_error () */


/*
 * Function:
 *	random_number ()
 *
 * Description:
 *	Returns a random number in the range 0..(modulus-1)
 */

long    random_number (long modulus)
{
            return (random () % modulus);
} /* end random_number () */


/*
 * Function:
 *      open_config_file ()
 *
 * Description:
 *      Open specified configuration file, returning a pointer to it
 */

FILE * open_config_file (char *config_filename)
{
    FILE * temp_fp;

    /* 
     * Open file, exiting if there's a problem
     */

    temp_fp = fopen (config_filename, "r");
    if (temp_fp == NULL)
    {
	bw_error (ERR_CFG_FILE);
    } /* end if */

    return (temp_fp);

} /* end open_config_file () */


/*
 * Function:
 *	get_configuration ()
 *
 * Description:
 *	Read BugWorld configuration from specified file, validating it
 *	as it goes
 */

config_struct * get_configuration (FILE * config_file)
{
    config_struct * temp_config;
    char    name[MAX_NAME_LEN];
    char    char_value[MAX_VALUE_LEN];
    char    value_copy[MAX_VALUE_LEN];
    long    value;
    int     loop;
    int     found_name;
    int     min_ok;
    int     max_ok;

    /* 
     * Valid configuration names, min. and max. values
     * -1 means that there isn't a need for a min. & max. value
     * eg. for a filename
     */

    static char *config_names[] =
    {
	"POPSIZE", "GENERATIONS", "GENOMESIZE",
	"BUGVISION", "BULLYVISION", "SEED", "XBOUNDARY", "YBOUNDARY",
	"FOODAMOUNT", "LITTERAMOUNT", "SELECTION", "MUTATIONRATE",
	"MOVES", "FITTESTCHANCE", "TRACE"
    };

    static int  min_cfg_values[NO_CFG_ENTRIES] =
    {
	2, 1, 7, 1, 1, -1, 50, 20, 100, 100, 1, 0, 1500, 0, -1
    };

    static long max_cfg_values[NO_CFG_ENTRIES] =
    {
	200, 1000, 25, 20, 20, -1, 500, 500, 5000, 5000, 4, 100,
	10000, 100, -1
    };

    /* 
     * Allocate space for configuration, checking for faults
     */

    temp_config = (config_struct *) malloc (sizeof (config_struct));
    if (temp_config == NULL)
    {
	bw_error (ERR_MALLOC);
    }

    /* 
     * Initialise configuration record
     */

    temp_config -> popsize = D_POPSIZE;
    temp_config -> generations = D_GENERATIONS;
    temp_config -> genome_size = D_GENOMESIZE;
    temp_config -> bug_vision = D_BUGVISION;
    temp_config -> bully_vision = D_BULLYVISION;
    temp_config -> seed = D_SEED;
    temp_config -> x_boundary = D_XBOUNDARY;
    temp_config -> y_boundary = D_YBOUNDARY;
    temp_config -> food_amount = D_FOODAMOUNT;
    temp_config -> litter_amount = D_LITTERAMOUNT;
    temp_config -> selection = D_SELECTION;
    temp_config -> mutation_rate = D_MUTATIONRATE;
    temp_config -> moves = D_MOVES;
    temp_config -> fittest_chance = D_FITTESTCHANCE;
    temp_config -> trace = D_TRACE;

    /* 
     * Read 'em in, validating "on the fly"
     */

    while (fscanf (config_file, "%s%s", &name, &char_value) != EOF)
    {
	/* 
	 * Convert the string read into a long
	 */

	strcpy (value_copy, char_value);
	value = atol (char_value);

	found_name = -1;
	min_ok = 0;
	max_ok = 0;

	for (loop = 0; loop < NO_CFG_ENTRIES; loop++)
	{
	    /* 
	     * Is the name valid?
	     */

	    if (!strcmp (config_names[loop], name))
	    {
		found_name = loop;
	    } /* end if */

	} /* end for */

	/* 
	 * Does its value fall within the legal 
	 * limits?
	 */

	if ((min_cfg_values[found_name] != -1) && (max_cfg_values[found_name] != -1))

	    /* 
	     * Numeric value relevant
	     */

	{
	    if (value >= min_cfg_values[found_name])
	    {
		min_ok++;
	    } /* end if */

	    if (value <= max_cfg_values[found_name])
	    {
		max_ok++;
	    }
	}
	else

	    /* 
	     * Value irrelevant
	     */

	{
	    min_ok++;
	    max_ok++;
	} /* end if */

	/* 
	 * Something was wrong with the entry, so print
	 * its name out as an aid to the user
	 */

	if ((found_name == -1) || (!min_ok) || (!max_ok))
	{
	    fprintf (stderr, "%s\n", name);
	} /* end if */

	/* 
	 * Can't find name supplied in valid list 
	 */

	if (found_name == -1)
	{
	    bw_error (ERR_CFG_NAME);
	} /* end if */

	/* 
	 * Its value is too small
	 */

	if (!min_ok)
	{
	    bw_error (ERR_TOO_SMALL);
	} /* end if */

	/* 
	 * Its value is too big
	 */

	if (!max_ok)
	{
	    bw_error (ERR_TOO_BIG);
	} /* end if */

	/* 
	 * Configuration ok, so put it in the configuration
	 * record
	 */

	switch (found_name)
	{
	    case POPSIZE: 
		temp_config -> popsize = value;
		continue;

	    case GENERATIONS: 
		temp_config -> generations = value;
		continue;

	    case GENOMESIZE: 
		temp_config -> genome_size = value;
		continue;

	    case BUGVISION: 
		temp_config -> bug_vision = value;

	    case BULLYVISION: 
		temp_config -> bully_vision = value;
		continue;

	    case SEED: 
		temp_config -> seed = value;
		continue;

	    case XBOUNDARY: 
		temp_config -> x_boundary = value;
		continue;

	    case YBOUNDARY: 
		temp_config -> y_boundary = value;
		continue;

	    case FOODAMOUNT: 
		temp_config -> food_amount = value;
		continue;

	    case LITTERAMOUNT: 
		temp_config -> litter_amount = value;
		continue;

	    case SELECTION: 
		temp_config -> selection = value;
		continue;

	    case MUTATIONRATE: 
		temp_config -> mutation_rate = value;
		continue;

	    case MOVES: 
		temp_config -> moves = value;
		continue;

	    case FITTESTCHANCE: 
		temp_config -> fittest_chance = value;
		continue;

	    case TRACE: 
		temp_config -> trace = value;
		continue;
	} /* end case */
    } /* end while */


    /* Check that mandatory configurations are set */

    if (temp_config -> popsize == UNDEFINED)
    {
	bw_error (ERR_NO_POPSIZE);
    }
    else
	if (temp_config -> generations == UNDEFINED)
	{
	    bw_error (ERR_NO_GEN);
	} /* end if */

    /* Check that popsize is an even number */

    if ((temp_config -> popsize % 2) != 0)
    {
	bw_error (ERR_POP_ODD);
    } /* end if */

    fclose (config_file);
    return (temp_config);

} /* end get_configuration () */


/*
 * Function:
 *	print_genome ()
 *
 * Description:
 *	Prints out the supplied genome to the screen
 */

void print_genome (genome_struct * genome, config_struct * config)
{
    int     loop;
    genome_struct temp_g;

    for (loop = 0; loop < config -> genome_size; loop++)
    {
	temp_g = genome[loop];
	printf ("%d %d %d\n", temp_g.field1, temp_g.field2, temp_g.field3);
    } /* end for */

    printf ("\n");

} /* end print_genome () */


/*
 * Function:
 *	print_long_genome ()
 * 
 * Description:
 *	Prints out a more informative version of the supplied genome
 */

void print_long_genome (genome_struct * genome, config_struct * config)
{
    int     loop;
    genome_struct temp_g;

    for (loop = 0; loop < config -> genome_size; loop++)
    {
	printf ("%d\t", loop);

	temp_g = genome[loop];
	switch (temp_g.field1)
	{
	    case LOOK: 
		printf ("LOOK");
		break;

	    case MEMUP: 
		printf ("MEMUP");
		break;

	    case MEMDOWN: 
		printf ("MEMDOWN");
		break;

	    case RESTART: 
		printf ("RESTART");
		break;

	    case MOVE: 
		printf ("MOVE");
		break;

	    case TURN: 
		printf ("TURN");
		break;

	    case JUMP: 
		printf ("JUMP");
		break;

	    case REMEMBER: 
		printf ("REMEMBER");
		break;

	    case REST: 
		printf ("REST");
		break;

	    case ENERGY: 
		printf ("ENERGY");
		break;

	    case LESS: 
		printf ("LESS");
		break;

	    case MORE: 
		printf ("MORE");
		break;

	    case EQUAL: 
		printf ("EQUAL");
		break;
	} /* end case */

	printf (" ");

	if (temp_g.field1 > RESTART && temp_g.field1 < ENERGY)
	{
	    printf ("%d\n", temp_g.field2);
	}
	else
	    if (temp_g.field1 > REST)
	    {
		printf ("%d %d\n", temp_g.field2, temp_g.field3);
	    }
	    else
	    {
		printf ("\n");
	    } /* end if */
    } /* end for */

} /* end print_long_genome () */
