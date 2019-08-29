
/*
 * comp.c
 *
 * BugWorld virtual computer implementation routines.
 *
 * Contents:
 * 	move_organism ()
 *	turn_organism ()
 *	interpret_gene ()
 *	do_energy ()
 *
 * Dependencies:
 *	sys.c
 *	land.c
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
 *	move_organism ()
 *
 * Description:
 *	Moves specified organism
 */

organism_struct * move_organism (int loop, object_struct * landscape, organism_struct * population, config_struct * config)
{
    int     direction;
    int     under;
    int     there;

    /*
     * Exit if the current organism's a LitterBug and it's resting
     */

    if (population[loop].type == LITTERBUG && (population[loop].energy == 0 || population[loop].resting > 0))
    {
	return (population);
    } /* end if */

    /*
     * Update organism's coordinates 
     */

    direction = population[loop].current_direction;
    switch (direction)
    {
	case EAST: 
	    population[loop].x_pos++;
	    break;

	case WEST: 
	    population[loop].x_pos--;
	    break;

	case SOUTH: 
	    population[loop].y_pos++;
	    break;

	case NORTH: 
	    population[loop].y_pos--;
	    break;
    } /* end case */

    /*
     * Check if organism moved over a boundary - take action if it did
     */

    if (population[loop].x_pos < 0)
    {
	population[loop].x_pos = config -> x_boundary;
    } /* end if */

    if (population[loop].y_pos < 0)
    {
	population[loop].y_pos = config -> y_boundary;
    } /* end if */

    if (population[loop].x_pos > config -> x_boundary)
    {
	population[loop].x_pos = 0;
    } /* end if */

    if (population[loop].y_pos > config -> y_boundary)
    {
	population[loop].y_pos = 0;
    } /* end if */

    /* 
     * Check what's currently there
     */

    under = check_location (population[loop].x_pos, population[loop].y_pos, &there, landscape, population, config);

    if (population[loop].type == LITTERBUG)
    {
	if (under == FOOD)
	{

            /*
             * Eat food
             */

	    population[loop].energy += 5;
	    landscape[there].exists = FALSE;
	}
	else
	    if (under == LITTER)
	    {

                /*
                 * Collect litter
                 */

		population[loop].litter++;
		landscape[there].exists = FALSE;
	    }
    }
    else
    {
	if (under == LITTERBUG)
	{

	    /*
             * Steal litter 
             */

	    if (random_number (CERTAINTY) <= 20)
	    {
		population[there].litter--;
	    } /* end if */
	} /* end if */
    } /* end if */

    return (population);

} /* end move_organism () */


/*
 * Function:
 *	turn_organism ()
 *
 * Description:
 *	Turns indexed organism
 */

organism_struct * turn_organism (int loop, organism_struct * population, config_struct * config)
{
    population[loop].current_direction = random_number (4);

    return (population);

} /* end turn_organism () */


/*
 * Function:
 *	interpret_gene ()
 *
 * Description:
 * 	Interpret gene pointed to by population [loop].program_counter, and take
 *	appropriate action
 */

organism_struct * interpret_gene (int loop, object_struct * landscape, organism_struct * population, config_struct * config, FILE * trace)
{
    genome_struct * temp_g;
    genome_struct temp_g2;
    int     pc;
    int     op;
    int     arg1;
    int     arg2;

    pc = population[loop].program_counter;
    temp_g = population[loop].genome;
    temp_g2 = temp_g[pc];
    op = temp_g2.field1;
    arg1 = temp_g2.field2;
    arg2 = temp_g2.field3;


    /*
     * Print trace info to file
     */

    if ((loop == config -> trace) && (config -> trace != UNDEFINED))
    {
	if (population[loop].resting > 0 && population[loop].type == LITTERBUG)
	{
	    fprintf (trace, "(Resting)\n");
	    return (population);
	 } /* end if */

	fprintf (trace, "%d\t%d\t%d\t%d\t%d\t", pc, population[loop].memory,
		population[loop].energy,
		population[loop].litter,
		population[loop].current_direction);
	fprintf (trace, "%d\t%d\t", population[loop].x_pos,
		population[loop].y_pos);
	fprintf (trace, "%d\t%d\t%d\n", op, arg1, arg2);
    } /* end if */

    switch (op)
    {
	case MOVE: 
	    if (random_number (CERTAINTY) <= arg1)
	    {
		population = move_organism (loop, landscape,
			population, config);
	    } /* end if */
	    break;

	case TURN: 
	    if (random_number (CERTAINTY) <= arg1)
	    {
		population = turn_organism (loop, population,
			config);
	    } /* end if */
	    break;

	case LOOK: 
	    population = look (loop, landscape, population, config);
	    break;

	case RESTART: 
	    population[loop].program_counter = 0;
	    return (population);

	case REST: 
	    /* Safe if resting -- can't be attacked */
	    population[loop].energy = 0;
	    population[loop].resting = arg1;
	    break;

	case MEMUP: 
	    if (++population[loop].memory > MAX_X_VALUE)
	    {
		population[loop].memory = 0;
	    } /* end if */
	    break;

	case MEMDOWN: 
	    if (--population[loop].memory < 0)
	    {
		population[loop].memory = MAX_X_VALUE;
	    } /* end if */
	    break;

	case REMEMBER: 
	    population[loop].memory = arg1;
	    break;

	case JUMP: 
	    population[loop].program_counter = arg1;
	    return (population);

	case LESS: 
	    if (population[loop].memory < arg1)
	    {
		population[loop].program_counter = arg2;
		return (population);
	    }
	    else
	    {
		break;
	    } /* end if */

	case MORE: 
	    if (population[loop].memory > arg1)
	    {
		population[loop].program_counter = arg2;
		return (population);
	    }
	    else
	    {
		break;
	    } /* end if */

	case EQUAL: 
	    if (population[loop].memory == arg1)
	    {
		population[loop].program_counter = arg2;
		return (population);
	    }
	    else
	    {
		break;
	    } /* end if */

	case ENERGY: 
	    if (population[loop].energy < arg1)
	    {
		population[loop].program_counter = arg2;
		return (population);
	    }
	    else
	    {
		break;
	    } /* end if */

    } /* end case */

    /*
     * Increment program counter, wrapping round
     */

    population[loop].program_counter++;

    if (population[loop].type == LITTERBUG)
    {
	if (population[loop].program_counter == config -> genome_size)
	{
	    population[loop].program_counter = 0;
        } /* end if */
    }
    else
    {
	if (population[loop].program_counter == B_GENOME_SIZE)
	{
	    population[loop].program_counter = 0;
	} /* end if */
    } /* end if */

    return (population);

} /* end interpret_gene () */


/*
 * Function:
 *	do_energy ()
 *
 * Description:
 *	Deal with LitterBug's energy level
 */

organism_struct * do_energy (int loop, organism_struct * population)
{

    if (population[loop].energy <= 0)
    {
	population[loop].resting = (population[loop].resting) - 1;

	if (population[loop].resting == 0)
	{
	    population[loop].energy = D_ENERGY;
	    return (population);
	} /* end if */
    }
    else
    {
	population[loop].energy = population[loop].energy - 1;
    } /* end if */

    if (population[loop].energy == 0 && population[loop].resting == 0)
    {
	population[loop].resting = 10;
    } /* end if */

    return (population);

} /* end do_energy() */
