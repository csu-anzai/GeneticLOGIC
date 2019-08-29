
/*
 * land.c
 *
 * Routines to deal with the virtual landscape.
 *
 * Contents:
 *	check_location ()
 *	look ()
 *	randomly_place_object ()
 *	initialise_landscape ()
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
#include "bugworld.h"


/*
 * Function:
 *	check_location ()
 *
 * Description:
 *	Examine landscape and return a code corresponding
 *	to any object/organism found at (x,y). If nothing is found,
 *	BLANK is returned
 */

int     check_location (long x, long y, int *what, object_struct * landscape, organism_struct * population, config_struct * config)
{
    int     loop;

    /* 
     * Check landscape
     */

    for (loop = 0; loop < (config -> food_amount + config -> litter_amount); loop++)
    {
	if ((landscape[loop].x_pos == x) &&
		(landscape[loop].y_pos == y) &&
		(landscape[loop].exists == TRUE))
	{
	    *what = loop;
	    if (landscape[loop].type == FOOD)
	    {
		return (FOOD);
	    }
	    else
	    {
		return (LITTER);
	    } /* end if */
	} /* end if */
    } /* end for */

    /* 
     * Check organisms
     */

    for (loop = 0; loop < config -> popsize; loop++)
    {
	if ((population[loop].x_pos == x) &&
		(population[loop].y_pos == y))
	{
	    *what = loop;
	    if (population[loop].type == LITTERBUG)
	    {
		return (LITTERBUG);
	    }
	    else
	    {
		return (BULLY);
	    } /* end if */
	} /* end if */
    } /* end for */

    return (BLANK);

} /* end check_location() */


/*
 * Function:
 *	look ()
 *
 * Description:
 * 	The specified organism "looks" in the current direction; a code
 *	corresponding to what can be seen is placed in the organism's memory
 */

organism_struct * look (int organism, object_struct * landscape, organism_struct * population, config_struct * config)
{
    int     loop;
    int     see;
    int     what;
    long    range;
    long    look_x;
    long    look_y;
    long    current_x;
    long    current_y;

    /*
     * Store current location and calculate first location to check
     */

    current_x = population[organism].x_pos;
    current_y = population[organism].y_pos;

    switch (population[organism].current_direction)
    {
	case NORTH: 
	    look_y = current_y - 1;
	    break;

	case SOUTH: 
	    look_y = current_y + 1;
	    break;

	case EAST: 
	    look_x = current_x + 1;
	    break;

	case WEST: 
	    look_x = current_x - 1;
	    break;
    } /* end case */

    /*
     * Work out appropriate range of vision
     */

    if (population[organism].type == LITTERBUG)
    {
	range = config -> bug_vision;
    }
    else
    {
	range = config -> bully_vision;
    } /* end if */

    /*
     * Look for "range" locations
     */

    for (loop = 0; loop < range; loop++)
    {

        /*
         * What's there?
         */

	see = check_location (look_x, look_y, &what, landscape, population, config);
	if (see != BLANK)
	{

            /*
             * Saw something - return its code
             */

	    population[organism].memory = see;
	    return (population);
	} /* end if */

        /*
         * Calculate next location to check
         */

	switch (population[organism].current_direction)
	{
	    case NORTH: 
		look_y--;
		continue;

	    case SOUTH: 
		look_y++;
		continue;

	    case EAST: 
		look_x++;
		continue;

	    case WEST: 
		look_x--;
		continue;
	} /* end case */
    } /* end for */

    /*
     * Saw nothing, so return blank
     */

    population[organism].memory = BLANK;
    return (population);

} /* end look() */


/*
 * Function:
 *      randomly_place_object ()
 *
 * Description:
 *      Places an object of type 'type' in the landscape, placed at a
 *      random point
 */

object_struct * randomly_place_object (int type, long ob_no, object_struct * landscape, config_struct * config)
{
    long    rand_x;
    long    rand_y;

    /*
     * Choose location
     */

    rand_x = random_number (config -> x_boundary);
    rand_y = random_number (config -> y_boundary);

    /* 
     * Update the landscape to reflect the new object
     */

    landscape[ob_no].x_pos = rand_x;
    landscape[ob_no].y_pos = rand_y;
    landscape[ob_no].type = type;
    landscape[ob_no].exists = TRUE;

    return (landscape);

} /* end randomly_place_object() */


/*
 * Function:
 *	initialise_landscape ()
 *
 * Description:
 *	Distributes food, litter and population around landscape
 */

object_struct * initialise_landscape (config_struct * config)
{
    object_struct * temp_l;
    int     loop;

    /* 
     * Allocate memory for landscape
     */

    temp_l = (object_struct *) malloc (sizeof (object_struct) * (config -> popsize + config -> food_amount + config -> litter_amount));

    if (temp_l == NULL)
    {
	bw_error (ERR_MALLOC);
    } /* end if */


    for (loop = 0; loop < config -> food_amount; loop++)
    {
	/* 
	 * Distribute food
	 */

	temp_l = randomly_place_object (FOOD, loop, temp_l, config);
    } /* end for */

    /* 
     * Distribute litter
     */

    for (; loop < config -> litter_amount; loop++)
    {
	temp_l = randomly_place_object (LITTER, loop, temp_l, config);
    } /* end for */

    return (temp_l);

} /* end initialise_landscape() */
