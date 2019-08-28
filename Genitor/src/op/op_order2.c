
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1990                                       */
/*  Darrell L. Whitley                                       */
/*  Computer Science Department                              */
/*  Colorado State University                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/****************************************************************
 *								*
 *	This is the order operator developed by Syswerda (The 	*
 * 	Genetic Algorithms Handbook, ed L Davis) and 		*
 *	implemented by Susan McDaniel at Colorado State 	*
 *	University.    						*
 *								*
 *								*
 *	To use this program include the following lines in 	*
 *	the main_tsp.c file:					*
 *								*
 *								*
 *	This call should only happen once:			*
 *	get_city_table (Pool->string_length);			*
 *								*
 *								*
 *	This code should be in a loop so it is called once	*
 *	for each recombination:					*
 *	if ( bitgen () )					*
 *	   order (mom->string, dad->string, child->string, 	*
 *  	           Pool->string_length, city_table);		*
 *      else							*
 *	   order (dad->string, mom->string, child->string, 	*
 *		   Pool->string_length, city_table);		*
 *								*
 *								*
 *								*
 *	This operator differs from the above order based 	*
 *	operator in that several points are chosen randomly 	*
 *	and the order in which these cities appear in the 	*
 *	selected parent is imposed on the other parent.  	*
 *	Beginning with the initial city in the unselected 	*
 *	parent, if a city is in select list it is replaced 	*
 *	with the first city from this list.  If a city is 	*
 *	not in this list it is inherited from the unselected 	*
 *	parent from the current position.  This process 	*
 *	continues until the offspring tour is complete.		*
 *								*
 *	Example- alternate order cross:				*
 *	Parent 1: 	a b c d e f g h i j  select list:ajib	*
 *	Parent 2:	c f a j h d i g b e			*
 *	Cross Pts:	    * *      *   *  (Parent 2 selected)	*
 *								*
 *	Offspring:	a j c d e f g h i b			*
 *								*
 *	Positions 3, 4, 7, and 9 have been selected as has 	*
 *	Parent 2.  The cities in these positions are a, j, 	*
 *	i and b.  Beginning with P1[1] (a), we see that this 	*
 *	city is in the select list of cities.  Since it is 	*
 *	also the first city in the select list it is 		*
 *	inherited by the child tour: Off[1] = P1[1] = a.  	*
 *	P1[2] = b is also in the select list, but is not 	*
 *	the next city so it is replaced by the next city: 	*
 *	Off[2] = j.  P1[3] through P1[8] are not in the 	*
 *	select list and so are inherited by the child 		*
 *	tour in positions 3 through 8.  P1[9] (i) is the 	*
 *	next city in the select list so Off[8] = i and 		*
 *	finally Off[9] = b completing the tour.			*
 *								*
 *								*
 ****************************************************************/


#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "ga_random.h"
#include "gene.h"
#include "op_order2.h"

/*****************************************************************
 *  FUNCTION: get_city_table 
 *
 *  DESCRIPTION: allocates space for the city data table
 *		 the city data table contains the position
 *		 of each city in each parent and also 
 *		 has a field which is set when a city has
 *		 been included in the offspring tour.
 *
 * INPUT PARAMETERS: string length
 *
 *
 * RETURN VALUE: the address of the city table
 *
 *****************************************************************/
CITY_DATA *
get_city_table (length)
int length;

{
    CITY_DATA *city_table;

    /*  malloc one extra location so that cities 1-N can be accessed
	directly.  location 0 will not be used  */

    if (!(city_table = (CITY_DATA *) malloc ((length + 1)*sizeof (CITY_DATA))))
       printf ("get_city_table: Malloc failure. \n");

    return (city_table);

}


/**********************************************************************/
/*            Syswerda's ORDERCROSS CROSSOVER OPERATOR                 */
/***********************************************************************/
void order(mom, dad, kid, length, city_table)
    GENE_DATA mom[];
    GENE_DATA dad[];
    GENE_DATA kid[];
    int	     length;
    CITY_DATA *city_table;
{
   int k, j, count, pos, select, num_positions;

   for (k = 1; k <= length; k++)  {       /* initalize city_table */
     city_table[k].used = 0;
     city_table[k-1].select_list = -1;
   }

   /* randomly determine the number of positions to be inherited from
      mother tour.  */
   num_positions = randomain (2*length/3, length/3);   

   for (k=0; k<num_positions; k++)	{  /* make a list of selected cities */
      pos = randomain (length - 1, 0);
      city_table[pos].select_list = mom[pos];
      city_table[mom[pos]].used = 1;	   /* mark these cities as used */
   }


   count = 0;
   k = 0;

   /* consolidate the select list to adjacent positions */
   while (count < num_positions) {
      if (city_table[k].select_list == -1)	  {
	 j = k + 1;
	 while ((city_table[j].select_list == -1) && (j < length))
	    j++;

	 city_table[k].select_list = city_table[j].select_list;
	 city_table[j].select_list = -1;
	 count ++;
      }
      else
	 count ++;
      k++;
   }

   select = 0;

   for (k=0; k<length; k++)	{	
      if (city_table[dad[k]].used)   {	/* if city in dad has been used then */
	 kid[k] = city_table[select].select_list;	/* child inherits */
	 select ++;			/* next city in  the select list   */
      }
      else	/* city isn't used yet, so inherit from dad */
	 kid[k] = dad[k];
   }


}
/* end of ordercross */

/***************************************************************************
 * FUNCTION: maketour
 *
 * DESCRIBE: Randomly generates a legal "traveling salesman" tour 
 *           (i.e. where each point is visited only once.)
 *
 *           Essentially, this routine fills an array with all possible
 *           points on the tour and randomly chooses the 'next' city from
 *           this array.  When a city is chosen, the array is shortened
 *           and the procedure repeated.
 *
 * INPUT PARAMETERS: an empty tour, number of points per tour
 *
 * RETURN VALUE: none
 *
 * RESULT: fills input tour
 ****************************************************************************/
void
maketour(gene, num_points)
GENE_DATA gene[];
int      num_points;
{
static int          first_time = 1;
static GENE_DATAPTR temp;
int                 remainder;
int                 next, i;

if (first_time)
   {
    if (!(temp = (GENE_DATAPTR) malloc (num_points*sizeof(GENE_DATA))))
 	   fatal_error ("maketour: malloc failure\n");
    first_time = 0;
   }
   
for(i = 0; i < num_points; i++)
   temp[i] = (GENE_DATA) i+1;

remainder = num_points - 1;

for(i = 0; i < num_points; i++)
   {
    next = randomain(remainder, 0);
    gene[i] = temp[next];
    temp[next] = temp[remainder];
    remainder--;
   }
} 


