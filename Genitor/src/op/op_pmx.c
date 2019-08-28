
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
 *	This is the PMX operator described by Goldberg & 	*
 * 	Lingle	(Proc Int'l Conf on GA's) and implemented by   	*
 *	Keith Mathias at Colorado State University.     	*
 *								*
 *								*
 *	To use this program include the following lines in 	*
 *	the main_tsp.c file:					*
 *								*
 *								*
 *	These calls should only happen once:			*
 *								*
 *	failed = get_array (Pool->string_length);		*
 *	from = get_array (Pool->string_length);			*
 *	indx = get_array (Pool->string_length);			*
 *	check_list = get_array (Pool->string_length);		*
 *								*
 *								*
 *	This code should be in a loop so it is called once	*
 *	for each recombination:					*
 *	if ( bitgen () )					*
 *	   pmx (mom->string, dad->string, child->string, 	*
 *  	           Pool->string_length, city_table);		*
 *      else							*
 *	   pmx (dad->string, mom->string, child->string, 	*
 *		   Pool->string_length, city_table);		*
 *								*
 *								*
 *								*
 *	This operator tries to preserve position 		*
 *	information during recombination.  A parent and 	*
 *	two crossover sites are selected randomly and the 	*
 *	cities between the two starting positions in the 	*
 *	selected parent are inherited by the child tour.  	*
 *	Each city between the two crossover points in the 	*
 *	unselected parent are mapped to the position held 	*
 *	by this city in the selected parent.  Then the 		*
 *	remaining cities are inherited from the unselected 	*
 *	parent.							*
 *								*
 *	Example- PMX 1:						*
 *	Parent 1: 	a b c d e f g h i j			*
 *	Cross Pts:	     *      *       (Parent 1 selected)	*
 *	Parent 2:	d i j h a g c e b f			*
 *								*
 *	Offspring:	h i c d e f j a b g			*
 *								*
 *	The cities in positions 3, 4, 5 and 6 are inherited 	*
 *	by the child tour from Parent 1.  Then beginning 	*
 *	with position 3, the city in P1 (c) is located in 	*
 *	P2 and (position 7) and this position in the 		*
 *	offspring is filled with the city in Parent 2 at 	*
 *	position 3:  Off[7] = P2[3].  Moving to position 4 in 	*
 *	Parent 1, we find a d and see that it occurs at 	*
 *	position 1 in Parent 2 so Off[1] = P2[4] = h, P1[5] 	*
 *	is city e which is in Parent 2 at position 8 so Off[8] 	*
 *	= P2[5] = a and f (P1[6]) is at P2[10] so Off[10] = 	*
 *	P2[6] = g.  The remaining cities are inherited from 	*
 *	P2: Off[2] = P2[2] = i and Off[9] = P2[9] = b.		*
 *								*
 *	Example- PMX 2:						*
 *	Parent 1: 	a b c d e f g h i j			*
 *	Parent 2:	c f a j h d i g b e			*
 *	Cross Pts:	    * *      *   *  (Parent 2 selected)	*
 *								*
 *	Offspring:	a j c d e f i g b h			*
 *								*
 *	In this example the mapping proceeds as above with 	*
 *	Off[3 - 6] = P1[3 - 6] and then Off[1] = P2[3] = a, 	*
 *	but then breaks down when attempting to map 		*
 *	P2[4] (j) to Off[6], since Off[6] has already been 	*
 *	filled.  City j is skipped over for the time being 	*
 *	and the h (P2[4]) maps to Off[10] then city d (P2[6]) 	*
 *	maps to Off[2] and even though this is a duplicate it 	*
 *	is left in the offspring temporarily.   Cities i, g 	*
 *	and b are then inherited from P2 leaving a tour with 	*
 *	no j city and two d cities.  The city d which is 	*
 *	outside the originally selected positions 3 through 6 	*
 *	is replaced with a j resulting in a complete and legal 	*
 *	tour.							*
 *								*
 *								*
 *								*
 ****************************************************************/


#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include "ga_random.h"
#include "gene.h"
#include "op_pmx.h"



/*****************************************************************
 *  FUNCTION: get_array 
 *
 *  DESCRIPTION: allocates space for integer arrays used by pmx
 *
 * INPUT PARAMETERS: string length
 *
 *
 * RETURN VALUE: the address of the array table
 *
 *****************************************************************/
extern int *
get_array (length)
int length;

{
    int *array;


    if (!(array = (int *) malloc ((length + 1)*sizeof (int))))
       printf ("get_array: Malloc failure. \n");

    return (array);

}




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



 
 
 
 
#define d_gene 1                 /* indicator for gene from dad */
#define m_gene 0                 /* indicator for gene from mom */
 
 
/***************************************************************************
 * FUNCTION: pmx
 *
 * DESCRIBE: This is the pmx operator which is a genetic operator
 *           designed for use with the Traveling Salesman Problem.
 *
 *           This operator is described in detail above.
 *
 * INPUT PARAMETERS: parent tours, child tour, tour length, 
 *		     pointers to 4 integer arrays used by 
 *		     operator.
 *
 * RETURN VALUE: none
 *
 * RESULT: recombines parent tours to create offspring tour.
 *
 ****************************************************************************/
 
void pmx(mom, dad, kid, length, failed, from, indx, check_list)
GENE_DATA mom[];
GENE_DATA dad[];
GENE_DATA kid[];
int 	  length;
int      *failed;
int      *from;
int      *indx;
int      *check_list;
 
{
  int left, right, temp, i, j, k;
  int mx_fail, done, found, mx_hold;

 

/* no mutation so start up the pmx replacement algorithm */
  for (k = 0; k < length; k++) {
    failed[k] = -1;
    from[k] = -1;
    check_list[k+1] = 0;
  }
 

  left = randomain(length-1, 0);              /* locate crossover points      */
  right = randomain(length-1, 0); 
  if (left > right) {
    temp = left;
    left = right;
    right = temp;
  }
 
  for (k = 0; k < length; k++)    {          /* copy P2 int offspring        */
    kid[k] = dad[k];
    from[k] = d_gene;
    check_list[dad[k]]++;
  }
 
  for (k = left; k <= right; k++) {
    check_list[kid[k]]--;
    kid[k] = mom[k];
    from[k] = m_gene;
    check_list[mom[k]]++;
  }

  mx_fail = 0;
  for (k = left; k <= right; k++)   {   /* for all elements in the P1-2 */
    if (mom[k] == dad[k]) 		/* segment find the match in P2 */
      found = 1;

    else	 {
      found = 0;                          /* and substitute element of  */
      j = 0;                                  /* P2-2 corresponding to elem*/

      while ((! found) && (j < length))   {   /* in P2-2 into the offspring */
        if ((kid[j] == mom[k])  && (from[j] == d_gene)) {
	  check_list[kid[j]]--;
          kid[j] = dad[k];
          found = 1;
	  check_list[dad[k]]++;
        }  /* end if */

        j++;

      }  /* end while */
    }   /* end else */

    if (! found)     {                     /* then gene was not replaced   */
      failed[mx_fail] = mom[k];
      indx[mx_fail] = k;
      mx_fail++;
    }   /* end if */

  }  /* end for */
 
  /* now to see if any genes could not be replaced */
  if (mx_fail > 0) {
    mx_hold = mx_fail;

    for (k = 0; k < mx_hold; k++) {
      found = 0;
      j = 0;

      while ((! found) && (j < length)) {

        if ((failed[k] == kid[j]) && (from[j] == d_gene)) {
	  check_list[kid[j]]--;
          kid[j] = dad[indx[k]];
	  check_list[dad[indx[k]]]++;
          found = 1;
          failed[k] = -1;
	  mx_fail--;
        }	/* end if    */

        j++;

      }		/* end while */
    }		/* end for   */
  }		/* end if    */
  
  for (k = 1; k <= length; k++) {

    if (check_list[k] > 1) {
      i = 0;

      while (i < length) {
        if ((kid[i] == k) && (from[i] == d_gene)) {
	  j = 1;

	  while (j <= length) {
	    if (check_list[j] == 0) {
	      kid[i] = j;
	      check_list[k]--;
	      check_list[j]++;
	      i = length + 1;
	      j = i;
	    } 	/* end if */

	    j++;
	  }	/* end while */
	}	/* end if    */

	i++;
      }  	/* end while */

    }		/*  end if   */
  }		/* end for   */
  

/* done = 0;
 k = 1;
 while ((k <= length) && (! done)) {

   if (check_list[k] > 1) {
     printf("\n\n");  
     for (i = 0; i < length; i++)
       printf("%d ", check_list[i]);

     printf("\n");
     for (i = 0; i < length; i++)
       printf("%d ", mom[i]);

     printf("\n");
     for (i = 0; i < length; i++)
       printf("%d ", dad[i]);

     printf("\n");
     for (i = 0; i < length; i++)
       printf("%d ", kid[i]);

     printf("\n");
    done = 1;
     printf("left: %d       right: %d\n", left, right);
     exit();
   }
   k++;

 } */

}
