
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
#include <stdio.h>
#include "gene.h"
#include "ga_random.h"

/***************************************************************************
 * FUNCTION: rand_sequence
 *
 * DESCRIBE: Randomly generates a random sequence of numbers from 
 *           0 to num_points-1
 *           (i.e. where each point is enumerated only once.)
 *
 *           Essentially, this routine fills an array with all possible
 *           numbers in the set and randomly chooses the 'next' number from
 *           this array.  When a number is chosen, the array is shortened
 *           and the procedure repeated.
 *
 * INPUT PARAMETERS: an empty tour, max number
 *
 * RETURN VALUE: none
 *
 * RESULT: fills input array
 ****************************************************************************/
void
rand_sequence( seq, num_points)
int      seq[];
int      num_points;
{
static int          first_time = 1;
static int         *temp;
int                 remainder;
int                 next, i;

if (first_time)
   {
    if (!(temp = (int *) malloc (num_points*sizeof(int))))
 	   fatal_error ("rand_sequence: malloc failure\n");
    first_time = 0;
   }
   
for(i = 0; i < num_points; i++)
   temp[i] = (int) i;

remainder = num_points - 1;

for(i = 0; i < num_points; i++)
   {
    next = randomain(remainder, 0);
    seq[i] = temp[next];
    temp[next] = temp[remainder];
    remainder--;
   }
} 

