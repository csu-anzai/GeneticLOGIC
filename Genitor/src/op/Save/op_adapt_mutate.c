#include <stdio.h>
#include "gene.h"
#include "ga_random.h"


/**************************************************************************
 * FUNCTION: get_mutate_level
 *
 * DESCRIBE: determines the appropriate level of mutation for a child
 *           according to the hamming distance between its two parents;
 *           the less the difference, the higher the mutation rate.
 *
 * INPUT PARAMETERS: number of positions which differ between parents;
 *                   length of parents;
 *                   maximum mutation rate;
 *
 * RETURN VALUE: mutation level between 0 and maximum mutation rate
 **************************************************************************/

float
get_mutate_level (numdiff, length, mutate_rate)
int              numdiff;
int              length;
float            mutate_rate;
{
 if (numdiff <= 1) 
    return (mutate_rate);
 else 
    return (mutate_rate / ((numdiff / (float) length) * 100.0));
}



/**************************************************************************
 * FUNCTION: adaptive_mutate
 *
 * DESCRIBE: Mutates the input string (a child) according to the hamming
 *           distance between its two parents.
 *
 * INPUT PARAMETERS: character array (child; 1s and 0s);
 *                   length of array;
 *                   hamming distance between 2 parents of input child;
 *                   mutation rate;
 *
 * RETURN VALUE: none
 *
 * RESULT: The input array is altered by mutation
 * 
 * CALLS:
 *
 **************************************************************************/
void 
adaptive_mutate (buf, length, numdiff, mutate_rate)
GENE_DATA        buf[];
int              length;
int              numdiff;
float            mutate_rate;
{
 int   i;
 float mutate_level;

 /*
  * The level of mutation a child undergoes is a function of
  * the relative number of differences between its two parents;
  */

 mutate_level = get_mutate_level (numdiff, length, mutate_rate);

 /* mutation of a given position flips the bit */

 for (i=0; i< length; i++) 
     if (fracrand() < mutate_rate)  
        buf[i] = MUTATE(buf[i]);
}
