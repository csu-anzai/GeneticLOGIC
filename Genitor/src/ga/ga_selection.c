
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
#include "ga_copy.h"

/***************************************************************************
 * FUNCTION: linear
 *
 * DESCRIBE: generates random integer between 0 and input max number
 *           using input linear bias
 *
 *           ALGORITHM: (Joan Kauth)
 *           See Knuth vol 2 pg 102.
 *           Probability distribution function is: f(x) = bias - 2(bias - 1)x
 *           bias = (prob of first rule) / (prob of middle rule)
 *
 * INPUT PARAMETERS: max number (integer), linear bias (float)
 *
 * RETURN VALUE: integer between 0 and max number
 *
 * CALLS: fracrand
 ****************************************************************************/

linear(max, bias)
int    max;
float  bias; /* y-intercept of linear distribution */
{
 int index;            /* index between 0 and pop_size */
 double sqrt();

 index = max * (bias - sqrt(bias * bias - 4.0 * (bias -1) * fracrand()))
	     / 2.0 / (bias -1);

 return(index);
}

/***************************************************************************
 * FUNCTION: get_parents
 *
 * DESCRIBE: according to bias described by input params, two genes are selected
 *           from the pool
 *
 * INPUT PARAMETERS: GENEPTRs to parents, 
 *                   POOLPTR  to pool,
 *                   function pointer to bias function
 *                   float    bias
 *
 * RETURN VALUE: none
 *
 * RESULT: input mom and dad pointers are given values of two genes in pool
 *
 * CALLS: input bias_fun
 ****************************************************************************/
void
get_parents (mom, dad, pool, bias_fun, bias)
GENEPTR  mom, dad;
POOLPTR  pool;
int      (*bias_fun)();
float    bias;
{
 int one, two;
 
 one = (*bias_fun)(pool->size, bias);
 two = (*bias_fun)(pool->size, bias);

 if (pool->size > 1)
    while(one==two)
       two = (*bias_fun)(pool->size, bias);

 gene_copy (mom, &pool->data[one], pool->string_length);
 gene_copy (dad, &pool->data[two], pool->string_length);
}
