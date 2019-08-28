
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
#include <ctype.h>
#include "ga_random.h"
#include "gene.h"
#include "ga_global.h"
#include "ga_params.h"
#include "ga_pool.h"
#include "ga_selection.h"
#include "ga_status.h"
#include "ga_signals.h"
#include "op_edge_recomb.h"

/*******************************************
 Declare evaluation function which tells you
 how "good" a particular gene's solution is.
 NOTE: the input parameters for the eval
       function should always be a gene and
       the gene's length
 *******************************************/
#include "eval_tsp.h"


/***********************************************************/
/* swap best individuals between subpopulations.           */
/* GLOBALS: NumPop, SwapInterval, SwapNumber               */
/***********************************************************/
void swap_strings(pool_array, swap_iteration) 
POOLPTR pool_array[]; /* pointers to the subpopulations */
int swap_iteration;   /* round-robin placement */
{
   int dest;
   int subpop;
   int top;
   int swapper;

   for (subpop=0; subpop<NumPop; subpop++) {
      dest = ((subpop + swap_iteration) % NumPop);
                         /* pool index 0 is the best in a subpop */
      for (swapper=0; swapper<SwapNumber; swapper++) 
	 insert_unique_gene(&pool_array[subpop]->data[swapper], 
			    pool_array[dest], SequenceFlag);
   }
}


/***********************************************************/
/* main program for running subpopulations                 */
/***********************************************************/
int main (argc, argv)
int   argc;
char *argv[];
{
 int        i,j;
 GENEPTR    mom, dad, child;
 FILE      *fp;
 int      **coord_array;
 EDGE_NODE *edge_table;
 int        num_diffs;
 POOLPTR    *pool_array;
 int subpop;
 int swap_counter;
 int swap_iteration;
 int experiment_cnt;

/**********************
 Setup signal handlers.
 **********************/
 setup_signal();


/*************************************************************
 Set the global parameters according to command line arguments.
 *************************************************************/
 argc--;
 argv++;
 parse_command_line (argc, argv);

/**********************
 Print Parameter Values
 **********************/
 fprintf (stdout, "\n");
 print_params(stdout);
 fprintf (stdout, "\n");
 fprintf (stdout, "Trial   Bst           Wst           Median        Avg\n");

/********************************
 Seed the Random Number Generator
 ********************************/
 srandom(RandomSeed);

/******************************************************
 Allocate a genetic pool referenced by the global, Pool
 ******************************************************/
 if (!(pool_array = (POOLPTR *)malloc(sizeof(POOLPTR) * NumPop)))
    fatal_error(NULL);

 for (i=0; i<NumPop; i++) 
    if ( !(pool_array[i] = get_pool(PoolSize, StringLength)) )
       fatal_error(NULL);


/*****************************************************
 Read in a description of the points to be toured and
 create a representation of the distance between them.
 *****************************************************/
 SequenceFlag = 1;
 make_dist_array (NodeFile, StringLength);


/*******************************************************
 Allocate temporary storage for parents of reproduction.
 *******************************************************/
 mom = get_gene (pool_array[0]->string_length);
 dad = get_gene (pool_array[0]->string_length);


/****************************************************************
 Allocate a table to be used with the Edge Recombination Operator
 ****************************************************************/
 edge_table = get_edge_table (pool_array[0]->string_length);

for (experiment_cnt=0; experiment_cnt<Experiments; experiment_cnt++) {
   if (experiment_cnt)
      CurrentGeneration = 0;  

/**************************************
 Initialize the genetic pool with data.
 **************************************/
 for (i=0; i<NumPop; i++) 
    init_pool (SeedPool, pool_array[i], 0, pool_array[i]->size, tsp_eval); 

/***********************************
 Sort the initial genetic pool data.
 ***********************************/
 for (i=0; i<NumPop; i++) 
    sort_pool (pool_array[i]);

/**********
 Optimize !
 **********/

 subpop = 0;       /* start with the first subpop */
 swap_counter = 0; /* & start counter */
 swap_iteration = 1;

 for (/* CurrentGeneration already set :
         either intialized to 0 in its declaration OR
         initialized by a restart of a previous experiment */;

         CurrentGeneration < NumberTrials; 
         CurrentGeneration++)
    {

     if (StatusInterval && !(CurrentGeneration % StatusInterval)) {
        show_progress_brief (stdout, pool_array[subpop], CurrentGeneration);
	if (pool_array[subpop]->data[0].worth < CutOff)
	   CurrentGeneration = NumberTrials;
     }

    swap_counter++;
    if (swap_counter == SwapInterval) {
       subpop++;
       swap_counter = 0;
       if (subpop == NumPop) {
	  subpop = 0;
	  swap_strings(pool_array, swap_iteration);
	  swap_iteration++;
	  if (swap_iteration == NumPop)
	     swap_iteration = 1;
       }
       else 
	  CurrentGeneration -= SwapInterval;
    }
    /**********************************
     Choose two genes for reproduction.
     **********************************/
     get_parents(mom, dad, pool_array[subpop], linear, SelectionBias);

    /***************************************************** 
     Fill a table with the edges possessed by mom and dad.
     As number within a range from 2 -4 indicating the
     difference between the two parents is returned, where
     2 means they're identical and 4 means they're unique.
     *****************************************************/
     num_diffs = build_edge_table (mom->string, dad->string, 
                                   pool_array[subpop]->string_length,
				   edge_table);

    /******************************************************
     The resulting offspring will be built in mom's storage
     ******************************************************/
     child = mom;

    /*****************************************************************
     Create offspring using the parental information in the edge_table
     *****************************************************************/
     build_tour (edge_table, child->string, pool_array[subpop]->string_length);

    
    /**************************
     So, kid, how good are you?
     **************************/
     child->worth = tsp_eval (child->string, pool_array[subpop]->string_length);

    /******************************************************
     Insert new gene into population according to its worth
     ******************************************************/
     insert_gene (child, pool_array[subpop]);

    /*****************************************************************
     If the DumpInterval parameter was set and this is the appropriate
     time, save the population and key parameters to disk for later
     reference (or to restart execution later.
     *****************************************************************/
     if (DumpInterval && !(CurrentGeneration % DumpInterval))
        dump_status(pool_array[subpop], DumpBase);
    }
 
/*****************
 Summarize Results
 *****************/

  for (subpop=0; subpop<NumPop; subpop++) {
     show_progress_brief (stdout, pool_array[subpop], CurrentGeneration);
   }
 }
}
