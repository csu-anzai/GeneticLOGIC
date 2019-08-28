
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
/***************************************************************************
							   main_tsp.c

 ***************************************************************************

 This sample main() driver function implementation is designed for use with
 the Traveling Salesman Problem (TSP).  The TSP is a classic optimization
 problem defined as follows:  given a fixed set of cities, in what relative
 sequence must the cities be visited such that the shortest route is taken
 AND such that each city is visited once and only once?

 Rather than manipulating the traditional strings of 1s and 0s, Genitor's
 algorithm for the TSP manipulates symbolic data (actually, integers
 representing 'cities' in the tour).  Further, a special operator developed
 at Colorado State University, the "edge recombination" operator, can be
 used to genetically recombine the cities. 

 ***************************************************************************

 This file provides an example of a genetic algorithm implementation using
 the ga_ modules's subroutines provided with the Genitor package.  For most
 experiments, you should be able to USE THE MODULES WITHOUT ALTERATIONS.
 However, your MAIN() AND EVAL() PROCEDURES CAN DIFFER considerably depending
 on your application.  Further, once you are familiar with the coding 
 conventions of the Genitor package, you may wish to
 CREATE YOUR OWN GENETIC OPERATORS to add to the library of operators
 distributed with Genitor.  The hope is that you will find the basic Genitor
 modules useful in whatever form of experiment you undertake. 

 While, this main() procedure is provided only as an example, it does display
 conventions to which you must adhere in order to use the modular Genitor
 environment.  To find out more about coding with the Genitor package, you
 must read the documentation included with the package before beginning.
 ***************************************************************************/
											
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


int
main (argc, argv)
int   argc;
char *argv[];
{
 int        i,j;
 GENEPTR    mom, dad, child;
 FILE      *fp;
 int      **coord_array;
 EDGE_NODE *edge_table;
 int        num_diffs;
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

/********************************
 Seed the Random Number Generator
 ********************************/
 srandom(RandomSeed);

/******************************************************
 Allocate a genetic pool referenced by the global, Pool
 ******************************************************/
 if ( !(Pool = get_pool(PoolSize, StringLength)) )
	fatal_error(NULL);


/*****************************************************
 Read in a description of the points to be toured and
 create a representation of the distance between them.
 *****************************************************/
 make_dist_array (NodeFile, StringLength);


/**************************************
 Initialize the genetic pool with data.
 **************************************/
 init_pool (SeedPool, Pool, 0, Pool->size, tsp_eval); 

/***********************************
 Sort the initial genetic pool data.
 ***********************************/
 sort_pool (Pool);

/*******************************************************
 Allocate temporary storage for parents of reproduction.
 *******************************************************/
 mom = get_gene (Pool->string_length);
 dad = get_gene (Pool->string_length);


/****************************************************************
 Allocate a table to be used with the Edge Recombination Operator
 ****************************************************************/
 edge_table = get_edge_table (Pool->string_length);


/**********
 Optimize !
 **********/

 for (/* CurrentGeneration already set :
	     either intialized to 0 in its declaration OR
	     initialized by a restart of a previous experiment */;

	     CurrentGeneration < NumberTrials; 
	     CurrentGeneration++)
	{
	/**********************************
	 Choose two genes for reproduction.
	 **********************************/
	 get_parents(mom, dad, Pool, linear, SelectionBias);

    /***************************************************** 
	 Fill a table with the edges possessed by mom and dad.
	 As number within a range from 2 -4 indicating the
	 difference between the two parents is returned, where
	 2 means they're identical and 4 means they're unique.
	 *****************************************************/
     num_diffs = build_edge_table (mom->string, dad->string, 
								   Pool->string_length, edge_table);

    /******************************************************
	 The resulting offspring will be built in mom's storage
	 ******************************************************/
	 child = mom;

    /*****************************************************************
	 Create offspring using the parental information in the edge_table
	 *****************************************************************/
     build_tour (edge_table, child->string, Pool->string_length);

    
	/**************************
	 So, kid, how good are you?
	 **************************/
     child->worth = tsp_eval (child->string, Pool->string_length);

    /******************************************************
	 Insert new gene into population according to its worth
	 ******************************************************/
	 insert_gene (child, Pool);

    /*******************************************************************
	 If the StatusInterval parameter was set and this is the appropriate
	 time, print the population best, worst, mean, and average to stdout
	 *******************************************************************/
	 if (StatusInterval && !(CurrentGeneration % StatusInterval))
	    show_progress (stdout, Pool, CurrentGeneration);

    /*****************************************************************
	 If the DumpInterval parameter was set and this is the appropriate
	 time, save the population and key parameters to disk for later
	 reference (or to restart execution later.
	 *****************************************************************/
     if (DumpInterval && !(CurrentGeneration % DumpInterval))
        dump_status(Pool, DumpBase);
	}
 
/*****************
 Summarize Results
 *****************/
 {
  /*************************************************************
   Each time an offspring uses an edge between two points which
   was not in either of its parents, it is counted as a failure.
   *************************************************************/
   int failures; 

   if (failures = get_number_failures())
       fprintf (stdout, 
				"\nFailures: %d  Avg: %d\n",
		         failures,
				 (int) CurrentGeneration/failures);
   else
	   fprintf (stdout, "No Failures.\n");

  
  final_pool (FinalPool, Pool, CurrentGeneration); 
  fprintf (stdout, "\n");
  print_pool (stdout, Pool, 0, 1);
 }

}
