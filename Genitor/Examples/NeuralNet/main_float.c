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
                               main_float.c

 This sample main() driver function implementation can be used with 
 floating point data, such as might be desired for neural net optimization

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
#include "ga_params.h"
#include "ga_pool.h"
#include "ga_selection.h"
#include "ga_status.h"
#include "ga_signals.h"
#include "ga_global.h"
#include "op_red_surrog.h"


/*******************************************
 Declare evaluation function which tells you
 how "good" a particular gene's solution is.
 NOTE: the input parameters for the eval
	   function should always be a gene and
	   the gene's length
 *******************************************/

#include "eval_float.h"



int
main (argc, argv)
int   argc;
char *argv[];
{
 int     i;
 GENEPTR mom, dad, child;
 int     numdiffs;

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


/**************************
 Print Alogrithm Parameters
 **************************/
 fprintf (stdout, "\n");
 print_params (stdout);
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


/**************************************
 Initialize the genetic pool with data.
 **************************************/
 init_pool (SeedPool, Pool, 0, Pool->size, float_eval); 


/***********************************
 Sort the initial genetic pool data.
 ***********************************/
 sort_pool (Pool);


/*******************************************************
 Allocate temporary storage for parents of reproduction.
 *******************************************************/
 mom = get_gene (Pool->string_length);
 dad = get_gene (Pool->string_length);


 /**********
  Optimize !
  **********/
 for (/* CurrentGeneration already set :
		 either intialized to 0 in its declartion OR
		 initialized by a restart of a previous experiment */;

	     CurrentGeneration < NumberTrials; 
	     CurrentGeneration++)
	{

    /********************************** 
	 Choose two genes for reproduction.
	 **********************************/
     get_parents(mom, dad, Pool, linear, SelectionBias);


	/************************************************************
	 Reproduce using the 'reduced surrogate crossover' operator.
	 A difference factor (actually, the hamming distance) between
	 the two parents is returned.
	 ************************************************************/
     numdiffs =red_surrogate_cross(mom->string,dad->string,Pool->string_length);

    /****************************************************************
	 Choose one of the two offspring to insert into the genetic pool.
	 ****************************************************************/
	 child = ((bitgen() == 0) ? mom : dad);

    /**************************
	 So, kid, how good are you?
	 **************************/
     child->worth = float_eval (child->string, StringLength);

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
  final_pool (FinalPool, Pool, CurrentGeneration); 
  fprintf (stdout, "\n");
  print_pool (stdout, Pool, 0, 1);
}
