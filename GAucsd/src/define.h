/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/

/*
 *  file:	define.h
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981 
 *
 *  purpose:	global definitions for genesis
 */

/**************************************************************/
/*****  SYSTEM-DEPENDENT DEFINITIONS (edit if necessary)  *****/
/**************************************************************/

#include <limits.h>
/*
 * If your system doesn't have <limits.h>, you need to #define
 * the following machine constants here by hand:
 *
 *     CHAR_BIT - the number of bits in a byte
 *      INT_MAX - the largest signed integer
 *     LONG_MAX - the largest signed long int
 */

#include <signal.h>
/*
 * If your system doesn't have <signal.h>, you won't be able to
 * interrupt running experiments in a controlled manner and must
 * #define the symbol NOSIGNAL here.
 */

#include <sys/types.h>
/*
 * If your system doesn't have <sys/types.h>, comment out this
 * line and try again - things may just work without this file.
 * If they do not, follow the same instructions as given for a
 * missing <sys/stat.h> below.
 */

#include <sys/stat.h>
/*
 * If your system doesn't have <sys/stat.h>, you cannot use the
 * subdirectory feature and must #define the symbol NOSTAT here.
 * Also, "inset" won't recognize a local Makefile -- you'll have
 * to compile such experiments manually with "make GAeval=...".
 */

#ifdef UTILITY
#include <string.h>
/*
 * If your system doesn't have <string.h>, try using <strings.h>
 * instead.  If you don't have that either, you won't be able to
 * compile the inset utility.  To run an experiment named "foo",
 * you will have to compile it with
 *
 *         make -f $GAUCSD/bin/$MACH/Makefile GAeval=foo
 *
 * Then use $GAUCSD/usr/sample.in as template to create "foo.in".
 */
#endif


/*************************************/
/*****  end of system-dependency *****/
/*************************************/

/* fitness ratio 'r' closest to 1.0 that DPE can handle */
#define FIT_RATIO 0.99

/* This is what happens when a mutation occurs */
#define MUTATION(Gene,Mask) (Gene ^= Mask)

/* maximum number of application-specific arguments */
#define MAXGARG 50


/***********************************/
/*****  DO NOT EDIT BELOW HERE *****/
/***********************************/

/* define various constants and bitmasks */
#define BIT(X)  ((1 << (CHAR_BIT - 1)) >> (X))
#define PRE(X)  ((~0 << CHAR_BIT) >> (X))
#define POST(X) (~PRE(X))

/* each member of the population has this form */
typedef struct {
	char *Gene;
	double Perf;
	int Needs_evaluation;
} STRUCTURE;

/* the best structures are saved in the following record */
typedef struct {
	char *Gene;
	char *Phene;
	double Perf;
	int Gen;
	long Trials;
} BESTSTRUCT;

/* print a debugging message */
#define Trace(s)  if (Traceflag) \
		{ printf(s); printf("\n"); fflush(stdout);}


/****************************************************************/
/* Rand() returns a pseudo-random double value between 0 and 1; */
/* Randint(L,H) produces an integer between L and H inclusive.  */
/****************************************************************/

#define RAND_DEG 31 	/* size of state array */

#define Randint(low,high) ((int)(low + (high-low+1) * Rand()))

/** end of file **/

