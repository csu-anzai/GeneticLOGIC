
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	define.h
 *
 *  purpose:	global definitions for genesis
 */
 
/************************ INCLUDE FILES ***********************/

#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include "format.h"


/********************** SYSTEM DEPENDENCIES *******************/

/* Be sure to set exactly one of the following			*/
/* Set UNIX for Vax or Sun (or other 32-bit UNIX systems)	*/
/* Set TURBOC for Turbo C for DOS systems			*/

#define TURBOC	0
#define UNIX	1

#if UNIX

#include <curses.h>

#else

/* the following provide functions similar to UNIX <curses.h> */
#include <conio.h>
#define clrtoeol	clreol
#define refresh()	fflush(stdout)
#define printw		printf
#define scanw		scanf
#define getw		gets

#endif


/********************** CONSTANTS ****************************/

#define CHARSIZE 8

/* used in random number generator below */
#define MASK 2147483647
#define PRIME 65539
#define SCALE 0.4656612875e-9



/************************ TYPES ******************************/

/* each member of the population has this form */
typedef struct {
	char *Gene;
	double Perf;
	int Needs_evaluation;
} STRUCTURE;

/* the best structures are saved in the following record */
typedef struct {
	char *Gene;
	double Perf;
	int Gen;
	int Trials;
} BESTSTRUCT;

/* records for interpreting bitstrings according to template file */
typedef struct {
	double min;
	double max;
	unsigned long values;
	char format[16];
	double incr;
	int bitlength;
} GENESTRUCT;

/************************ MACROS *****************************/

/* Comparison of two performance values */
#define BETTER(X,Y)	(Maxflag ? (X) > (Y) : (X) < (Y))

/* An allele has converged if all but a FEW */
/* structures have the same value at that position. */
#define	FEW		(Popsize/20)


/* print a debugging message		*/
#define Trace(s)  if (Traceflag) \
		{ printf(s); printf("\n"); fflush(stdout);}


/****************************************************************/
/*	 Rand computes a psuedo-random				*/
/*	 double value between 0 and 1, excluding 1.  Randint	*/
/*	 gives an integer value between low and high inclusive.	*/
/****************************************************************/

#define Rand()  (( Seed = ( (Seed * PRIME) & MASK) ) * SCALE )

#define Randint(low,high) ( (int) (low + (high-low+1) * Rand()))

/** end of file **/
