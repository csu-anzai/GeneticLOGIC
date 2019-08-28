/*

        *********************** GIGA.H **************************
        *       Structures, Constants and Definitions           *
        *********************************************************

* Copyright
* Author: Joseph Culberson
* Date: April 24, 1992
* Permission is hereby granted to copy all or any part of
* this program for free distribution.  The author's name
* and this copyright notice must be included in any copy.
* This program is provided ``as is'' without any express or implied
* warranties.

* Comments and suggestions may be emailed to joe@cs.ualberta.ca
* but this implies no obligation on the part of the author.

* Users are encouraged to modify and extend this program,
* and such modifications should be commented with appropriate attribution.
* This program should not be considered a final product, but rather it is hoped
* that it will serve as a stimulus for further experimentation and development.

* Initial Version Created April 12, 1992.


*/

#include<stdio.h>
#include<math.h>
#include<values.h>

/* Maximum length of individual */
#define MAXSTRING 512
/* Population size limit */
#define MAXPOP 1000
/* Limit for swapping bits in crossover */
#define SWAPBASIS 100
/* Limit for number of collection points */
#define MAXCOLLECT 1000
/* Limit for collection on experiments of first mins and maxes */
#define MAXEXPER 200

enum flagtype  {yes, no};

struct popmember_st {
	double value;
	unsigned char individual[MAXSTRING];
};


struct option_set {
/*
	This is for global option control of various routines
*/
	/* --- Population parameters --- */
	int
		popsize,	/* population size			 */
		stringsize,	/* length of the member strings		 */
		alphasize,	/* alphabet size - 2 to 256		 */
		popinit;	/* Population initialization type
					1 - random
					2 - random rotation equi-# of each*/
	enum flagtype
		initsort,	/* sort population at initialization? 	 */
		sortflag;	/* Keep population sorted?		 */
	
	/* --- Mating parameters --- */
	double
		epsilon;	/* amount of error to allow checking for
					equality; negative no checking;
					if equal => no mating		 */
	enum flagtype
		elitist_mate;	/* Does mating selection include parents?*/
	int
		familysize,	/* number of crossover pairs per family	 */
		bestdef;	/* definition of best for selection (mate)
					1 - Maximum of pair
					2 - Minimum of pair
					3 - Maximum Difference
					4 - Minimum Difference (SQUISH)	 */

	/* --- Crossover parameters --- */
	int
		swapbias,	/* probability of swapping(mod SWAPBASIS)
				   types 2 and 4 for uniform part	*/
		crossrate[MAXSTRING], /* ratio sum for the varying crossover*/
		crossnum[MAXSTRING], /* number of cross points 0-> uniform*/
		total_cross_freq, /* type 4 - sum of type freq's	*/
		crosstype;	/* cross over type
					1 - One point crossover
					2 - Uniform (biased by swapbias) 
					3 - One point - No repeat	
					4 - Varying X-rate		*/

	/* --- Selection parameters --- */
	int
		selecttype;	/* selection type
					1 - unbiased adjacent pairs
					2 - rotating adjacent pairs
					3 - alternating adjacent pairs	 */
	
	/* --- Termination parameters --- */
	int
		maxiterations;	/* maximum iterations before terminating*/

	/* --- Report parameters --- */
	enum flagtype
		beforesort,	/* print initial population before sorting */
		aftersort,	/* print initial population after sorting  */
		final,		/* print final population		   */
		intermediate;	/* print intermediate populations	   */
	int
		frequency;	/* print intermediate every f iterations   */

	/* --- Experiment parameters ---*/
	int
		numexperiments, /* The number of experiments		   */
		collect_freq;	/* gathering for summary repoort	   */

	/* --- Function parameters --- */
	int
		whichfnc;	/* selects which function to optimize	 */
	enum flagtype
		asgray;		/* pass to function to say if gray code  */
	double
		parameters[10];	/* special function parameters		 */

}; /* end of option_set struct */

struct results_set {
	double 
		max,		/* maximum value evaluated		 */
		min;		/* minimum value evaluated		 */

	int
		eval_count;	/* Number of evaluations done		 */

	enum flagtype
		new_results;	/* Flag when max or min changes		 */
};

extern struct option_set OPTIONS;
extern struct results_set RESULTS;

extern long random();
extern void exit();

extern void initpop();
extern struct popmember_st *getmember();
extern void swappoptemp();
extern void crossover();
extern void p_select();
extern double feval();
extern void optioninit();
extern void resultinit();
extern int mate();
extern void evaluate();
extern void printpop();
extern void swappop();
extern void sortadjust();
extern double chartodbl();
extern void summaryinit();
extern void summaryupdate();
extern void summaryfirst();
extern void printsummary();
extern void ungray();
