#include "giga.h"
#include <malloc.h>

/*

	*********************** ADT POOL ************************
	*	Routines and data structures to manipulate	*
	*	the pool, return indicated members, select	*
	*	free cells, maintain indices, and sort and 	*
	*	initialize the pool.				*
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

#define ENDLIST(i)  ( (i) >= STORAGE )

struct popmember_st *pool[MAXPOP];	/* population indicator allocation */
struct popmember_st *temporary[4];	/* volatile storage for mating process;
					   to save copying, just swap pointers */

int poolcmp(a,b)
/*
	comparison routine of pool values for qsort sorting
*/
struct popmember_st **a, **b;
{
	if ((*a)->value < (*b)->value) return(-1);
	else if ((*a)->value == (*b)->value) return(0);
	else return(1);
}

void initpop()
/* 
	Initialize population randomly over population
*/
{
	int i,j,k,n;
	
	switch (OPTIONS.popinit) {
	  case 1: { /* Random */
		for(i=0;i<OPTIONS.popsize; i++) {
			pool[i] = (struct popmember_st *) 
				malloc( sizeof(struct popmember_st));
	
			for(j=0;j<OPTIONS.stringsize; j++) 
			  pool[i]->individual[j] = 
				(unsigned char) random() % OPTIONS.alphasize;

			evaluate(pool[i]);
		}
		break;
	  }
	  case 2: { /* Rotation */
		for (i=0; i<OPTIONS.popsize; i+=OPTIONS.alphasize){
		
			pool[i] = (struct popmember_st *) 
				malloc( sizeof(struct popmember_st));

			/* first is chosen at random */
			for(j=0;j<OPTIONS.stringsize; j++) 
			  pool[i]->individual[j] = 
				(unsigned char) random() % OPTIONS.alphasize;

			evaluate(pool[i]);
			
			/* put in other members in rotation */
			for(k=1; ((k < OPTIONS.alphasize) &&
			  ((n = k+i) < OPTIONS.popsize)); k++) {
				pool[n] = (struct popmember_st *) 
				   malloc( sizeof(struct popmember_st));

				for(j=0; j<OPTIONS.stringsize; j++)
				  pool[n]->individual[j] = 
				   (pool[i]->individual[j] +k) %
					OPTIONS.alphasize;

				evaluate(pool[n]);
			}
		}
		break;
	  }
	  default: { /* unimplemented */
		printf("Population initialization type not implemented\n");
		exit(1);
	  }
	} /* end switch statement */
					

        if (OPTIONS.beforesort == yes) {
                printf("Presort Initial ");
                printpop();
        }

	if (OPTIONS.initsort == yes) {
		qsort( (char *) &(pool[0]), (int) OPTIONS.popsize,
			sizeof(struct popmember_st *), poolcmp);

        	if (OPTIONS.aftersort == yes) {
                	printf("Postsort Initial ");
                	printpop();
        	}
	}

	/* initialize storage for mating process */
	for(i=0;i<4;i++) {
		temporary[i] = (struct popmember_st *)
                        malloc( sizeof(struct popmember_st));
	}
}

void freepop()
/*
	Free the members of the population and temprorary before another
	experiment
*/
{
	int i;

	for(i=0; i<OPTIONS.popsize; i++)
		if (! free(pool[i])) printf("Free error\n");

	for(i=0; i<4; i++) 
		if (! free(temporary[i])) printf("Free error\n");

}

struct popmember_st *getmember(i)
/*
	return pointer to the ith member of the population
*/
int i;
{
	return(pool[i]);
}

void swappoptemp(pid,tid)
/*
	swap the population member with the temporary
*/
int pid, tid;
{
	struct popmember_st *t;
	t = pool[pid];
	pool[pid] = temporary[tid];
	temporary[tid] = t;
}

void swappop(idx,idy)
/*
	swap the pair of elements in population
*/
int idx,idy;
{
	
	struct popmember_st *t;
	t = pool[idx];
	pool[idx] = pool[idy];
	pool[idy] = t;
}

void sortadjust(idx,idy)
/* 
	Adjust the sorted order with respect to idx and idy
*/
{
	enum flagtype okay;

	if ( ((pool[idx]->value > pool[idy]->value) && (idx < idy))
	   || ((pool[idx]->value < pool[idy]->value) && (idx > idy)) ) 
		swappop(idx,idy); /* so they will not pass */

	okay = yes;

	while (okay == yes) {
		okay = no; /* if no swap then will exit */
		if (idx < (OPTIONS.popsize -1) ) {
			if (pool[idx+1]->value < pool[idx]->value) {
				okay = yes;
				swappop(idx,idx+1);
				idx++;
			}
		} 
		if (idx > 0) {
			if (pool[idx-1]->value > pool[idx]->value) {	
				okay = yes;
				swappop(idx-1,idx);
				idx--;
			} 
		}
	}

        okay = yes;

        while (okay == yes) {
                okay = no; /* if no swap then will exit */
                if (idy < (OPTIONS.popsize -1) ) {
                        if (pool[idy+1]->value < pool[idy]->value) {
                                okay = yes;
                                swappop(idy,idy+1);
                                idy++;
                        }
                }
                if (idy > 0) {
                        if (pool[idy-1]->value > pool[idy]->value) {
                                okay = yes;
                                swappop(idy-1,idy);
                                idy--;
                        }
                }
        }
}

