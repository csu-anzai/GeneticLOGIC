
#include "giga.h"

/*

	*********************** MATING **************************
	*	Mating and Replacement Routine			*
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

int mate(idx, idy)
/*
	mate the (idx,idy) pair of the pool using 
	return 1 if parents are identical -> no mating
	return 0 else.

	OPTIONS.familysize trials (crossovers)
*/

int idx, idy;
{

	extern struct popmember_st *temporary[];
	struct popmember_st *parent[2];
	int i;
	int max, min, nmax, nmin, temp1, temp2;
	enum flagtype parent_best, different;
	double tv;
	
	/* get the parents */

	parent[0] = getmember(idx);
	parent[1] = getmember(idy);

	/* determine whether the parents differ if not crossover is usless */
	tv = parent[0]->value - parent[1]->value;
	if (tv < 0.0) tv = -tv;
	if (tv <= OPTIONS.epsilon) {
		different = no;
		for(i=0; (i<OPTIONS.stringsize) && (different == no); i++) {
			if (parent[0]->individual[i] != parent[1]->individual[i])
				different = yes;
		}
		if (different == no) {
			return(1);
		}
	} 

	max = -1; min = -1;
	temp1 = 0; temp2 = 1;
	
	/* crossover and keep track of max-min pair */
	for(i=0; i< OPTIONS.familysize; i++) {
		crossover(parent[0], parent[1], 
			temporary[temp1], temporary[temp2]);

		evaluate(temporary[temp1]);
		evaluate(temporary[temp2]);

		if (temporary[temp1]->value > temporary[temp2]->value) {
			nmax = temp1; nmin = temp2;
		} else { nmax = temp2; nmin = temp1; }

		if (max <0) { max = nmax; min = nmin; } /* first time */

		/* select on basis of */
		switch (OPTIONS.bestdef) {
			case 1 : { /* MAXIMUM */
				if (temporary[nmax]->value >
				    temporary[max]->value){
					max = nmax; min = nmin;
				}
				break;
			}
			case 2: { /* MINIMUM */
				if (temporary[nmin]->value <
				    temporary[min]->value){
					max = nmax; min = nmin;
				}
				break;
			}
			case 3: { /* MAXIMUM DIFFERENCE */
				if ( (temporary[nmax]->value 
					- temporary[nmin]->value) >
				       (temporary[max]->value 
					- temporary[min]->value) ) {
					   max = nmax; min = nmin;
				}
				break;
			}
			case 4: { /* MINIMUM DIFFERENCE (SQUISH) */
				if ( (temporary[nmax]->value 
					- temporary[nmin]->value) <
				       (temporary[max]->value 
					- temporary[min]->value) ) {
					   max = nmax; min = nmin;
				}
				break;
			}
			default: {
				printf("Selection basis not implemented\n");
				exit(1);
			}
		}

		/* reset temporaries for next crossover */
		temp1 = 3 - min;
		temp2 = 3 - max;
	}

	parent_best = no;

	if (OPTIONS.elitist_mate == yes) {
		/* we must verify whether the best offspring is better than the
			parents */
		if ( parent[0]->value > parent[1]->value ) {
			nmax = 0; nmin = 1;
		} else { nmax = 1; nmin = 0;}

                switch (OPTIONS.bestdef) {
                        case 1 : { /* MAXIMUM */
                                if (parent[nmax]->value >
                                    temporary[max]->value){
                                        parent_best = yes;
                                }
				break;
                        }
                        case 2: { /* MINIMUM */
                                if (parent[nmin]->value <
                                    temporary[min]->value){
                                        parent_best = yes;
                                }
				break;
                        }
                        case 3: { /* MAXIMUM DIFFERENCE */
                                if ( (parent[nmax]->value
                                        - parent[nmin]->value) >
                                       (temporary[max]->value
                                        - temporary[min]->value) ) {
                                           parent_best = yes;
                                }
				break;
                        }
                        case 4: { /* MINIMUM DIFFERENCE (SQUISH) */
                                if ( (parent[nmax]->value
                                        - parent[nmin]->value) <
                                       (temporary[max]->value
                                        - temporary[min]->value) ) {
                                           parent_best = yes;
                                }
				break;
                        }
                        default: {
                                printf("Selection basis not implemented\n");
                                exit(1);
                        }
                }
	}

	if ( parent_best == no) {
		/* swap the children to replace the parents */
		/* note: we replace upper in the event no   */
		/* sorting is done to get implicit sorting  */

		if (idx > idy) {
			swappoptemp(idx, max);
			swappoptemp(idy, min);
		} else {
			swappoptemp(idy, max);
			swappoptemp(idx, min);
		}
	} else { /* if parent_best then verify that best is in top */
		if (parent[0]->value > parent[1]->value) 
			swappop(idx,idy);
	}
	return(0);
}
