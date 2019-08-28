#include "giga.h"

/*

	********************** CROSSOVER ************************
	*	 	Crossover Routines			*
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

void crossover(p1,p2,c1,c2)
/*
	Perform a crossover of two parents storing in children
*/
struct popmember_st *p1,*p2, *c1, *c2;
{
	long xpoint, xsw, rate, cnr;
	short swt[MAXSTRING];
	int i,k;
	static int restart = 0;
	static int numremain = 0;
	static int s_vec[MAXSTRING];

	switch (OPTIONS.crosstype) {
		case 1: { /* one point */
			/* guarantee at least one bit from opposite parent */
			xpoint = random() % (OPTIONS.stringsize - 1);
			for(i=0;i<=xpoint; i++) {
				c1->individual[i] = p1->individual[i];
				c2->individual[i] = p2->individual[i];
			}
			for(i=xpoint+1; i< OPTIONS.stringsize; i++ ) {
				c1->individual[i] = p2->individual[i];
				c2->individual[i] = p1->individual[i];
			}
			break;
		}
		case 2: { /* Uniform */
			for(i=0; i< OPTIONS.stringsize; i++) {
				xsw = random() % SWAPBASIS;
				if (xsw < OPTIONS.swapbias) {
					c1->individual[i] = p1->individual[i];
					c2->individual[i] = p2->individual[i];
				} else {
					c1->individual[i] = p2->individual[i];
					c2->individual[i] = p1->individual[i];
				}
			}
			break;
		}
		case 3: { /* one point - no repeat */
			if (restart == 0) {
				if (OPTIONS.familysize >= OPTIONS.stringsize) {
					printf("WARNING: familysize too large -");
					printf("set to stringsize-1 in CROSS\n");
				 	OPTIONS.familysize = OPTIONS.stringsize-1;
				}
				/* restart keeps the system in sync with mating */
				restart = OPTIONS.familysize;
				numremain = OPTIONS.stringsize-1;
				for(i=0;i<numremain;i++)
					s_vec[i] = i;
			}

			if (numremain > 1) 
				xpoint = s_vec[k =  random() % numremain];
			else xpoint = s_vec[k=0];
			numremain--;
			restart--;
			s_vec[k] = s_vec[numremain];
			for(i=0;i<=xpoint; i++) {
				c1->individual[i] = p1->individual[i];
				c2->individual[i] = p2->individual[i];
			}
			for(i=xpoint+1; i< OPTIONS.stringsize; i++ ) {
				c1->individual[i] = p2->individual[i];
				c2->individual[i] = p1->individual[i];
			}
			break;
		}
		case 4: { /* scalable - 1 to n */
			/* The idea here is to vary between 1 and n point
			   crossover, perhaps time varying?
			   First attempt.... selection of parameter
			*/
			rate = random() % OPTIONS.total_cross_freq;
			i=0;
			while (OPTIONS.crossrate[i] < rate) i++;
			if ((cnr = OPTIONS.crossnum[i]) == 0) {
				/* uniform */
                           for(i=0; i< OPTIONS.stringsize; i++) {
                                xsw = random() % SWAPBASIS;
                                if (xsw < OPTIONS.swapbias) {
                                        c1->individual[i] = p1->individual[i];
                                        c2->individual[i] = p2->individual[i];
                                } else {
                                        c1->individual[i] = p2->individual[i];
                                        c2->individual[i] = p1->individual[i];
                                }
                           }
			} else { /* multipoint */
				for(i=0;i<OPTIONS.stringsize;i++) swt[i] = 0;
				for(i=0;i<cnr;i++) 
					swt[random() % OPTIONS.stringsize] = 1;
				xsw = 0;
				for(i=0;i<OPTIONS.stringsize;i++) {
					if (swt[i] == 1) xsw = 1-xsw;
					if (xsw == 0) {
                                           c1->individual[i] = p1->individual[i];
                                           c2->individual[i] = p2->individual[i];
                                	} else {
                                           c1->individual[i] = p2->individual[i];
                                           c2->individual[i] = p1->individual[i];
                                	}
				}
			}
			break;
		}
		default: { /* unimplemented */
			printf("Crossover type unimplemented\n");
			exit(1);
		}
	}
}
					

