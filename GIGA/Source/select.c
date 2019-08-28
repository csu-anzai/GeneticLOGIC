#include "giga.h"

/*

	************************ SELECT *************************
	*	 	Selection Routines			*
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

void p_select(idx, idy)
/*
	select two population indices to indicate parents for mating
*/
int *idx,*idy;
{

	static int next = 0;
	static int way = 1;

	if (OPTIONS.popsize == 2) {
		/* no choices available */
		*idx = 0;
		*idy = 1;
	}
	else switch (OPTIONS.selecttype) {
		case 1: { /* unbiased adjacent pairs */
			*idx = random() % (OPTIONS.popsize - 1);
			*idy = (*idx) + 1;
			break;
		}
		case 2: { /* rotating adjacent pairs */
			*idx = next;
			*idy = (*idx) + 1;
			next = (next +1 ) % (OPTIONS.popsize - 1);
			break;
		}
		case 3: { /* alternating adjacent pairs */
			*idx = next;
			*idy = (*idx)+1;
			next = next+way;
			if ((next >= (OPTIONS.popsize - 2)) ||
				( next <= 0 )) way = -way;
			break;
		}
		default: { /* undefined selection type */
			printf ("Selection type %d not implemented\n",
				OPTIONS.selecttype);
			exit(1);
		}
	}
}
