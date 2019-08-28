#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function Goldberg's Deception *
	*********************************
*/

/*
* Author: Joseph Culberson
* Date: April 24, 1992

* Adapted from Eshelman FOGA 91


*/

/* From the table in Eshelman page 278 FOGA 91 */
static double valset[8] = {
	28.0, 26.0, 22.0, 0.0, 14.0, 0.0, 0.0, 30.0};

double fdgb(str)
/*
	Goldberg's 3-bit deception
*/
unsigned char str[];
{
	int i,j,c,s3;
	double sum;

	if ( 0 != (OPTIONS.stringsize % 3)) {
	  printf("For 3-bit deception, stringsize must be a multiple of 3\n");
		exit(1);
	}
	if ( 2 != OPTIONS.alphasize) {
		printf("For 3-bit deception alphabet must be of size 2\n");
		exit(1);
	}

	if (OPTIONS.parameters[0] == MAXDOUBLE) {
		/* tightly coupled */
		sum = (double) 0.0;
		for(i=0; i<OPTIONS.stringsize ;i += 3) {
			sum += valset[ (int) chartodbl(&str[i],3,2) ];
		}
		return(sum);
	} else {
		/* loosely ordered */
		sum = (double) 0.0;
		s3 = OPTIONS.stringsize/3;
		for(i=0;i<s3; i++) {
			c = 0;
			for(j=0;j< 3; j++) {
				c = (2*c) + str[i+(j*s3)];
			}
			sum += valset[c];
		}
		return(sum);
	}
}
