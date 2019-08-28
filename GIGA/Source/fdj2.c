#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function fdj2 - DeJong (GAucsd)	*
	*********************************
*/

/*
	Date: April 13, 1992
*/
/*
	Adapted by: Joseph Culberson
*/

double fdj2(strng)
/*
	Modified from f2-ga.c from GAucsd
*/
unsigned char strng[];
{
	int i;
	double p[2], sum;
	double center; /* modified by OPTIONS */
	unsigned char *str, gray[MAXSTRING];

	if (OPTIONS.stringsize < 24) {
		printf("Error - string size less than 24 for f2.c\n");
		exit(1);
	}

	if (OPTIONS.alphasize != 2 ) {
		printf("Error not binary alphabet in f2.c\n");
		exit(1);
	}

	center = (double) -2.048;
	if (OPTIONS.parameters[0] < MAXDOUBLE) center = OPTIONS.parameters[0];

	sum = (double) 0.0;

	for(i=0;i<2;i++) {
                if (OPTIONS.asgray == yes) {
                        ungray(&strng[i*12],gray,12);
                        str = gray;
                } else str = &strng[i*12];

		p[i] = (chartodbl(str,12,2)*(double) 0.001) + center;
	}
	sum = (p[0]*p[0] -p[1]);
	sum *= sum;
	sum = ((double) 100.0)*sum + ((double) 1.0 -p[0]) *((double) 1.0 -p[0]);
	return(sum);
}
