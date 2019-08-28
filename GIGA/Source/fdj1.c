#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function fdj1 - DeJong (GAucsd)	*
	*********************************
*/

/*
	Date: April 13, 1992
*/
/*
	Adapted by: Joseph Culberson
*/

double fdj1(strng)
/*
	Modified from f1-ga.c from GAucsd
*/
unsigned char strng[];
{
	int i;
	double p, sum;
	double center; /* modified by OPTIONS */
	unsigned char  *str, gray[MAXSTRING];

	if (OPTIONS.stringsize < 30) {
		printf("Error stringsize < 30 in f1.c\n");
		exit(1);
	}

	if (OPTIONS.alphasize != 2 ) {
		printf("Error not binary alphabet in f1.c\n");
		exit(1);
	}

	center = (double) -5.12;
	/* allow user to override default center */
	if (OPTIONS.parameters[0] < MAXDOUBLE) center = OPTIONS.parameters[0];

	sum = (double) 0.0;

	for(i=0;i<3;i++) {
		if (OPTIONS.asgray == yes) {
			ungray(&strng[i*10],gray,10);
			str = gray;
		} else str = &strng[i*10];
		p = (chartodbl(str,10,2)*(double) 0.01) + center;
		sum += p*p;
	}
	return(sum);
}

