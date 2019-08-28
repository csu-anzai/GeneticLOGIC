#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function fdj3 - DeJong (GAucsd)	*
	*********************************
*/

/*
	Date: April 13, 1992
*/
/*
	Adapted by: Joseph Culberson
*/

double fdj3(strng)
/*
	Modified from f3-ga.c from GAucsd
*/
unsigned char strng[];
{
	int i;
	double p[5], sum;
	double center, offset; /* modified by OPTIONS */
	int k[5];
	unsigned char *str, gray[MAXSTRING];

	if (OPTIONS.stringsize < 50) {
		printf("Error stringsize < 50 in f3.c\n");
		exit(1);
	}

	if (OPTIONS.alphasize != 2 ) {
		printf("Error not binary alphabet in f3.c\n");
		exit(1);
	}

	center = (double) -5.12;
	if (OPTIONS.parameters[0] < MAXDOUBLE) center = OPTIONS.parameters[0];

	sum = (double) 0.0;

	for(i=0;i<5;i++) {
                if (OPTIONS.asgray == yes) {
                        ungray(&strng[i*10],gray,10);
                        str = gray;
                } else str = &strng[i*10];

		p[i] = (chartodbl(str,10,2)*(double) 0.01) + center;
	}

	for(i=0;i<5; i++) {
		k[i] = p[i];
		if (k[i] > p[i]) k[i] -= 1;
		sum += k[i];
	}

	offset = (double) 30.0;
	if (OPTIONS.parameters[1] < MAXDOUBLE) offset = OPTIONS.parameters[1];

	return(sum + offset);
}



