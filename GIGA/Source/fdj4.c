#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function fdj4 - DeJong (GAucsd)	*
	*********************************
*/

/*
	Date: April 14, 1992
*/
/*
	Adapted by: Joseph Culberson
*/

double fdj4(strng)
/*
	Modified from f4-ga.c from GAucsd
*/
unsigned char strng[];
{
	int i,k;
	double p[30], pd, sum;
	double center, noise; /* modified by OPTIONS */
	unsigned char *str, gray[MAXSTRING];

	if (OPTIONS.stringsize < 240) {
		printf("Error stringsize < 240 in f4.c\n");
		exit(1);
	}

	if (OPTIONS.alphasize != 2 ) {
		printf("Error not binary alphabet in f4.c\n");
		exit(1);
	}

	center = (double) -1.28;
	if (OPTIONS.parameters[0] < MAXDOUBLE) center = OPTIONS.parameters[0];

	for(i=0;i<30;i++) {
                if (OPTIONS.asgray == yes) {
                        ungray(&strng[i*8],gray,8);
                        str = gray;
                } else str = &strng[i*8];

		p[i] = (chartodbl(str,8,2)*(double) 0.01) + center;
	}

	sum = (double) 0.0;

	for (i=0; i<30; i++) {
		pd = (double) 1.0;
		for(k=0; k< 4; k++) pd *= p[i];
		sum += ((double) (i+1)) * pd;
	}

	/* now add Gaussian noise */
	noise = (double) -6.0;
	k = 12;

	/* user specified noise factor */
	if (OPTIONS.parameters[1] < MAXDOUBLE) {
		noise = OPTIONS.parameters[1];
		k = (int) (-2.0 * noise);
		if (k<0) k = -k;
	}

	for (i=0; i<k; i++) 
		noise += ((double) random()) / (((double) MAXINT) + 1.0);

	return(sum+noise);
}
