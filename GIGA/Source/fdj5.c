#include "giga.h"

/*
	Functions must return double, and take one
	argument, array of char.
	include "giga.h" to obtain various parameters etc.
*/

/*
	*********************************
	* Function fdj5 - DeJong (GAucsd)	*
	*********************************
*/

/*
	Date: April 14, 1992
*/
/*
	Adapted by: Joseph Culberson
*/

static int a[2][25] ={
        {
                -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
                -32, -16, 0, 16, 32, -32, -16, 0, 16, 32        },
        {
                -32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
                16, 16, 16, 16, 16, 32, 32, 32, 32, 32  }
};

static int K = 500;


double fdj5(strng)
/*
	Modified from f5-ga.c from GAucsd
*/
unsigned char strng[];
{
	int i,j,n;
	double p[2], fj, diff, prod, sum;
	double center; /* modified by OPTIONS */
	unsigned char *str, gray[MAXSTRING];

	if (OPTIONS.stringsize < 34) {
		printf("Error stringsize < 34 in f5.c\n");
		exit(1);
	}

	if (OPTIONS.alphasize != 2 ) {
		printf("Error not binary alphabet in f5.c\n");
		exit(1);
	}

	center = (double) -65.536;
	if (OPTIONS.parameters[0] < MAXDOUBLE) center = OPTIONS.parameters[0];

	for(i=0;i<2;i++) {
                if (OPTIONS.asgray == yes) {
                        ungray(&strng[i*17],gray,17);
                        str = gray;
                } else str = &strng[i*17];

		p[i] = (chartodbl(str,17,2)*(double) 0.001) + center;
	}

	sum = (double) 1.0 / (double) K;

	for(j=0; j< 25; j++) {
		fj = (double) j+1;
		for (i=0; i<2; i++) {
			diff = p[i] - a[i][j];
			prod = (double) 1.0;
			for(n=0; n<6; n++) prod *= diff;
			fj += prod;
		}
		sum += ((double) 1.0) / fj;
	}

	return ((double) 1.0 / sum);
}
