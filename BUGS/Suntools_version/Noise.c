
/*
c
c                    Gaussian Random Number Generator
c
c    Donald H. House     	July 1, 1982
c
c  This function takes as parameters real valued mean and standard-deviation,
c  and an integer valued seed.  It returns a real number which may be
c  interpreted as a sample of a normally distributed (Gaussian) random variable
c  with the specified mean and standard deviation.  As a side effect the value
c  of the integer seed is modified.
c
c  The computational technique used is to pass a uniformly distributed random
c  number through the inverse of the Normal Distribution function.

				 Translated into C by Robert Allen January 25, 1989
*/



#define	ITBLMAX 20
#define	didu	(20.0 / 0.5)
/* Where it says 20.0, put the value of ITBLMAX									*/


double noise (mean, std)
double mean, std;

{
	double drand48(), u, du;
	int index, sign;
	double temp;

	static double tbl[ITBLMAX+1]
	 = {0.00000E+00, 6.27500E-02, 1.25641E-01, 1.89000E-01,
     		  2.53333E-01, 3.18684E-01, 3.85405E-01, 4.53889E-01,
     		  5.24412E-01, 5.97647E-01, 6.74375E-01, 7.55333E-01,
     		  8.41482E-01, 9.34615E-01, 1.03652E+00, 1.15048E+00,
    		  1.28167E+00, 1.43933E+00, 1.64500E+00, 1.96000E+00,
     		  3.87000E+00	};

	u = drand48();
	if (u >= 0.5) {
		sign = 1;
		u = u - 0.5;}
    else
		sign = -1;
	

	index = (int)(didu * u);
	du = u - index / didu;
	temp = mean + sign * (tbl [index] + (tbl [index + 1] - tbl [index]) *
     		du * didu) * std;

	return temp;
}



/* Try the noise generator-- by JRS 										*/
/*
#include <stdio.h>
#include <math.h>
test_noise () {
	extern srand48();
	
	int i;
	double mean;

	for (mean=0.0; mean < 5.0; mean += 1.0) {
		printf("mean: %g  ",mean);
		for (i=0; i<5; i++) {
			printf("%g, ", noise(mean,0.1));
		}
		printf("\n");
	}
}
*/
