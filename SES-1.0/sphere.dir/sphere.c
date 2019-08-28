
#include <stdio.h>
#include <math.h>
#include "es.h"

static double x[100];


unsigned char scale2color(Individual *ind, int len)
{
	return ((int)log(ind->fit.value)+256) % 256;
/*
	int ret;
        ret = (int) (    12 * (log(ind->fit) /log(10.0)+10.0)   );
	if (ret > 255) ret = 255;
	if (ret <   0) ret =   0;
	return (unsigned char) ret;	
*/
}

	

Fitness eval(double *x, int n)
{
	static Fitness result = { 1, 0.0 };
	int i;


	result.value = 0.0;

	for (i = 0; i < n; i++, x++) {
		result.value += (*x * *x);
	}
	return(result);
}
