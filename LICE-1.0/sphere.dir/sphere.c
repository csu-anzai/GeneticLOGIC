


#include <stdio.h>
#include <math.h>
#include "lice.h"

/*
 * If you're using PixMon, this function should map an individual
 * to a color index. If not, just leave it as it is and ingnore it.
 *
 */
unsigned char scale2color(Individual *ind, int len)
{
	return ((int)(10*log(ind->fit.value))) % 256;
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
