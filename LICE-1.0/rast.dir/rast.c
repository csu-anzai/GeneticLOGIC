


#include <stdio.h>
#include <math.h>
#include "lice.h"

static const double omega =   6.28318530717958647688,
                    amp  =  10.0;


static double x[100];


unsigned char scale2color(Individual *ind, int len)
{
	int ret;
	if (ind->fit.value < 10.0) return (int) ind->fit.value * 10;

        ret = 100 + (int)(155 * (log(ind->fit.value)/log(50000.0)));
	if (ret > 255) ret = 255;
	if (ret <   0) ret =   0;
	return (unsigned char) ret;	
}

	

Fitness eval(double *x, int n)
{
  register i;
  Fitness result;
  double fx = 0.0;

  for (i = 0; i < n; i++) {
    fx += x[i] * x[i] - amp * cos(omega * x[i]);
  }
  result.valid = 1;
  result.value = fx + n * amp ;
  return(result);
}
