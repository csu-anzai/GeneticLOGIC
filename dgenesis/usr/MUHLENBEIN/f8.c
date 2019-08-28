
/*
 * ==================================================
 *
 *    DGENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@babbage.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 *
 *
 *  file:       f8.c
 *
 *  Griewank's function
 *  i = 1, 2, ..., 10
 *  -600 <= x <= 600
 *  min = f(0,...,0) = 0 
 */

#include <math.h>
#define D 4000

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int i, j;
	double sum, mult;

	sum = 0.0;
	mult = 1.0;
	for (i = 0; i < genes; i++){
		sum += (vect[i] * vect[i]);
		mult *= cos(vect[i] / sqrt((double) i + 1));
	}
	
	return (sum / D - mult + 1);
}

/*** end of file ***/
