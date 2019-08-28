
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
 *  file:       f6.c
 *
 *  Rastringin's function
 *  i = 1, 2, ..., 20
 *  -5.12 <= x <= 5.12
 *  min = f(0,...,0) = 0
 */ 

#include <math.h>
#define A 10.0
#define OMEGA 6.28318530717958647688 

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int i;
	double sum;
	static double best = 10000;

	sum = genes * A;
	for (i = 0; i < genes; i++)
		sum += vect[i] * vect[i] - A * cos (OMEGA * vect[i]);

	return (sum);
}

/*** end of file ***/
