
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
 *  file:       f7.c
 *
 *  Schwefel's function
 *  i = 1, 2, ..., 10
 *  -500 <= x <= 500
 */ 

#include <math.h>

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int i;
	double sum;

	sum = 0.0;
	for (i = 0; i < genes; i++)
		sum += -vect[i] * sin(sqrt(fabs(vect[i])));

	return (sum);
}

/*** end of file ***/
