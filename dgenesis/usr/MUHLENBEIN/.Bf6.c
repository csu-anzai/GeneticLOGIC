/************************************************  file f6.c  ****
 i = 1, 2, ..., 20
 -500 <= x <= 500
*/ 
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
		sum += vect[i] * vect[i] - A * cos(2*PI*vect[i]);

	return (sum);
}

/************************************************ end of file ****/
