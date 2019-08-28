/************************************************  file f6.c  ****/

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int i;
	double sum;

	sum = 0.0;
	for (i = 0; i < length; i++)
		sum += (str[i] == '0');

	return (sum);
}

/************************************************ end of file ****/
