/************************************************  file f3.c  ****/

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int k[5];
	register int i;
	double ans;

	ans = 0;
	for (i=0; i<5; i++)
	{
		k[i] = vect[i];
		if ( k[i] > vect[i]) k[i] -= 1;
		ans += k[i];
	}
	return (ans + 30.0);
}

/** end of file **/
