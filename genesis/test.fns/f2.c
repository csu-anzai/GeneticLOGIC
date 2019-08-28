/************************************************  file f2.c  ****/

double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	double ans;

	ans = (vect[0]*vect[0] - vect[1]);
	ans *= ans;
	ans = 100.0*ans + (1.0 - vect[0])*(1.0 - vect[0]);
	return (ans);
}

/** end of file **/
