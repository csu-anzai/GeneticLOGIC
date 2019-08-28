/************************************************  file f5.c  ****/


static int a[2][25] ={
	{
		-32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
		-32, -16, 0, 16, 32, -32, -16, 0, 16, 32	},
	{
		-32, -32, -32, -32, -32, -16, -16, -16, -16, -16, 
		16, 16, 16, 16, 16, 32, 32, 32, 32, 32	}
};


static int K = 500;


double eval(str, length, vect, genes)
char str[];	/* string representation			*/
int length;	/* length of bit string				*/
double vect[];	/* floating point representation		*/
int genes;	/* number of elements in vect			*/
{
	register int i, j;
	register int n;
	double ans;
	double fj; 
	double cj;
	double prod;
	double diff;

	ans = 1.0/K;
	for (j=0; j<25; j++)
	{
		fj = cj = j+1;
		for (i=0; i<genes; i++)
		{
			diff = vect[i] - a[i][j];
			for ( prod=1, n=0; n<6; n++)
				prod *= diff;
			fj += prod;
		}
		ans += 1.0 / fj;
	}
	return (1.0 / ans);
}

/** end of file **/
