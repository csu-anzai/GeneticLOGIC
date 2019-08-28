
/************************************************  file f5.c  ****/


static int a[2][25] = {
	{ -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
	  -32, -16, 0, 16, 32, -32, -16, 0, 16, 32 },
	{ -32, -32, -32, -32, -32, -16, -16, -16, -16, -16, 
	   0, 0, 0, 0, 0, 16, 16, 16, 16, 16, 32, 32, 32, 32, 32 }
	};


double f5(x)
register double *x;
{
	register int i, j;
	register int n;
	double ans;
	double fj; 
	double prod;
	double diff;

	ans = 0.002;
	for (j=0; j<25; j++)
	{
		fj = j+1;
		for (i=0; i<2; i++)
		{
			diff = x[i] - a[i][j];
			for ( prod=1, n=0; n<6; n++)
				prod *= diff;
			fj += prod;
		}
		ans += 1.0 / fj;
	}
	return (1.0 / ans);
}

/* GAeval f5 17:65.536bd2 */

/** end of file **/

