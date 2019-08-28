
/************************************************  file f4.c  ****/

double f4(x)
register double *x;
{
	extern double Rand();
	register int i;
	register int k;
	double ans;
	double prod;

	ans = 0.0;
	for (i = 0; i < 30; i++)
	{
		for (prod = 1.0, k = 0; k < 4; k++)
			prod *= x[i];
		ans += (i + 1)*prod;
	}

	/* now add Gaussian noise */
	prod = -6.0;
	for (i = 0; i < 12; i++) prod += Rand();

	ans += prod;
	return(ans);
}

/* GAeval f4 8:1.28bd30 */

/** end of file **/

