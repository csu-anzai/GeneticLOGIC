
/************************************************  file f3.c  ****/



double f3(x)
register double *x;
{
	register int k[5];
	register int i;
	double ans;

	ans = 0;
	for (i=0; i<5; i++)
	{
		k[i] = x[i];
		if (k[i]>x[i]) k[i] -= 1;
		ans += k[i];
	}
	return (ans + 30.0);
}

/* GAeval f3 10:5.12bd5 */

/** end of file **/

