
/************************************************  file f2.c  ****/



double f2(x)
register double *x;
{
	double ans;

	ans = (x[0]*x[0] - x[1]);
	ans *= ans;
	ans = 100.0*ans + (1.0 - x[0])*(1.0 - x[0]);
	return (ans);
}

/* GAeval f2 12:2.048bd2 */

/** end of file **/

