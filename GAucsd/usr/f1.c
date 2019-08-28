/************************************************  file f1.c  ****/

double f1(x)
register double *x;
{
	register int i;
	register double sum;

	for (sum = 0.0, i = 0; i < 3; i++)
	{
		/*  accumulate sum of squares of x's  */
		sum += x[i]*x[i];
	}
	return (sum);
}

/* GAeval f1 10:5.12d3 */

/************************************************ end of file ****/
