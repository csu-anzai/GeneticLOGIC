
/* evaluation file for GENESIS 1.4ucsd GA simulator */
/* produced by "wrapper" awk(1) script from "f1.c" */

extern int   GArgc;   /* number of application-specific arguments */
extern char *GArgv[]; /* vector of application-specific arguments */
extern double Ctoi(); /* double returned to facilitate 'A' option */

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

int GAgenes = 3; /* the number of genes */

/* locates the end of each gene; negate to inhibit DPE */
int GAposn[3] = {10, 20, 30};

/* multiplication factors (see _eval() below) */
double GAfact[3] = {0.010000, 0.010000, 0.010000};

/* displacement terms (see _eval() below) */
double GAbase[3] = {-5.120000, -5.120000, -5.120000};

/* user's evaluation function needs unpacking and decoding */
double _eval(genome, length)
	char *genome;
	int length;
{
	static double p1[3];
	char tmp[10];
	register int i;
	extern   char *Buff;
	register char *buff;
	register double *f = GAfact;
	register double *b = GAbase;

	if (length < 0)	/* report previous phenotype */
	{
		sprintf(genome, "\n%10g %10g %10g ",
			p1[0], p1[1], p1[2]);
		return;
	}
	/*  GAlength 30  */
	if (length < 30)
		Error("length error in eval");

	Unpack(genome, buff = Buff);
	for (i = 0; i < 3; i++, buff += 10)
	{
		Degray(buff, tmp, 10);
		p1[i] = (double) (Ctoi(tmp, 10) * *f++ + *b++);
	}
	return((double) f1(p1));
}

/************************************************ end of file ****/
