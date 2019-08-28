/*
 *  vlcr -- very-long-cycle random number generator
 *
 *  CYCLE LENGTH
 *    p = 30269 ; q = 30307; r = 30323
 *    l = (p-1) * (q-1) * (r-1)/4
 *    l = 6.95E12
 *
 *  SEE ALSO
 *    Wichmann, B.A. and Hill, I.D. (1987) "Building a Random-Number
 *      Generator: A Pascal routine for very-long-cycle random-number
 *      sequences", BYTE March, p127+
 *
 *    Wichmann, B.A. and Hill, I.D. (1982) "A Pseudo-Random Number
 *      Generator" NPL report, DITC, 6/82.
 *
 *    Grafton, R.G.T. "Algorithm AS 157: The Run-up and Run-down Tests"
 *      Applied Statistics, vol.30, pp81-85.
 *
 *  NOTE
 *    "Anyone who considers arithmetical methods of producing random
 *     digits is, of course, in a state of sin."
 *     --- John von Neumann (1951)
 */

/*
 * $Id: vlcr.c,v 1.1 1993/10/04 09:44:18 joke Exp $
 */

#ifdef TEST
#include <stdio.h>
#endif

int vlcr_x, vlcr_y, vlcr_z;	/* 3 seeds, between 1 and 30,000 */

double
random()
{
	double tmp;

	/* 1st generator */
	vlcr_x = 171 * (vlcr_x%177) - 2 * (vlcr_x/177);
	if (vlcr_x < 0)
		vlcr_x += 30269;

	/* 2nd generator */
	vlcr_y = 172 * (vlcr_y%176) - 35 * (vlcr_y/176);
	if (vlcr_y < 0)
		vlcr_y += 30307;

	/* 3rd generator */
	vlcr_z = 170 * (vlcr_z%178) - 63 * (vlcr_y/178);
	if (vlcr_z < 0)
		vlcr_z += 30323;

	/* combine the 3 values */
	tmp = vlcr_x/30269.0 + vlcr_y/30307.0 + vlcr_z/30323.0;
	return (tmp - (int)tmp);
}

#ifdef TEST
main()
{
	int i;

	vlcr_x = 1; vlcr_y = 10000; vlcr_z = 3000;
	for (i=0; i<100; i++)
		printf ("%lf\n", random());

	return 0;
}
#endif
