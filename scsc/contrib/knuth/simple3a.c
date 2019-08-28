/* Output from p2c, the Pascal-to-C translator */
/* From input file "simple3a.p" */


#include "p2c.h"


/* simple 3a: a program to flip 20 coins and keep track of heads ans tails
   uses repeat--until construct */

/* $Id: simple3a.p,v 1.1 1993/02/15 10:06:27 joke Exp $ */

#define ncoins          20		/* number of coins */

#define probability     0.5		/* probability of heads turning up */


main (argc, argv)
    int argc;
    Char *argv[];
{					/* Main program */
    long heads_or_tails[2];		/* heads or tails count */
    long j;				/* loop counter */
    boolean toss;			/* toss: true=heads, false=tails */

    PASCAL_MAIN (argc, argv);
    heads_or_tails[0] = 0;
    heads_or_tails[1] = 0;
    randomize (0);		/* patched here  -joke */
    j = 0;
    do {				/* coin toss loop */
	toss = flip (probability);
/* p2c: simpl3a.p, line 22: Warning: Symbol 'FLIP' is not defined [221] */
	if (toss)
	    heads_or_tails[0]++;
	else
	    heads_or_tails[1]++;
	j++;
    } while (j != ncoins);
    printf ("In %ld coin tosses there were %ld heads and %ld tails\n",
	    (long) ncoins, heads_or_tails[0], heads_or_tails[1]);
    exit (0);
}					/* Main program */

/* End. */
