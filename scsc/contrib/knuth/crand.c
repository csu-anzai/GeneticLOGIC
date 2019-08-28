/* crand.f -- translated by f2c (version of 27 September 1990  15:58:58).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;


/* 	D.E.KNUTH'S RANDOM NUMBER GENERATOR OF */
/* 	"THE ART OF COMPUTER PROGRAMMING", VOL 2, PP. 170-173 */
/*	$Id: crand.c,v 1.1 1993/02/15 09:43:09 joke Exp $ */

integer irn55_(ia)
integer *ia;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer i, j;

/* 	ASSUMING THAT IA(1), ..., IA(55) HAVE BEEN SET UP PROPERLY, */
/* 	THIS SUBROUTINE RESETS THE IA ARRAY TO THE NEXT 55 NUMBERS */
/* 	OF A PSEUDO-RANDOM SEQUENCE, AND RETURNS THE VALUE 1. */
    /* Parameter adjustments */
    --ia;

    /* Function Body */
    for (i = 1; i <= 24; ++i) {
	j = ia[i] - ia[i + 31];
	if (j < 0) {
	    j += 1000000000;
	}
	ia[i] = j;
/* L1: */
    }
    for (i = 25; i <= 55; ++i) {
	j = ia[i] - ia[i - 24];
	if (j < 0) {
	    j += 1000000000;
	}
	ia[i] = j;
/* L2: */
    }
    ret_val = 1;
    return ret_val;
} /* irn55_ */


/* Subroutine */ int in55_(ia, ix)
integer *ia, *ix;
{
    static integer idum;
    extern integer irn55_();
    static integer i, j, k, ii;

/* 	THIS SUBROUTINE SETS IA(1), ..., IA(55) TO STARTING */
/* 	VALUES SUITABLE FOR LATER CALLS ON IRN55(IA). */
/* 	IX IS AN INTEGER "SEED" VALUE BETWEEN O AND 999999999. */
    /* Parameter adjustments */
    --ia;

    /* Function Body */
    ia[55] = *ix;
    j = *ix;
    k = 1;
    for (i = 1; i <= 54; ++i) {
	ii = i * 21 % 55;
	ia[ii] = k;
	k = j - k;
	if (k < 0) {
	    k += 1000000000;
	}
	j = ia[ii];
/* L1: */
    }
/* 	THE NEXT THREE LINES "WARM UP" THE GENERATOR */
    idum = irn55_(&ia[1]);
    idum = irn55_(&ia[1]);
    idum = irn55_(&ia[1]);
    return 0;
} /* in55_ */


/* 	MAIN PROGRAM */

/* Main program */ MAIN__()
{
    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    extern integer irn55_();
    static integer i;
    static real u;
    static integer jrand, ia[55];
    extern /* Subroutine */ int in55_();

    /* Fortran I/O blocks */
    static cilist io__12 = { 0, 6, 0, "(f10.8)", 0 };


    jrand = 55;

    in55_(ia, &c__0);

    for (i = 1; i <= 1000; ++i) {
	++jrand;
	if (jrand > 55) {
	    jrand = irn55_(ia);
	}
	u = (real) ia[jrand - 1] * (float)1e-9;

/* f2c(1) procuses this:
	s_wsfe(&io__12);
	do_fio(&c__1, (char *)&u, (ftnlen)sizeof(real));
	e_wsfe(); */

/* I'd like: */
	printf ("%.8f\n", u);

/* L1: */
    }
} /* MAIN__ */

