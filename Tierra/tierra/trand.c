/* trand.c  9-9-92  adapted from ``Numerical Recipes in C'' by Press,
Flannery, Teukolsky and Vetterling.  Tdrand() returns a uniform random deviate
between 0.0 and 1.0.  Tlrand() returns a uniform random deviate between 0 and
2^31 */

#ifndef lint
static char     sccsid[] = "@(#)trand.c	1.5     7/21/92";
#endif

#include "tierra.h"
#include "extern.h"

#ifdef MEM_CHK
#include <memcheck.h>
#endif

#define M1 259200L
#define IA1 7141L
#define IC1 54773L
#define RM1 (1.0/M1)
#define M2 134456L
#define IA2 8121L
#define IC2 28411L
#define RM2 (1.0/M2)
#define M3 243000L
#define IA3 4561L
#define IC3 51349L

void tsrand(seed)
I32s  seed;
{   I16s  j;

    RandIx1 = (IC1 + seed) % M1;       /* Seed the first routine */
    RandIx1 = (IA1 * RandIx1 + IC1) % M1;
    RandIx2 = RandIx1 % M2;            /* and use it to seed the second */
    RandIx1 = (IA1 * RandIx1 + IC1) % M1;
    RandIx3 = RandIx1 % M3;            /* and third routines. */
    for (j = 1; j <= 97; j++)    /* Fill the table with sequential uniform */
    {   RandIx1 = (IA1 * RandIx1 + IC1) % M1; /* deviates generated by the */
        RandIx2 = (IA2 * RandIx2 + IC2) % M2; /* first two routines. */
        TrandArray[j] = (RandIx1 + RandIx2 * RM2) * RM1;
    }                    /* Low- & high-order pieces combined here */
}

double tdrand()
{   double temp;
    I16s j;

    RandIx1 = (IA1 * RandIx1 + IC1) % M1;
                             /* Except when initializing, this is where we */
    RandIx2 = (IA2 * RandIx2 + IC2) % M2;
                             /* start.  Generate the next number for each */
    RandIx3 = (IA3 * RandIx3 + IC3) % M3;                  /* sequence. */
    j = 1 + ((97 * RandIx3) / M3);
                       /* Use the third sequence to get an integer between */
    if (j > 97 || j < 1) /* 1 and 97 */
    {   FEError(-1200,EXIT,NOWRITE,"Tierra tdrand() This cannot happen.");
	fprintf(stderr,"Tierra tdrand() TIERRA INTERNAL ERROR: %s",__FILE__);
	exit(-1200);
    }
    temp = TrandArray[j];               /* Return that table entry, */
    TrandArray[j] = (RandIx1 + RandIx2 * RM2) * RM1;  /* and refill it. */
    return temp;
}

/* ======================================================================= */
#ifdef FUTURE
float gasdev()
{
	static I32s iset=0;
	static float gset;
	float fac,r,v1,v2;

	if  (iset == 0) {
		do {
			v1=2.0*tdrand()-1.0;
			v2=2.0*tdrand()-1.0;
			r=v1*v1+v2*v2;
		} while (r >= 1.0);
		fac=sqrt(-2.0*log(r)/r);
		gset=v1*fac;
		iset=1;
		return v2*fac;
	} else {
		iset=0;
		return gset;
	}
}
#endif
/* ======================================================================= */
#undef M1
#undef IA1
#undef IC1
#undef RM1
#undef M2
#undef IA2
#undef IC2
#undef RM2
#undef M3
#undef IA3
#undef IC3
