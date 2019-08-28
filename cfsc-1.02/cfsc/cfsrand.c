/*      CFSRAND.C

This file, CFSRAND.C, is part of the CFS-C classifier system (Copyright 1986,1988,1990 Rick L. Riolo).

It contains a number of pseudo-Random number generators.
The basic generator is URand01(), so you can fill in your own for that one.
You can also fill in InitRand() if need be for your generator.
The basic method I use is described in CACM, 31, #10, p1195 (1988).
There is code here that also filters these basic numbers
through the single-sequence "shuffle" mechanism described by Knuth.

The routines in this file are described below:
    Rand01()                    return f, 0 <= f < 1, a uniform random distribution.
    InitRand();                 initialize random number generator (if needed).
    NRand1( Mean, SD );         Returns normal distribution around specified mean with specified SD.
    UNoise( MaxDelta );	        return random noise around 0, in specified range.
    RandChoose( n, N, Sample[], Set[] );  Choose at random n int's from N.
    RandPermute( N, Set[] );    Randomly permute (shuffle) N int's.

See the file UTILITY.H for these two macros:
	URandN	( upperbound )		return random draw from 0 to upperbound.
	URandF	( UpperFloat )		return float between 0 to UpperFloat

******/

#include	"compiler.h"
#include	<math.h>
#include	<ctype.h>
#include	<stdio.h>	   /* used by URand01() for opening a test file */

#include	"utility.h" 
#include	"core.h"

    /* here's the function prototype for this file */

float           URand01();
VOID            InitRand();
float           NRand1( ); /* not on ds5000: float Mean, float SD ); */
float           UNoise( ); /* not on ds5000: float MaxDelta ); */
int             RandChoose(); /* not on ds5000: int n, int N, int Sample[], int Set[] ); */
VOID            RandPermute( ); /* not on ds5000:); /* not on ds5000: int N, int Set[] ); */

#define RANDa 16807     /* basic constants for URand01() */
#define RANDm 2147483647
#define RANDq 127773
#define RANDr 2836

#if (INTSZ == 16)		/* if neither of these, errors will get someone's attention */
long int URndSd = 1;
#else
#if (INTSZ == 32)
int URndSd = 1;
#endif
#endif

short           GotXRand = FALSE;   /* for use with NRand1 normal distr. generator */
float           XtraRand = 0;       /* ditto */
float           LastMean = 0;
float           LastSD   = 0;

#define  ran00valuesSize  100     /* used for shuffle */
float    ran00index, ran00max, ran00values[ran00valuesSize];

float URand01()	        /***** the basis for all the other generators ****/
{
#if (INTSZ == 16)
	long int lo, hi, test;
#else
	register int  lo, hi, test;
#endif
    register int    index;
    extern char     *GOutBuff;

        /* Use last random value to get index into 0..ran00valuesSize-1
           (index in thar range because ran00index < 1.0 and we truncate).
        */

    index = ran00valuesSize * ran00index;

    if ( index >= ran00valuesSize ) {
        sprintf( GOutBuff, "\n\nERR (URand01,cfsrand.c): index %d not in 0..%d!\n\n", index, ran00valuesSize-1 );
        WriteStd( GOutBuff );
        exit( 4 );
    }

    ran00index = ran00values[index];  /* get stored random number */
 
        /* get new random number into the slot used */

	hi = URndSd / RANDq;
	lo = URndSd % RANDq;
	test = (RANDa * lo) - (RANDr * hi);

	if ( test > 0 )
		URndSd = test;
	else
		URndSd = test + RANDm;

	        /* this was: ran00values[index] = 1.0 * URndSd / RANDm; */

    ran00values[index] = ( (float) URndSd / (float) RANDm );

        /* return the value extracted from the slot.
           The kludge to ensure a value a little less than 1,
           so index will be in range, and so NRandN will give correct value.
        */

    if ( ran00index == (float) 1.0 ) {
        ran00index = (float) 0.9999999;
    }

    return( ran00index );
 
}   /* end URand01 */


/**************************

InitRand	Initialize random number generator if needed.

	Called from Init_CFS() in INIT.C .

*******/

VOID	InitRand (  )
{
#if (INTSZ == 16)
	long int lo, hi, test;
#else
	register int  lo, hi, test;
#endif
    register int i;

        /* fill up the shuffle array */

    for ( i = 0; i < ran00valuesSize; ++i ) {
    	hi = URndSd / RANDq;
	    lo = URndSd % RANDq;
    	test = (RANDa * lo) - (RANDr * hi);

	    if ( test > 0 )
		    URndSd = test;
    	else
	    	URndSd = test + RANDm;

        ran00values[i] = ( (float) URndSd / (float) RANDm );
    }

        /* get the first ran00index value */

  	hi = URndSd / RANDq;
    lo = URndSd % RANDq;
   	test = (RANDa * lo) - (RANDr * hi);

    if ( test > 0 )
	    URndSd = test;
   	else
    	URndSd = test + RANDm;

    ran00index = ( (float) URndSd / (float) RANDm );

}   /* end InitRand */


/*
NRand1  Normal distribution generator--polar method.
        
Mean    Target mean.
SD      Target SD.

*/

float  NRand1 ( Mean, SD )
	float Mean, SD;
{
    register float  urand1, urand2, sum;

    if ( GotXRand && LastMean == Mean && LastSD == SD ) {
        GotXRand = FALSE;
		return ( XtraRand );
    }

    do {
        urand1 = (2.0 * URand01()) - 1;
        urand2 = (2.0 * URand01()) - 1;
        sum = ( (urand1*urand1) + (urand2*urand2) );
    }  while ( sum >= 1.0 );

    sum = sqrt( ( -2 * log( sum ) ) / sum );

        /* If we multiply urand1 or 2 by sum, we get mean 0, sd 1 rv.
           So we can modify it as called for.
        */

    GotXRand = TRUE;    /* we can save one and maybe use it later */
    LastMean = Mean;
    LastSD = SD;
    XtraRand = Mean + (urand1 * sum * SD);

    urand2   = Mean + (urand2 * sum * SD);

    return( urand2 );

} /* NRand1 */



float UNoise ( MaxDelta )       /* Uniform noise in small region */
	float MaxDelta;
{
	float noise, sign;

	if ( URand01() >= 0.5 ) 				/* get the sign */
		sign = -1.0;
	else
		sign = 1.0;
	noise = URand01();						/* another uniform in 0..1 */
	noise *= (sign * MaxDelta); 			/* scale MaxDelta and add sign */

	return( noise );						/* In -MaxDelta <= noise <= +MaxDelta */

} /* UNoise */



/*************

RandChoose  Choose n items from set of N, 0 < n <= N.

    Set     Array of N int's to choose from.
    Sample  Array into which n selected from Set can be placed.
    Return  Number actually chosen.

    Based on selection algorithms in Knuth, vol 2.

*****/

int  RandChoose( n, N, Sample, Set )
	int n, N, Sample[], Set[];
{
    register int    t;              /* number tried so far */
    register int    m;              /* number selected so far */
    register float  f;              /* for random number */
    extern float    URand01();      /* returns uniform random r, 0 <= r < 1 */

    if ( n < 0 || n > N ) {
        printf( "\n\n**ERR (RandChoose): illegal n %d or N %d.\n", n, N );
        return( 0 );
    }

    for ( t = m = 0; m < n && t < N; ++t ) {
        f = URand01();
            /* printf( "\nf %f, N %d, t %d, n %d, m %d: ", f, N, t, n, m ); */
        if ( (N - t) * f < n - m )  {
            /* printf( "Sample[m=%d] <- Set[t=%d]  (%d).", m, t, Set[t] ); */
            Sample[m++] = Set[t];
        }
    }

    if ( m < n ) {
        printf( "\n\n**ERR (RandChoose): only chose %d of n requested %d.\n", m, n );
    }

    return( m );

} /* end RandChoose */



/*************

RandPemute  permute an array of N int's.

    Based on shuffle/permutation algorithm in Knuth, vol 2.

*****/

VOID RandPermute( N, Set )
	int N, Set[];
{
    register int    k;              /* highest one to exchange */
    register int    j;              /* another one to exchange */
    register int    i;              /* temp for an exchange */
    register float  f;              /* for random number */
    extern float    URand01();      /* returns uniform random r, 0 <= r < 1 */

        /* work way from highest end, exchanging to lower elements */

    for ( k = N - 1; k >= 1; --k ) {
        f = URand01();              /* 0 <= f < 1 */
        j = f * k;                  /* 0 <= j < k (no sense exchanging j == k) */
        i = Set[j];                 /* exchange */
        Set[j] = Set[k];
        Set[k] = i;
    }

} /* end RandPermute */

