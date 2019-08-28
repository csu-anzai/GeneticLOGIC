/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/

/*
 *
 *	$Id$
 *	$Log$
 *
 *      file:   mutate.c
 *
 *    author: 	Thomas Baeck
 *
 *   created:	May 1991
 *
 *   purpose:	General mutation mechanism for the new GA
 *
 *  modified:	Thomas Baeck, 22 june 90  
 *			calculation of `Avg_M_rate' added, i.e. the average 
 *			mutation rate.
 *
 *		Thomas Baeck, 25 june 90  
 *			setting `M_bits' from the setup program instead 
 *			of DEFINE.
 *
 *		Thomas Baeck, 24 july 90  
 *			maintain minimum and maximum mutation rates.
 *
 *		Thomas Baeck, 14 sept 90  
 *			MAXMRATE introduced, i.e. the maximum mutation 
 *			rate which is allowed.
 *
 *		Thomas Baeck, 27 nov 90
 *			No longer use packed individuals.
 *
 *
 */
 
#include "extern.h"
 


void
GenMut()		/* generic mutation function */

{
	void 		AtvMtt(),
			Mutate();
	double		ClcOptMrt();

	switch (MttScm) {		/* mutation schemes */

		case STD_MTT:		/* normal */
			Mutate();
			break;

		case ADT_MTT:		/* adaptive, including first */
		case ADX_MTT:		/* adaptive, excluding first */
			AtvMtt();
			break;

		case OPT_MTT:		/* optimal mutation rate calculation */
			M_rate = ClcOptMrt((int) Best_current_perf, 
						Length, Mu, Popsize);
			Mutate();
			break;

		default:
			Mutate();
			break;
		
	} /* end switch */

} /* end GenMut */




void
Mutate()

{
    	static int 	Flg = 1,
			bits; 		/* number of bits per pop */

    	register int 	i,        	/* index of the Mutated structure */
			j,  		/* position within the structure */
			k;		/* a random allele */
 
	Trace("Mutate entered");
 
    	if (Flg) {
        	bits = Popsize * Length;
        	Flg  = 0;
    	}
 
    	if (M_rate > 0.0) {		/* Mutations are enabled */

        	while (Mu_next < bits) {
            		i = Mu_next / Length;   /* Mutated structure */
            		j = Mu_next % Length;  	/* Mutated position */
 
            		k = Randint(0,1);	/* choose a random allele */
 
            		if (k != New[i].Gene[j]) { /* an effective mutation */
                		New[i].Gene[j] 		= k;
                		New[i].Needs_evaluation = 1;
            		}
 
            		if (M_rate < 1.0) /* update next mutation location */
                		Mu_next += ceil (log(Rand()) / 
						 log(1.0 - M_rate));
            		else
                		Mu_next += 1;
        	}
        	Mu_next -= bits; /* adjust Mu_next for next Generation */
    	}
    	Trace("Mutate completed");

} /* end Mutate */
 

 
 
void
AtvMtt()

{
    	register int 	i,      	/* loop control */
    			j,      
			l,
			Pos,
			EffLen;		/* effective length of mutated part */

    	double   	Rat,
			DcdMttRat();

    	Trace("Adaptive mutate entered");

	if (NbrMttRts == 1) {
		EffLen = Length;
	}
	else {
		EffLen = ChrLen;
	}

	for (i = 0; i < Popsize; i++) {  

		for (j = 0; j < NbrMttRts; j++) { /* each mutation rate */

			switch (MttScm) {

				case ADX_MTT:	/* mutate with constant rate */
					Rat = M_rate;
					break;

				case ADT_MTT:	/* mutate with its own rate */
					Rat = New[i].MttRts[j];
					break;

				default:
					Rat = M_rate;
					break;

			}  /* end switch */

			for (l = 0; l < M_bits; l++) { /* mutate mutation rate*/
    
				if (Rand() <= Rat) {	/* mutate at Pos */
					Pos = j ? j * M_bits + l : l;
					if (New[i].MttGen[Pos] == ((char) 0)) 
						New[i].MttGen[Pos] = 1;
					else
						New[i].MttGen[Pos] = 0;
		    		}

	    		} /* end for; mutation rates are mutated now */

			New[i].MttRts[j] 
				 = DcdMttRat(&(New[i].MttGen[M_bits * j]));
			Rat = New[i].MttRts[j];

			/* mutate associated genotype information */

			for (l = 0; l < EffLen; l++) {
	    			if (Rand() <= Rat) {	
					Pos = j ? j * EffLen + l : l;
					if (New[i].Gene[Pos] == ((char) 0)) 
						New[i].Gene[Pos] = 1;
					else
						New[i].Gene[Pos] = 0;
        	    			New[i].Needs_evaluation = 1;
	    			}
			} /* end for; object variables are mutated now */

		} /* end for */

    	} /* end for */
 
    	Trace("Adaptive mutate completed");

} /* end AtvMtt */




double
DcdMttRat(Gen)
char		       *Gen;		/* Mutation rate genotype */

{
	extern int  	Ctoi();

	static int	Flg = 1;
	static double	MaxDbl;		/* maximum double value */
	static char    *Buf;

	char	       *malloc();

	double		pow();

	register int	Rat;		/* integer decoded mutation rate */

	void		Degray();

	if (Flg) {
		if ((Buf = malloc((unsigned) M_bits)) == NULL) {
			Error("DcdMttRat: No space");
		}
		MaxDbl = pow(2.0, (double) M_bits) - 1.0;	
		Flg = 0;
	}

	Degray( &(Gen[0]), Buf, M_bits);
	Rat = Ctoi( Buf, M_bits);
	
	return(MAXMRATE * ((double) Rat) / MaxDbl);
		
} /* end DcdMttRat */
 

#define	PLUS	0
#define COMMA	1

double
ClcOptMrt(	Bst,		/* best objective function value */
		Len,		/* length of individuals */
		Mu,		/* number of parents */
		Lda)		/* number of offspring */

int		Bst,
		Len,
		Mu,
		Lda;

{
	double	golden(), 
		x1   = 0.0, 
		x2   = 1.0, 
		xmid = 0.1,
		fmin,
		min;

	int	Strategy = COMMA;

	if (Eliteflag && (Mu == 1))	/* eventually switch to plus */
		Strategy = PLUS;

	/*
	 *  setup the call for golden search optimization of the expected
	 *  progress for this specific situation.
	 */

	if (Gen == 0)			/* initially best mutation rate */
		return 0.5;

	if (Bst > 0) {			/* calculate mutation rate */
		fmin = golden(	x1, xmid, x2, 1.0e-2, &min, 
				Len, Len - Bst, Mu, Lda, Strategy);
		return min;
	}

	return 0.0;			/* mutation rate is zero */

} /*** end ClcOptMrt ***/


#define R			0.61803399
#define C			(1.0 - R)
#define	SHFT2(a, b, c)		(a)=(b);(b)=(c);
#define SHFT3(a, b, c, d)	(a)=(b);(b)=(c);(c)=(d);

double
golden(	ax, bx, cx, tol, xmin, length, ones, Mu, Lda, Str)

double	ax, bx, cx, tol, *xmin;
int	length, ones, Mu, Lda, Str;

{
	double	f0, f1, f2, f3, x0, x1, x2, x3,
		Progress();

	x0 = ax;
	x3 = cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1 = bx;
		x2 = bx + C * (cx-bx);
	}
	else {
		x2 = bx;
		x1 = bx - C * (bx-ax);
	}
	f1   = - Progress(length, ones, Mu, Lda, Str, x1);
	f2   = - Progress(length, ones, Mu, Lda, Str, x2);
	while (fabs(x3 - x0) > tol * (fabs(x1) + fabs(x2))) {
		if (f2 < f1) {
			SHFT3(x0, x1, x2, R*x1 + C*x3);
			SHFT2(f1, f2, 
				- Progress(length, ones, Mu, Lda, Str, x2));
		}
		else {
			SHFT3(x3, x2, x1, R*x2 + C*x0);
			SHFT2(f2, f1, 
				- Progress(length, ones, Mu, Lda, Str, x1));
		}
	}
	if (f1 < f2) {
		*xmin = x1;
		return -f1;
	}
	else {
		*xmin = x2;
		return -f2;
	}

} /*** end golden ***/




double
Progress(	Len,		/* total length */
		One,		/* number of ones */
		Mu,		/* mu, number of parents */
		Lda,		/* lambda, number of offspring */
		Strategy,	/* selection strategy */
		Prb)		/* mutation probability */

int		Len,
		One,
		Mu,
		Lda,
		Strategy;
double		Prb;

{
	double	sum, s1, s2, h1, h2, 
		choose(),
		EqPrb(),		/* probability to be equal */
		BtrPrb(),		/* probability to be better */
		Pow();

	int	i, j, k, Start;
 
	if (Strategy == PLUS)
		Start = 1;
	else
		Start = - One;

	if ((Lda == 1) && (Mu == 1)) {
		for (sum = 0.0, i = Start; i <= Len - One; i++)
			sum += ((double) i) * EqPrb(Len, One, i, Prb);
		return sum;
	}
	if ((Lda > 1) && (Mu == 1)) {
		for (sum = 0.0, i = Start; i <= Len - One; i++) {
			h2   =  EqPrb(Len, One, i, Prb);
			s1   = BtrPrb(Len, One, i, Prb);
			h1   = Pow(1.0 - s1 - h2 , ((double) (Lda - 1)));
			sum += ((double) i) * h2 * h1;
		}
		return (sum * ((double) Lda));
	}

	for (sum = 0.0, i = Start; i <= Len - One; i++) {
		h2   =  EqPrb(Len, One, i, Prb);
		s1   = BtrPrb(Len, One, i, Prb);
		for (s2 = 0.0, k = 1; k <= Mu; k++) {
			h1   = Pow(1.0 - s1 - h2, ((double) (Lda - k)));
			h1  *= Pow(s1, ((double) (k - 1)));
			h1  *= choose(Lda - 1, k - 1);
			s2  += h1;
		}
		sum += ((double) i) * h2 * s2;
	}
	
	return (sum * ((double) Lda) / ((double) Mu));

} /*** end Progress ***/



	/*              
 	 *	n! / (k! * (n-k)!)
 	 */

double
choose (n, k)
int     n, k;
{
	double  p1, p2, p3,
		exp(),
		lgamma();

	if (n < k)
		return 0.0;		/* this is quicker */
	p1 = lgamma(((double) (n + 1)));
	p2 = lgamma(((double) (k + 1)));
	p3 = lgamma(((double) (n - k + 1)));

	return exp(p1 - p2 - p3);

} /*** end choose ***/



double	
Pow(	x,				/* basis */
	y)				/* exponent */

double		x,
		y;

{
	double	h,
		fabs(),
		log(),
		exp();
	
	if ((x < 0.0) || (fabs(x) < 1.0e-20))
		return 0.0;

	h = y * log(x);
	return exp(h);

} /*** end Pow ***/



double
BtrPrb(		Len,		/* total length */
		One,		/* number of ones */
		k,		/* actual value */
		p)		/* mutation probability */

int		Len,
		One,
		k;
double		p;

{
	int	i;

	double	sum,
		kscPrb(),
		knsPrb();

	if (k < 0) {
		for (sum = 0.0, i = k+1; i <= -1; i++) 
			sum += knsPrb(Len, One, -i, p);
		for (i = 0; i <= Len - One; i++)
			sum += kscPrb(Len, One,  i, p);
		return sum;
	}
	else {
		for (sum = 0.0, i = k+1; i <= Len - One; i++)
			sum += kscPrb(Len, One,  i, p);
		return sum;
	}

} /*** end BtrPrb ***/



double
EqPrb(		Len,		/* total length */
		One,		/* number of ones */
		k,		/* actual value */
		p)		/* mutation probability */

int		Len,
		One,
		k;
double		p;

{
	double	kscPrb(),
		knsPrb();

	if (k < 0)
		return knsPrb(Len, One, -k, p);		/* no success */
	else
		return kscPrb(Len, One,  k, p);		/* success */

} /*** end EqPrb ***/




double
knsPrb(		l,		/* length of bitstring */
		f,		/* number of ones */
		k,		/* worsening */
		p) 		/* mutation probability */

int		l,
		f,
		k;
double		p;


{
	double		s1, h1,			/* intermediate sums */
			choose(),
			Pow();

	int		i,
			n = l - f;

	for (s1 = 0.0, i = 0; i <= n; i++) {

		h1  = choose(n, i);
		h1 *= choose(f, i + k);
		h1 *= (	  Pow(p, (double) (2 * i + k))
			* Pow(1.0 - p, (double) (l - 2 * i - k)));
		s1 += h1;
	}

	return s1;

} /*** end knsPrb ***/



double
kscPrb(		l,		/* length of bitstring */
		f,		/* number of ones */
		k,		/* improvement */
		p) 		/* mutation probability */

int		l,
		f,
		k;
double		p;


{
	double		h1, s1, h2,		/* intermediate sums */
			choose(),
			Pow();

	int		i,
			n = l - f;

	for (s1 = 0.0, i = 0; i <= f; i++) {

		h1  = choose(f, i);
		h1 *= (	Pow(p, (double) i) * 
			Pow(1.0 - p, (double) (f - i)));
		h2  = choose(n, i + k);
		h2 *= (	Pow(p, (double) (i + k)) * 
			Pow(1.0 - p, (double) (l - f - i - k)));
		s1 += (h1 * h2);
	}

	return s1;
}


/*** end of file ***/
