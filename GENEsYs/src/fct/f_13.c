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
 *	file:	f_13.c
 *
 *    author: 	Thomas Baeck,	11. apr. '91
 *				15. jun. '91 	stochastic variant, options
 *				23. jun. '91	new variant
 *
 *   purpose:	Calculate approximate values for the Weierstrass-Mandelbrot
 *		fractal function.
 *		The function is described in:
 *			Jens Feder: Fractals, Plenum Press, New York 1988
 *
 **TeX	
 *	\Fct{Weierstrass-Mandelbrot fractal function}{BL80}
 *	\Expr{f_{13}(\vec{x}) = \sum_{i=1}^n  	\left(
 *		\frac{C(x_i)}{C(1) \cdot {|x_i|}^{2-D}} + x_i^2 - 1	
 *						\right)}
 *	\Expr{C(x) = 	\sum_{j=-\infty}^\infty 
 *			\frac{1-\cos\left(b^jx\right)}{b^{(2-D)j}}}
 *	\begin{Cst}
 *		1 \leq D \leq 2 \quad \;; \quad b > 1		\\
 *		-5.12 \leq x_i \leq 5.12			\\
 *	\end{Cst}
 **TeX
 *
 *
 *	$Id$
 *
 */

#include "../extern.h"

#define EPSILON	1.0e-8
#define IDXBND  100

#define GCLIM	1.570796	/* gamma - cos limes for vicinity of 0 */

typedef struct {				/* value pairs */
	double 	FstVal,
		SndVal;
} TwoVal;

static	double *RndAng[2 * IDXBND + 1];		/* random phase angles */

extern FUNCTION		f_tab[];

	
double
f_13(x, n)
register double		x[];
register int		n;

{
	static 	 int		Flg = 1;
	static   unsigned int	StoSed;
	static	 double		M_2PI = 2.0 * M_PI;

	register int		i,
				j;
	char		       *calloc();
    	double 			WMF();

	if (Flg) {

		/*
		 *  The old seed of the random number generator is saved for
		 *  later restauration. From this point on we are using the 
		 *  seed given to the function either as a parameter or by
		 *  standard initialization.
		 */

		StoSed = Seed;		
		Seed   = (unsigned) f_tab[F_nbr].CstVal[2];	

		/*
		 *  randomly initialize (IDXBND * 2 + 1) * n  angles 
		 *  in the range [0, 2pi] and store them in RndAng.
		 */

		for (i = 0; i < 2 * IDXBND + 1; i++) {
			if ((RndAng[i] = (double *) calloc ((unsigned) n,
					     sizeof(double))) == NULL) {
				printf("No space allocated\n");
				exit(1);
			}
			for (j = 0; j < n; j++) 
				RndAng[i][j] = M_2PI * Rand();
		}
		Seed   = StoSed;	/* restore rng state */
		Flg    = 0;
	}

	/*
	 *  options for the three last parameters in the following call:
	 *  general topics:  		0 no angles
	 *			  	1 random angles
	 *  superposition function: 	0 sphere model
	 *				1 exponential
	 *				2 nothing
	 *  trend elimination:		0 subtraction as described in [BL80]
	 *				1 division method by Schwefel
	 *				2 nothing
	 */

	return(WMF(
		x, 				/* object variable vector */
		n, 				/* dimension of x */
		IDXBND, 			/* bound for index values */
		f_tab[F_nbr].CstVal[0], 	/* Box Dimension */
		f_tab[F_nbr].CstVal[1],		/* value of constant b */
		f_tab[F_nbr].FctOpt[0].OVal,	/* modus of angle determ. */
		f_tab[F_nbr].FctOpt[1].OVal, 	/* superposition function */
		f_tab[F_nbr].FctOpt[2].OVal	/* trend elimination */
		));
 
} /* end f_13() */



double 
WMF(x, n, Bnd, Dim, b, Mode, NewTrd, ElmTrd)
register double		x[];		/* objective variables */
register int		n,		/* dimension of x[] */
			Bnd;		/* index bound */
register double		Dim,		/* box dimension */
			b;		/* value of b */
register int		Mode;		/* modus for angle determination */
register int		NewTrd,		/* new added trend */
			ElmTrd;		/* old trend to eliminate */

{

	double		Res,		/* result value */
			Twmf(),		/* superposed trend function */
			fabs(),
			exp();

	register int	i;

	/*
	 *  calculate function values by calling Twmf, and
	 *  add a new trend on the result.
	 */

	for (Res = 0.0, i = 0; i < n; i++) {
		Res += Twmf(i, x[i], Bnd, Dim, b, ElmTrd, Mode);
		switch (NewTrd) {	
			case 0: 	/* sphere model */
				Res += x[i] * x[i];
				break;

			case 1: 	/* exponential function */
				Res -= exp(- x[i] * x[i]);
				break;

			default:	/* no trend */
				break;
		}
	}

	return(Res);

} /* end WMF */




double
Twmf(Idx, x, Bnd, Dim, b, ElmTrd, Mode)
register int		Idx;		/* x vector index */
register double		x;		/* x value */
register int		Bnd;		/* index bound */
register double		Dim,		/* box dimension */
			b;		/* value of b */
register int		ElmTrd,		/* old trend to eliminate */
			Mode;		/* modus for angle determination */

{

	static 	int	Flg = 1;	/* flag for first call of Twmf */

 	static 	double	MTd,		/* multiplicative trend factor */
			STd;		/* subtractive trend summand */
			

	double 		Nor,		/* normalization value */
			Res,		/* result value */
			Val1,
			Val2, 
			cos(),
			pow(),
			log(),
			exp(),
			lgamma(),
			fabs(),
			wmf();

	if (Flg) {
		
		/*
		 * multiplicative trend calculation according to
		 * Schwefel's suggestion, May 1991
		 */

		MTd  = wmf(Idx, 1.0, Bnd, Dim, b, Mode);

		/* 
		 * subtractive trend calculation according to the trend
	 	 * expression given by Berry and Lewis [BL80]
		 */

		if (fabs(Dim - 1.0) < EPSILON) 	/* exception case */
			STd  = GCLIM / log(b);
		else {
			Val1 = cos((2.0 - Dim) * M_PI / 2.0) / 
			       ((2.0 - Dim) * log(b));
			Val2 = lgamma(Dim - 1.0);
			STd  = exp(Val2) * Val1;
		}
		Flg  = 0;
	}
	
	
	switch (ElmTrd) {	/* eliminate old trend */
		case 0:			

		/*
	 	 *  Subtract the trend according to the formula in [BL80].
		 *  Be aware of the divergence problems caused by the trend 
		 *  expression for Dim->2 as well as for b->1 !
		 */

			Nor = STd * pow(fabs(x), 2.0 - Dim);
			Res = wmf(Idx, x, Bnd, Dim, b, Mode) - Nor;
			break;

		case 1: 		

		/*
		 *  Divide by trend as calculated by Schwefel. 
		 *  The original function is symmetric w.r.t the origin, so 
		 *  even the trend must be symmetric, i.e. |x| must be used in
		 *  the normalization procedure. Also subtract 1 to have a
		 *  function value of 0 at the global optimum.
		 *  The exception case x->0 is handled by the trick to
		 *  calculate first and then to test for division by zero
		 *  (the isnan call below).
		 */

			Nor = MTd * pow(fabs(x), 2.0 - Dim); 
			Res = wmf(Idx, x, Bnd, Dim, b, Mode) / Nor - 1.0;
			break;

		default:		

		/* 
		 *  No enttreding measure is performed in this case.
		 */

			Res = wmf(Idx, x, Bnd, Dim, b, Mode);
			break;
	}

	return (isnan(Res) ? 0.0 : Res);

} /* end Twmf */


double
wmf(Idx, x, Bnd, Dim, b, Mode)
register int		Idx;		/* x vector index */
register double		x;		/* x value */
register int		Bnd;		/* index bound */
register double		Dim,		/* box dimension */
			b;		/* value of b */
register int		Mode;		/* angle determination modus */

{	
	static   int		Flg = 1;
	static   TwoVal        *ValArr;
	static	 double		M_2PI = 2.0 * M_PI;

	register int		i,
				k1,
				k2;

	register double		Old,
				Sum,
				Sum1,
				Sum2;

	char		       *calloc();

	double			pow(),
				cos(),
				fabs(),
				fmod(),
				ClcSum();

	if (Flg) {	/* calculate fixed values and store them */
		if ((ValArr = (TwoVal *) calloc ((unsigned) (2 * Bnd + 1), 
					     sizeof(TwoVal))) == NULL) {
			printf("No space allocated\n");
			exit(1);
		}
		for (i = 0; i < 2 * Bnd + 1; i++) {
			ValArr[i].FstVal = 
				pow(b, (double) (-Bnd + i));
			ValArr[i].SndVal = 
				pow(b, ((double) (-Bnd + i)) * (2.0 - Dim));
		}
		Flg  = 0;
	}

	for (	Sum = ClcSum(x, Idx, Bnd, ValArr[Bnd].FstVal, 
					  ValArr[Bnd].SndVal,
					  Mode),
		Old = 0.0,
	     	i   = 1;
	       (i  <= Bnd) && (fabs(Sum - Old) > EPSILON * fabs(x));
	     	i++) {
			Old   = Sum;
			k1    = Bnd + i;
			k2    = Bnd - i;
			Sum1  = ClcSum(x, Idx, k1, ValArr[k1].FstVal, 
					ValArr[k1].SndVal, Mode);
			Sum2  = ClcSum(x, Idx, k2, ValArr[k2].FstVal, 
					ValArr[k2].SndVal, Mode);
			Sum  += (Sum1 + Sum2);
	}
	return(Sum);

} /* end wmf */





double 
ClcSum(x, XIdx, VIdx, Fst, Snd, Mode)
register int 		XIdx,		/* index of value x */
			VIdx;		/* index in the ValArr */
register double		x,		/* x value */
			Fst,		/* FstVal in ValArr */
			Snd;		/* SndVal in ValArr */
register int		Mode;		/* calculation mode */

{
	static double	M_2PI = 2.0 * M_PI;
	double		Res,		/* function result */
			Phi,		/* phase angle */
			Val1,
			Val2,
			cos(),
			sin(),
			fmod();

	switch (Mode) {
		case 0:		

		/*
		 *  The deterministic variant with all angles set 0.
		 */

			Res = (1.0 - cos(fmod(Fst * x, M_2PI))) / Snd;
			break;
		case 1:	

		/*
		 *  The stochastic variant. All angles have been calculated
		 *  in f_13 and are used here to evaluate the more general
		 *  expression based upon the real part of the Weierstrass-
		 *  Mandelbrot function.
		 */

			Phi  = RndAng[VIdx][XIdx];
			Val1 = cos(Phi) * (1.0 - cos(fmod(Fst * x, M_2PI)));
			Val2 = sin(Phi) * sin(fmod(Fst * x, M_2PI));
			Res  = (Val1 + Val2) / Snd;
			break;

		default:

		/*
		 *  Again the simpler deterministic variant.
		 */

			Res = (1.0 - cos(fmod(Fst * x, M_2PI))) / Snd;
			break;
	}
	return(Res);

} /* end ClcSum */


/*** end of file ***/
