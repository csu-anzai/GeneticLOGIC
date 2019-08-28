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
 *	file:	f_7.c
 *
 *    author: 	Thomas Baeck, 23 dec 90
 *
 *   purpose: 	objective function f_7, generalized Rastrigin's function.
 *
 **TeX
 *	\Fct{Generalized Rastrigin's function}{TZ89}
 *	\Expr{f_7(\vec{x}) =
 *		nA +  \sum_{i=1}^{n} x_i^2 - A \cos(\omega x_i)}
 *	\begin{Cst}
 *	A = 10				\quad	\;;\quad  \omega = 2\pi  \\
 *	-5.12 \leq x_i \leq 5.12					 \\
 *	\end{Cst}
 *	\Min{\min(f_7) = f_7(0,\ldots,0) = 0}
 **TeX
 *
 *	$Id$
 *
 */

#include <math.h>
#include "../extern.h"

#define OMEGA 	6.28318530717958647688
#define AMP	10.0


double
f_07(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;
    	double 		Sum,
    	   		cos();

    	for (Sum = 0.0, i = 0; i < n; i++) {
		Sum += x[i] * x[i] - AMP * cos(OMEGA * x[i]);
    	}

	Sum += n * AMP;
	return(Sum);
}

/** end of file **/
