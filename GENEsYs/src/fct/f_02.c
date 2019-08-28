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
 *	file:	f_2.c
 *
 *    author: 	Thomas Baeck
 *
 *	f2 - Generalized Rosenbrock's Function:
 *
 *	For n=2 function f2 comes out as the traditional Rosenbrock Function.
 *	f2 with n=2 was likewise defined by De Jong. 
 *
 *	It is a general test function for optimization methods,
 *	which is also used by many other authors.
 *
 *
 **TeX
 *		\Fct{Generalized Rosenbrock's function}{Jon75}
 *		\Expr{f_2(\vec{x}) = \sum_{i=1}^{n-1} 
 *			(100 \cdot (x_{i+1} - x_i^2)^2 + (x_i - 1)^2)}
 *		\begin{Cst}
 *			-5.12 \leq x_i \leq 5.12
 *		\end{Cst}
 *		\Min{\min(f_2) = f_2(1,\ldots,1) = 0}
 **TeX
 *
 *	f2 is a scalable function with n >= 2.
 *
 *
 *	$Id$
 *
 */

#include "../extern.h" 

double
f_02(x, n)
register double 	x[];
register int		n;

{

    	register int 	i; 

    	double 		Sum,
			Hlp1,
			Hlp2;

	for (Sum = 0.0, i = 0; i < (n - 1); i++) {
		Hlp1  = x[i] * x[i] - x[i+1];
		Hlp2  = (1.0 - x[i]) * (1.0 - x[i]);
		Sum  += (100 * Hlp1 * Hlp1 + Hlp2);
	}

    	return (Sum);
}

/** end of file **/
