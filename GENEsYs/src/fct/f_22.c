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
 *	file:	f_22.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   objective function f_22 (n = 10),
 *		a multimodal function by Griewank, mentioned in 
 *
 *		A. Toern, A. Zilinskas: Global optimization,
 *		Springer Verlag 1989, LNCS 350, page 186.
 *
 **TeX	
 *	\Fct{Griewank}{TZ89}
 *	\Expr{f_{22}(\vec{x}) = 
 *			\frac{1}{d} \; \sum_{i=1}^n x_i^2 -
 *			\prod_{i=1}^n \cos \left(\frac{x_i}{\sqrt{i}}\right)
 *			+ 1}
 *	\begin{Cst}
 *		d = 4000 \quad \;; \quad n = 10				\\
 *		-600.0 \leq x_i \leq 600.0				\\
 *	\end{Cst}
 *	\Min{\min(f_{22}) = f_{22}(0,\ldots,0) = 0}
 **TeX
 *
 *
 *	$Id$
 *
 */


#include "../extern.h"

#define D	4000.0


double
f_22(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;

    	double 		Val1,
			Val2,
			Sum,
			sqrt(),
			cos();

	for (Val1 = 0.0, Val2 = 1.0, i = 0; i < n; i++) {
		Val1 += x[i] * x[i];
		Val2 *= cos(x[i] / sqrt((double) (i + 1)) );
	}
	Sum = Val1 / D - Val2 + 1.0;
    	return (Sum);
}

/** end of file **/
