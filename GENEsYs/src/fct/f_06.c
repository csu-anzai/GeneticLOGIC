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
 *	file:	f_6.c
 *
 *    author: 	Thomas Baeck, 23 jul 90
 *
 *   purpose:   objective function f_6, problem 1.2 from Schwefel.
 *		min: f_6(0,...,0) = 0
 *
 **TeX
 *	\Fct{Schwefel's function 1.2}{Schw81}
 *	\Expr{f_6(\vec{x}) =	\sum_{i=1}^{n} \left(
 *				\sum_{j=1}^{i} x_j
 *			\right)^2
 *		     =  x^{\rm T} {\rm \bf A} x + b^{\rm T} x}
 *	\begin{Cst}
 *		-65.536 \leq x_i \leq 65.536
 *	\end{Cst}
 *	\Min{\min(f_6) = f_6(0,\ldots,0) = 0}
 **TeX
 *
 *	$Id$
 *
 */

#include "../extern.h"


double
f_06(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;

    	double 		Sum,
			Val;

	for (Sum = Val = 0.0, i = 0; i < n; i++) {
        	Val += x[i];
		Sum += Val * Val;
    	}
	return (Sum);

}

/** end of file **/
