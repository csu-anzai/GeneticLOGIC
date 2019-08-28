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
 *	file:	f_23.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   objective function f_23,
 *		a multimodal function (2 Optima) by Galar, mentioned in 
 *
 *		R. Galar: Simulation of local evolutionary dynamics of
 *		small populations.
 *
 **TeX	
 *	\Fct{Galar}{Gal91}
 *	\Expr{f_{23}(\vec{x}) = 
 *			(\exp(-5 x_1^2) + 2 \exp(-5 (1-x_1)^2)) \cdot
 *			\exp\left(-5 \sum_{i=2}^n x_i^2 \right)}
 *	\begin{Cst}
 *		-5.0 \leq x_i \leq 5.0					\\
 *	\end{Cst}
 **TeX
 *
 *
 *	$Id$
 *
 */


#include "../extern.h"

double
f_23(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;

    	double 		Val,
			Sum,
			exp();

	Sum = exp(- 1.0 * x[0] * x[0]) 
	      + 2.0 * exp(- 1.0 * (1.0 - x[0]) * (1.0 - x[0]));
	for (Val = 0.0, i = 1; i < n; i++) {
		Val += x[i] * x[i];
	}
	Sum *= exp(- 0.1 * Val);
    	return (Sum);
}

/** end of file **/
