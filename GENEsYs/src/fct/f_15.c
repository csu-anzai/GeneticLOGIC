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
 *	file:	f_15.c
 *
 *    author: 	Thomas Baeck, 	May 26., 1991
 *
 *   purpose:   objective function f_15, the weighted sphere model.
 *
 **TeX
 *	\Fct{Weighted sphere model}{Schw88}
 *	\Expr{f_{15}(\vec{x}) = \sum_{i=1}^n i \cdot x_i^2}
 *	\begin{Cst}
 *		-5.12 \leq x_i \leq 5.12
 *	\end{Cst}
 *	\Min{\min(f_{15}) = f_{15}(0,\ldots,0) = 0}
 **TeX
 *
 *	$Id$
 *
 */


#include "../extern.h"

double
f_15(x, n)
register double		x[];
register int		n;


{
 
    	register int 	i;
    	double 		Sum;

    	for (Sum = 0.0, i = 0; i < n; i++) {
        	Sum += x[i] * x[i] * ((double) (i + 1));	
    	}

    	return (Sum);
}

/** end of file **/
