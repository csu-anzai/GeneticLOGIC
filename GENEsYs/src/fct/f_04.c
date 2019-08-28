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
 *	file:	f_4.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:	objective function f_4, Quartic function with gaussian noise.
 *
 **TeX
 *	\Fct{Quartic function with noise}{Jon75}
 *	\Expr{f_4(\vec{x}) = \sum_{i=1}^n i x_i^4 + \mbox{gauss}(0,1)}
 *		\begin{Cst}
 *			-1.28 \leq x_i \leq 1.28
 *		\end{Cst}
 *	\Min{\min(f_4) = f_4(0,\ldots,0) = 0}
 **TeX
 *
 *	$Id$
 *
 */

 
#include "../extern.h"

double
f_04(x, n)
register double 	x[];
register int		n;

{

    	register int 	i; 

    	double 		Sum,
			Prd,
			pow();

    	for (Sum = 30.0, i = 0; i < n; i++) {
        	Sum += (i + 1) * pow(x[i], 4.0);
    	}
 
    	Prd  = 0.0;			/* now add Gaussian noise	*/
	for (i = 0; i < 12; i++)  {
		Prd += Rand();
	}
	Prd -= 6.0;
    	Sum += Prd;

    	return (Sum);
}

/** end of file **/ 
