/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1992                                     	*/
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
 *	file:	f_24.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   objective function f_24,
 *		a multimodal function which arises from a nonlinear
 * 		parameter estimation problem of dimension 4.
 *
 **TeX	
 *	\Fct{Kowalik}{Schw77}
 *	\Expr{f_{24}(\vec{x}) = \sum_{i=1}^{11} \left(
 *		a_i - \frac{x_1 (b_i^2 + b_i x_2)}{b_i^2 + b_ix_3 + x_4}
 *		\right)^2}
 *	\begin{Cst}
 *		n = 4							\\
 *		-5.0 \leq x_i \leq 5.0					\\
 *	\end{Cst}
 *	\begin{center}
 *		\begin{tabular}{|c|c|c|}\hline
 *			$i$	& $a_i$		& $b_i^{-1}$	\\ \hline
 *			1	& 0.1957	& 0.25		\\
 *			2	& 0.1947	& 0.5		\\
 *			3	& 0.1735	& 1		\\
 *			4	& 0.1600	& 2		\\
 *			5	& 0.0844	& 4		\\
 *			6 	& 0.0627	& 6		\\
 *			7	& 0.0456	& 8		\\
 *			8 	& 0.0342	& 10		\\
 *			9	& 0.0323	& 12		\\
 *			10	& 0.0235	& 14		\\
 *			11	& 0.0246	& 16		\\ \hline
 *		\end{tabular}
 *	\end{center}
 *	\Min{\min(f_{24}) \approx f_{24}(0.1928, 0.1908, 0.1231, 0.1358)
 *			  \approx 0.0003075}
 **TeX
 *
 *
 *	$Id$
 *
 */


#include "../extern.h"

static double a[11] = {
	0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 
	0.0456, 0.0342, 0.0323, 0.0235, 0.0246 };

static double b_inv[11] = {
	0.25, 0.5, 1.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0 };


double
f_24(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;

    	double 		d1,
			d2,
			Sum;

	for (Sum = 0.0, i = 0; i < n; i++) {
		d1 = 1.0 / (b_inv[i] * b_inv[i]);
		d2 = a[i] - (x[0] * (d1 + x[1] / b_inv[i]) /
		    	    (d1 + x[2] / b_inv[i] + x[3]));
		Sum += (d2 * d2);
	}
    	return (Sum);
}

/** end of file **/
