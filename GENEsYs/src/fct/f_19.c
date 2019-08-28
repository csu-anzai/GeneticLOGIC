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
 *	file:	f_19.c
 *
 *    author: 	Thomas Baeck
 *
 *   created:	13.08.91
 *
 *   purpose:	Objective function f_19, Shekel's function f_7
 *
 *  modified:
 *
 **TeX
 *	\Fct{Shekel-7}{TZ89}
 *	\Expr{f_{19}(\vec{x}) =
 *		- \sum_{i=1}^m \frac{1}{(x-A(i))(x-A(i))^T + c_i}}
 *	\begin{Cst}
 *		n = 4 \quad \;; \quad m = 7			\\
 *		0 \leq x_i \leq 10				\\
 *	\end{Cst}
 *	\begin{center}
 *		\begin{tabular}{|c|c|c|c|c|c|}\hline
 *		  $i$  & \multicolumn{4}{|c|}{$A(i)$} & $c_i$	\\ \hline
 *		  1 & 4 & 4 & 4 & 4 & 0.1			\\
 *		  2 & 1 & 1 & 1 & 1 & 0.2			\\
 *		  3 & 8 & 8 & 8 & 8 & 0.2			\\
 *		  4 & 6 & 6 & 6 & 6 & 0.4			\\
 *		  5 & 3 & 7 & 3 & 7 & 0.4			\\ 
 *		  6 & 2 & 9 & 2 & 9 & 0.6			\\
 *		  7 & 5 & 5 & 3 & 3 & 0.3			\\ \hline
 *		\end{tabular}
 *	\end{center}
 *	\Min{\min(f_{19}) = f_{19}(0,\ldots,0) = 0}
 **TeX
 */

#include "shekel.h"
#include "../extern.h"

#define	A	10.0	/* to shift global optimum to zero */

double f_19(x, n)
int n;
double *x;
{
	register	i, j;
	double		sp, h, result = 0.0;

	for (i = 0; i < 7; i++) {
		sp = 0.0;
		for (j = 0; j < 4; j++) {
			h   = x[j] - a[i][j];
			sp += h * h;
		}
		result += 1.0 / (sp + c[i]);
	}
	return(A - result);
}

/*** end of file ***/
