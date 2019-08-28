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
 *	file:	f_18.c
 *
 *    author: 	Thomas Baeck
 *
 *   created:	13.08.91
 *
 *   purpose:	Objective function f_18, Shekel's function f_5
 *
 *  modified:
 *
 **TeX
 *	\Fct{Shekel-5}{TZ89}
 *	\Expr{f_{18}(\vec{x}) =
 *		- \sum_{i=1}^m \frac{1}{(x-A(i))(x-A(i))^T + c_i}}
 *	\begin{Cst}
 *		n = 4 \quad \;; \quad m = 5			\\
 *		0 \leq x_i \leq 10				\\ 
 *	\end{Cst}
 *	\begin{center}
 *		\begin{tabular}{|c|c|c|c|c|c|}\hline
 *		  $i$  & \multicolumn{4}{|c|}{$A(i)$} & $c_i$	\\ \hline
 *		  1 & 4 & 4 & 4 & 4 & 0.1			\\
 *		  2 & 1 & 1 & 1 & 1 & 0.2			\\
 *		  3 & 8 & 8 & 8 & 8 & 0.2			\\
 *		  4 & 6 & 6 & 6 & 6 & 0.4			\\
 *		  5 & 3 & 7 & 3 & 7 & 0.4			\\ \hline
 *		\end{tabular}
 *	\end{center}
 *	\Min{\min(f_{18}) = f_{18}(0,\ldots,0) = 0}
 **TeX
 *	
 *
 */

#include "shekel.h"
#include "../extern.h"

#define	A	10.0	/* to shift global optimum to zero */

double f_18(x, n)
int n;
double *x;
{
	register	i, j;
	double		sp, h, result = 0.0;

	for (i = 0; i < 5; i++) {
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
