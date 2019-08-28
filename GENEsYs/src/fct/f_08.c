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
 *	file:	f_8.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   Provide an environment that changes after a given number
 *		of generations (GFLIP). Basically this relies upon the
 *		function f_1, the optimum being changed from (0,...,0)
 *		to (4,...,4).
 *
 **TeX
 *	\Fct{Sphere model, changing environment}{HB91b}
 *	\Expr{f_8(\vec{x(t)}) = \left\{ 
 *		\begin{array}{r@{\quad:\quad}l}
 *			\sum_{i=1}^n x_i^2(t)  	 & 
 *				t \; \mbox{mod} \; a \; \mbox{even} \\
 *			\sum_{i=1}^n (x_i - b)^2 & 
 *				t \; \mbox{mod} \; a \; \mbox{odd}  
 *		\end{array} \right.}
 *	\begin{Cst}
 *		-5.12 \leq x_i \leq 5.12 				\\
 *		a = 250 \; \mbox{generations} \quad \;; \quad b = 4	\\
 *	\end{Cst}
 *	\Min{\min(f_8) = \left\{
 *		\begin{array}{r@{\quad:\quad}l}
 *			f_8(0,\ldots,0) & 
 *				t \; \mbox{mod} \; a \; \mbox{even} 	\\
 *			f_8(b,\ldots,b) & 
 *				t \; \mbox{mod} \; a \; \mbox{odd}
 *		\end{array} \right\} = 0}
 **TeX
 *
 *	$Id$
 *
 */

#include <math.h>
#include "../extern.h"

extern FUNCTION		f_tab[];

double
f_08(x, n)
register double 	x[];
register int		n;

{
 
    	extern   int 	Gen;
    	register int 	Ivl,
			i;
	double 		Opt,
			Sum;

	Ivl = (int) f_tab[F_nbr].CstVal[0];	/* get parameters */
	Opt = f_tab[F_nbr].CstVal[1];

	if (Opt < f_tab[F_nbr].umin)		/* correct, if necessary */
		Opt = f_tab[F_nbr].umin;
	if (Opt > f_tab[F_nbr].umax)
		Opt = f_tab[F_nbr].umax;

        if (Gen < Ivl) {
    		for (Sum = 0.0, i = 0; i < n; i++) {
			Sum += x[i]*x[i];
		}
	}
	else {
    		for (Sum = 0.0, i = 0; i < n; i++) {
			Sum += (Opt - x[i]) * (Opt - x[i]);
		}
    	}
    	return (Sum);
}

/*** end of file ***/
