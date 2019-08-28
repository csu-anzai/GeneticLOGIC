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
 *	file:	f_9.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   objective function f_9, a multimodal function, for which a
 *		path towards the global optimum exists, such that it
 *		always becomes steeper.
 *
 *		f9(x1,...,xn) = - A * exp(-B * sqrt(Val1_{i=1}^n xi^2))
 *				- exp(Val1_{i=1}^n cos(C * xi))
 *			 	+ A + exp(n)
 *				
 *				A = 40, B = 0.2, C = Pi
 *
 *		global minimum: f9(0,...,0) = 0
 *
 *		David H. Ackley: A connectionist machine for genetic
 *		hillclimbing, Kluwer Academic Publishers, Boston 1987
 *
 **TeX	
 *	\Fct{Ackley's function}{Ack87}
 *	\Expr{f_9(\vec{x}) = - a \cdot \exp\left(-b
 *			\sqrt{\frac{1}{n} \cdot \sum_{i=1}^n x_i^2}\right)  
 *			- \exp\left(\frac{1}{n} \cdot \sum_{i=1}^n 
 *			\cos(c\cdot x_i)\right) +a+e}
 *	\begin{Cst}
 *		a = 20 \quad \;; \quad b = 0.2 \quad \;; \quad c = 2\pi	\\
 *		-32.768 \leq x_i \leq 32.768				\\
 *	\end{Cst}
 *	\Min{\min(f_9) = f_9(0,\ldots,0) = 0}
 **TeX
 *
 *
 *	$Id$
 *
 */


#include "../extern.h"

extern FUNCTION		f_tab[];

double
f_09(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i;

    	double 		A, B, D,
			Val1,
			Val2,
			Sum,
			exp(),
			sqrt(),
			cos();

	A = f_tab[F_nbr].CstVal[0];
	B = f_tab[F_nbr].CstVal[1];
	D = f_tab[F_nbr].CstVal[2];
    	for (Val1 = Val2 = 0.0, i = 0; i < n; i++) {
        	Val1 += x[i] * x[i];	
		Val2 += cos(D * x[i]);
    	}
	Sum = 	- A * exp(-B * sqrt(Val1 / n)) - exp(Val2 / n) + A + M_E;

    	return (Sum);
}

/** end of file **/
