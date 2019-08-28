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
 *	file:	f_3.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:	objective function f_3, step function
 *
 **TeX
 *	\Fct{Step function}{Jon75}
 *	\Expr{f_3(\vec{x}) = 6 \cdot n + \sum_{i=1}^n \lfloor x_i \rfloor}
 *		\begin{Cst}
 *			-5.12 \leq x_i \leq 5.12
 *		\end{Cst}
 *	\Min{\min(f_3) = f_3([-5.12,-5),\ldots,[-5.12,-5)) = 0}
 **TeX
 *
 *	$Id$
 *
 */

#include "../extern.h" 


double
f_03(x, n)
register double 	x[];
register int		n;

{

    	register int 	i; 

    	double 		Sum,
			floor();

    	for (Sum = 30.0, i = 0; i < n; i++) {
		Sum += floor(x[i]);	
    	}


    	return (Sum);
}

/** end of file **/
