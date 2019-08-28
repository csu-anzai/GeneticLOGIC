/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990                                       	*/
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI       			*/
/*  University of Dortmund                                   	*/
/*  Baroper Str. 301                                         	*/
/*  D-4600 Dortmund 50						*/
/*								*/
/*  e-mail: baeck@gorbi.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/

/*
 *	file:	f_5.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:   objective function f_5, Shekel's foxholes
 *
 **TeX
 *	\Fct{Shekel's foxholes}{Jon75}
 *	\Expr{\frac{1}{f_5(\vec{x})} = \frac{1}{K} + \sum_{j=1}^{25}
 *		\frac{1}{c_j + \sum_{i=1}^{2} (x_i - a_{ij})^6}}
 *	\begin{Cst}
 *	(a_{ij}) = \left(
 *		\begin{array}{rrrrr@{\quad}rrrrr}
 *		-32 & -16 &   0 &  16 &  32  &  -32 & \cdots &  0 & 16 & 32 \\
 *		-32 & -32 & -32 & -32 & -32  &  -16 & \cdots & 32 & 32 & 32 \\
 *		\end{array}
 *	\right)								\\
 *	K = 500				\quad	\;;\quad
 *	f_5(a_{1j},a_{2j}) \approx c_j = j				\\
 *			-65.536 \leq x_i \leq 65.536
 *	\end{Cst}
 *	\Min{\min(f_5) = f_5(-32,-32) \approx 1}
 **TeX
 *
 *
 *
 *	$Id$
 *
 */

#include "../extern.h" 

static int a[2][25] ={
    {
        -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
        -32, -16, 0, 16, 32, -32, -16, 0, 16, 32    },
    {
        -32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
        16, 16, 16, 16, 16, 32, 32, 32, 32, 32    }
};

static 	double	K = 500.0;

double
f_05(x, n)
register double 	x[];
register int		n;

{
 
    	register int 	i,
			j;

    	double 		Sum = 0.0,
			Val,
			Dif,
			pow();

	for (Sum = 1.0 / K, j = 0; j < 25; j++) {
		for (Val = j + 1, i = 0; i < n; i++) {
			Dif  = x[i] - a[i][j];
            		Val += pow(Dif, 6.0);
        	}
		Sum += 1.0 / Val;
    	}
    	return (1.0 / Sum);
}

/** end of file **/ 
