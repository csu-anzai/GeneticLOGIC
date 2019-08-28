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

#include "lice.h"

#define SQR(x) ((x)*(x))

	


static int a[2][25] ={
    {
        -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
        -32, -16, 0, 16, 32, -32, -16, 0, 16, 32    },
    {
        -32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
        16, 16, 16, 16, 16, 32, 32, 32, 32, 32    }
};

static 	double	K = 500.0;

unsigned char scale2color(Individual *ind, int len)
{
	int i;

	for (i = 0; i < 25; i++) {
		if (SQR(a[0][i]-ind->x[0]) + SQR(a[1][i]-ind->x[1]) < 64.0) {
			return(250-i*10);
		}
	}
	return 0;
}



Fitness eval(double *x, int n)
{
 
	Fitness         result;
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
	result.valid = 1;
	result.value = 1.0 / Sum;
    	return result;
}



/** end of file **/ 
