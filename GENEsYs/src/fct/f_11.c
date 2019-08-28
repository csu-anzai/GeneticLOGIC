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
 *	file:	f_11.c
 *
 *    author: 	Thomas Baeck, 04. Feb. '91
 *
 *		Low autocorrelation binary sequences test function.
 *
 *		Claas de Groot and Diethelm Wuertz and Karl Heinz Hoffman: 
 *		Low autocorrelation binary sequences: exact enumeration 
 *		and optimization by evolution strategies,
 *		Eidgenoessische Technische Hochschule Zuerich
 **TeX
 *	\Fct{Low autocorrelation binary sequences}{GWH89}
 *	\Expr{f_{11}(\vec{a}) = \sum_{k=1}^{l-1} \left(
 *				\sum_{i=1}^{l-k} \beta_i \beta_{i+k}
 *				\right)^2 \quad ; \quad	
 *		\beta_i = \left\{
 *		\begin{array}{r@{\hspace{0.5em},\hspace{0.5em}}l}
 *			-1	& \alpha_i = 0	\\
 *			 1	& \alpha_i = 1 	\\
 *		\end{array} \right.}
 **TeX
 *
 *	$Id$
 *
 */

#include "../extern.h"

double
f_11(x, Length)
register char		x[];
register int		Length;

{
 
    	register int 	k,
			i,
			Egy,	/* energy of a sequence	*/
			Cor;	/* correlation coefficient of a sequence */

	for (Egy = 0, k = 0; k < Length - 1; k++) {
		for (Cor = 0, i = 0; i < Length - k - 1; i++) {
			Cor += ((x[i] == x[i + k + 1]) ? 1 : -1);
		}
		Egy += Cor * Cor;
	}
	return ((double) Egy);
}	 

/*** end of file ***/
