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
 *	file:	f_14.c
 *
 *    author: 	Thomas Baeck, 27. Feb. '91
 *
 *		A fully deceptive function according to the article:
 *			Deceptiveness and Genetic Algorithm Dynamics,
 *			Gunar E. Liepins and Michael D. Vose, 1991
 *			Here: Modified to minimization tasks
 **TeX
 *	\Fct{Fully deceptive function}{LV91a}
 *	\Expr{f_{14}(\vec{a}) = \left\{
 *		\begin{array}{c@{\hspace{0.5em},\hspace{0.5em}}l}
 *			2^{-l}		& f_{12}(a) = 0		\\
 *			(1 + o(a))/l	& 0 < f_{12}(a) < l	\\
 *			0		& f_{12}(a) = l		\\
 *		\end{array} \right.}
 **TeX
 *
 *	$Id$
 *
 *
 */

#include "../extern.h"


double
f_14(x, Length)
register char		x[];
register int		Length;

{
 
    	register int 	i,
			Ord;		/* order of the string */

	double		pow();

	for (Ord = 0, i = 0; i < Length; i++) {
		Ord += (x[i]) ? 1 : 0;
	}
	if (Ord == 0)
		return(pow(2.0, - (double) Length));

	if (Ord == Length)
		return(0.0);

	return (((double) (1 + Ord)) / ((double) Length));
}	 

/*** end of file ***/
