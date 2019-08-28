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
 *	file:	f_12.c
 *
 *    author: 	Thomas Baeck, 27. Feb. '91
 *
 *		A simple binary testfunction (`mimikry problem').
 *		The global optimum is given by the bitstring (000000...000),
 *		the fitness function is simple defined as the Hamming distance
 *		to this optimum.
 *
 **TeX
 *	\Fct{Hamming distance to~$0^l$}{}
 *	\Expr{f_{12}(\vec{a}) = \sum_{i=1}^l \alpha_i}
 *	\Min{\min(f_{12}) = f_{12}(0,\ldots,0) = 0}
 **TeX
 *
 *	$Id$
 *
 *
 */

#include "../extern.h"


double
f_12(x, Length)
register char		x[];
register int		Length;

{
 
    	register int 	i,
			Dst;		/* distance to optimum */

	for (Dst = 0, i = 0; i < Length; i++) {
		Dst += (x[i]) ? 1 : 0;
	}

	return ((double) Dst);
}	 

/*** end of file ***/
