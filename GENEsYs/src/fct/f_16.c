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
 *	file:	f_16.c
 *
 *    author: 	Thomas Baeck,	27. jun. '91	
 *
 *   purpose:	Function according to Fletcher and Powell (1963)
 *
 *
 **TeX	
 *	\Fct{Fletcher and Powell}{}
 *	\Expr{f_{16}({\vec x}) = \sum_{i=1}^n (A_i - B_i)^2}
 *	\Expr{A_i = \sum_{j=1}^n (a_{ij}\sin \alpha_j + b_{ij}\cos \alpha_j)}
 *	\Expr{B_i = \sum_{j=1}^n (a_{ij}\sin x_j + b_{ij}\cos x_j)}
 *	\begin{Cst}
 *		a_{ij}, b_{ij} \in [-100,100]				\\
 *		\alpha_j \in [-\pi, \pi]				\\
 *	\end{Cst}
 *	$$
 *		(a_{ij}) = \left( \begin{array}{rrrrr}
 *			-25.5668 & 96.1559 & -15.2661 & -23.247 & 1.15129 \\
 *			-97.5908 & -82.9516 & 50.3364 & -96.552 & 29.4616 \\
 *			36.3096 & 54.3934 & 76.9586   & 64.3481 & 0.66188 \\
 *			-58.7916 & 30.4881 & -22.9354 & 36.1643 & 62.2798 \\
 *			66.582   & -18.2818 & 2.30184 & -38.6698 & -80.3784
 *			   \end{array} \right)				\\
 *	$$
 *	$$
 *		(b_{ij}) = \left( \begin{array}{rrrrr}
 *			-38.9994 & -49.1546 & -52.2403 & 66.4195 & -36.6538 \\
 *			-6.49429 & -97.6865 & 39.6734 & -91.4542 & 17.2329  \\
 *			-67.1681 & -42.8325 & -42.9307 & 36.9705 & -18.3463 \\
 *			-53.1271 & -2.74637 & 27.6244 & -72.7537 & 46.1798  \\
 *			-1.47036 & -95.2547 & 78.6214 & -54.3113 & 84.6737
 *			\end{array} \right)				\\
 *	$$
 *	$$
 *		(\alpha_j) = (1.98961, -1.39376, -2.03048, -0.621016, 
 *			      3.12736)
 *	$$
 *		
 **TeX
 *
 *
 *	$Id$
 *
 */

#include "../extern.h"

#define _F16DIM 	5

static double Avl[_F16DIM][_F16DIM] = { 
	{-25.5668, 96.1559, -15.2661, -23.247, 1.15129},
	{-97.5908, -82.9516, 50.3364, -96.552, 29.4616},
	{36.3096, 54.3934, 76.9586, 64.3481, 0.66188},
	{-58.7916, 30.4881, -22.9354, 36.1643, 62.2798},
	{66.582, -18.2818, 2.30184, -38.6698, -80.3784}
};

static double Bvl[_F16DIM][_F16DIM] = { 
	{-38.9994, -49.1546, -52.2403, 66.4195, -36.6538},
	{-6.49429, -97.6865, 39.6734, -91.4542, 17.2329},
	{-67.1681, -42.8325, -42.9307, 36.9705, -18.3463},
	{-53.1271, -2.74637, 27.6244, -72.7537, 46.1798},
	{-1.47036, -95.2547, 78.6214, -54.3113, 84.6737}
};

static double Aph[_F16DIM] = { 
	1.98961, -1.39376, -2.03048, -0.621016, 3.12736
};




double
f_16(x, n)
register double		x[];
register int		n;

{
	register int	i,
			j;

	double		Res,
			Val1,
			Val2,
			sin(),
			cos();

	for (Res = 0.0, i = 0; i < n; i++) {
		for (Val1 = Val2 = 0.0, j = 0; j < n; j++) {
			Val1 += (Avl[i][j] * sin(Aph[j]) 
				+ Bvl[i][j] * cos(Aph[j]));
			Val2 += (Avl[i][j] * sin(x[j]) + Bvl[i][j] * cos(x[j]));
		}
		Res += (Val1 - Val2) * (Val1 - Val2);
	}
	return(Res);

} /* end f_16 */
