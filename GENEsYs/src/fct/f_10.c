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
 *	file:	f_10.c
 *
 *    author: 	Thomas Baeck, 30. Jan '91
 *
 *		Krolaks 100 city reference TSP; optimum tour length 21282.
 *
 *		P. Krolak and W. Felts and G. Marble:
 *		A man-machine approach towards solving the traveling salesman
 *		problem, CACM 14(5), 1971, pp. 327-334
 *
 **TeX
 *	\Fct{Krolak's~$100$ city TSP}{KFM71}
 *	\Expr{f_{10}(\vec{x}) = \sum_{i=1}^n 
 *		 	d(c_{\pi(i \; {\rm mod} \; n)}, c_{\pi((i+1) \; 
 *			{\rm mod} \; n)})}
 *	\begin{Cst}
 *			-5.12 \leq x_i \leq 5.12
 *	\end{Cst}
 *	\Min{\min(f_{10}) = 21285}
 **TeX
 *
 *	$Id$
 *
 */

#include "../extern.h"
#include "towns100.h"

struct Rcd {
  	int 		index;
  	double 		value;
};
 
int 
CmpRcd(Rcd1, Rcd2)
struct Rcd 	*Rcd1;
struct Rcd 	*Rcd2;
 
{
   	if(Rcd1->value < Rcd2->value) 
		return(-1);
   	if(Rcd1->value > Rcd2->value) 
		return(1);  
   	return(0); 

} /* end CmpRcd */


double
f_10(x, p, n)
register double 	x[];
register int		p[];
register int		n;

{
 
    	register int 	i,
			Idx1,
			Idx2;

	int 		abs(), 
			CmpRcd();
    
    	struct 	Rcd 	Prs[100];

    	double 		Sum = 0.0,
	   		dx,
	   		dy,
	   		sqrt();

    	for (i = 0; i < n; i++) {
		Prs[i].index   = i;
    		Prs[i].value   = x[i];
    	}

    	qsort((char *) Prs, n , sizeof(Prs[0]), CmpRcd);

    	for(Sum = 0.0, i = 0; i < n; i++) {	/* tour length calculation */

		Idx1 = Prs[i].index;
		Idx2 = Prs[(i + 1 == n) ? 0 : (i + 1)].index;

		dx = abs(xcoord[Idx1] - xcoord[Idx2]);
		dy = abs(ycoord[Idx1] - ycoord[Idx2]);

   		Sum += sqrt(dx * dx + dy * dy);

		p[i] = Idx1;
    	}

    	return (Sum);
}

/*** end of file ***/
