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
 *	file:	f_17.c
 *
 *    author: 	Thomas Baeck
 *
 *   created:	01. July '91
 *
 *   purpose:	Problem according to Fletcher and Powell.
 *
 *  modified:	
 *
 **TeX	
 *	\Fct{Fletcher and Powell}{}
 *	\Expr{f_{17}({\vec x}) = \sum_{i=1}^n (A_i - B_i)^2}
 *	\Expr{A_i = \sum_{j=1}^n (a_{ij}\sin \alpha_j + b_{ij}\cos \alpha_j)}
 *	\Expr{B_i = \sum_{j=1}^n (a_{ij}\sin x_j + b_{ij}\cos x_j)}
 *	\begin{Cst}
 *		a_{ij}, b_{ij} \in [-100,100]				\\
 *		\alpha_j \in [-\pi, \pi]				
 *	\end{Cst}
 *		
 **TeX
 *
 */

#include <stdio.h>
#include <math.h>
#include "../extern.h"

#define RAND(A) 	(((double) rand() / 2147483647.0) * (A))
#define ZWEI_PI 	6.28318530717958647688

extern FUNCTION		  f_tab[];

static short 		  init = 1;
static short 		**a , 
			**b;
static float 		 *A;


double f_17(x, n)
int 			n;
double 			x[];
{	
	void		Init();			
	register 	i, 
			j;
	double 		sum, 
			hlp;

	if (init) Init(n);

	for (i = 0, sum = 0.0; i < n; i++) {
		for (j = 0, hlp = 0.0; j < n; j++)
			hlp += a[i][j] * sin((double) x[j])
			    +  b[i][j] * cos((double) x[j]);
		hlp -= A[i];
		sum += hlp * hlp;
	}

	return(sum);

}  /* end f_17 */



static void Init(n)
int 			n;
{
	register 	 i, 
			 j;

	double 		*alpha;

	init = 0;

	/* dynamische Arrays allokieren */

	a = (short**) malloc(n * sizeof(short*));
	b = (short**) malloc(n * sizeof(short*));

	if (a == NULL || b == NULL) {
		fprintf(stderr, "no sufficient memory\n");
		exit(1);
	}

	*a = (short*) malloc(n * n * sizeof(short));
	*b = (short*) malloc(n * n * sizeof(short));

	A     = (float*) malloc(n * sizeof(float));
	alpha = (double*) malloc(n * sizeof(double));

	if (*a == NULL || *b == NULL || A == NULL || alpha == NULL) {
                fprintf(stderr, "no sufficient memory\n");
                exit(1);
	}

	for (i = 1; i < n; i++) {
		a[i] = a[i-1] + n;
		b[i] = b[i-1] + n;
	}

	srand((unsigned) f_tab[F_nbr].CstVal[0]);	/* get seed */

	/* Arrays belegen */

	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++) {
			a[i][j] = ((int) RAND(201.0)) - 100;
			b[i][j] = ((int) RAND(201.0)) - 100;
		}
		alpha[i] = RAND(ZWEI_PI) - M_PI;
	}

	for (i = 0; i < n; i++) {
		A[i] = 0.0;
		for (j = 0; j < n; j++) 
			A[i] +=   a[i][j] * sin(alpha[j]) 
				+ b[i][j] * cos(alpha[j]);
	}

	free(alpha);
}



/*** end of file ***/
