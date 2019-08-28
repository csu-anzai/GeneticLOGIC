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
 *
 *	$Id: cross.c,v 1.2 1992/06/19 16:48:50 baeck Exp $
 *	$Log: cross.c,v $
 * Revision 1.2  1992/06/19  16:48:50  baeck
 * Adaptive recombintation removed
 *
 * Revision 1.1  1992/06/19  16:41:13  baeck
 * Initial revision
 *
 *
 *
 *      file:  	cross.c
 *
 *    author:  	Thomas Baeck
 *
 *   created:   27 april 1991
 *
 *   purpose:   Implementation of the general actions for crossover and of 
 *		all crossover operators which are available in the GENESIS
 *		implementation.
 *
 *  modified:	November 26th, 1991
 *			Fixed a bug concerning the allocated array Pts in
 *			DctXvr.
 *
 *		February 3rd, 1992
 *			Fixed a bug in intermediate recombination. 
 *			Decoded values are stored now and the random decision 
 *			who becomes the offspring is made only once.
 *
 */

#include "extern.h"

extern	FUNCTION	f_tab[];


void
GenXvr()

{
    	static   int 	Flg = 1,
    			Lst;   		/* last element to undergo Crossover */

    	register int 	Mom, 		/* participants in the crossover */
			Dad;

	void		PntXvr(),
			UfmXvr(),
			DctXvr(),
			ImdXvr();

    	Trace("GenXvr entered");

    	if (Flg) {
        	Lst = (C_rate * Popsize) - 0.5 ;
		Flg = 0;
    	}

    	for (Mom = 0; Mom < Lst ; Mom += 2) {	/* do crossover on parents */

        	Dad = Mom + 1;

		switch (RecScm[0]) {	/* recombine object variables */

			case STD_REC:	/* standard n-point */
				PntXvr(	&(New[Mom].Gene[0]), 
					&(New[Dad].Gene[0]), 
					Length, C_points);
				break;

			case UFM_REC:	/* uniform crossover */
				UfmXvr( &(New[Mom].Gene[0]), 
					&(New[Dad].Gene[0]), 
					Length);
				break;

			case DCT_REC:	/* discrete recombination */
				DctXvr( &(New[Mom].Gene[0]), 
					&(New[Dad].Gene[0]), 
					ChrLen, FctDim);
				break;

			case IMD_REC:	/* intermediate recombination */
				ImdXvr( &(New[Mom]), &(New[Dad]), 0.5, 1);
				break;

			case RID_REC:	/* random intermediate recombination */
				ImdXvr( &(New[Mom]), &(New[Dad]), Rand(), 1);
				break;

			case NOP_REC:	/* do nothing */
				break;

			default:
				PntXvr(	&(New[Mom].Gene[0]), 
					&(New[Dad].Gene[0]), 
					Length, C_points);
				break;
		
		} /* end switch */

		switch (RecScm[1]) {	/* recombine mutation rates */

			case STD_REC:	/* standard n-point */
				PntXvr(	&(New[Mom].MttGen[0]), 
					&(New[Dad].MttGen[0]), 
					MttLen, 
					C_points < MttLen ? C_points : MttLen);
				break;

			case UFM_REC:	/* uniform crossover */
				UfmXvr( &(New[Mom].MttGen[0]), 
					&(New[Dad].MttGen[0]), 
					MttLen);
				break;

			case DCT_REC:	/* discrete recombination */
				DctXvr( &(New[Mom].MttGen[0]), 
					&(New[Dad].MttGen[0]), 
					M_bits, NbrMttRts);
				break;

			case IMD_REC:	/* intermediate recombination */
				ImdXvr( &(New[Mom]), &(New[Dad]), 0.5, 0);
				break;

			case RID_REC:	/* random intermediate recombination */
				ImdXvr( &(New[Mom]), &(New[Dad]), Rand(), 0);
				break;

			case NOP_REC:	/* do nothing */
				break;

			default:
				PntXvr(	&(New[Mom].MttGen[0]), 
					&(New[Dad].MttGen[0]), 
					MttLen, 
					C_points < MttLen ? C_points : MttLen);
				break;
		
		} /* end switch */

        	New[Mom].Needs_evaluation = 1;	/* set evaluation flags	*/
        	New[Dad].Needs_evaluation = 1;

    	} /* end for */

    	Trace("GenXvr completed");

} /* end GenXvr() */



void
PntXvr(Kid1, Kid2, Len, NbrPts)
register char		*Kid1;		/* pointer to crossover partners */
register char		*Kid2;
register int		 Len,		/* length of crossover partners */
			 NbrPts;	/* number of crossover points */

{
	/*
	 * 	perform an n-point crossover or adaptive crossover
	 */

    	static   int 	Flg = 1,
    		       *Pts;		/* array of crossover points */
    
    	register int 	Act,		/* actual crossover-position */
    			Nxt,		/* next crossover-position */
			i;

    	register char 	Tmp;    	/* used for swapping alleles */

    	int	 	icmp();		/* function to compare integers	*/

    	double   	Xvr1,		/* the crossover point numbers */
	     		Xvr2,
	     		pow();

    	char 	       *calloc();

    	Trace("PntXvr entered");

    	if (Flg) {
		if ((Pts = (int *) calloc((unsigned) NbrPts, sizeof(int))) 
								== NULL) {
	     		Error("PntXvr: No space");
		}
        	Flg = 0;
    	}

	for (i = 0; i < NbrPts; i++) {		/* choose crossover points */
		Pts[i] = Randint(0, Len - 1);
	}

	if (NbrPts > 1) {			/* sort crossover points */
		qsort((char *) Pts, NbrPts, sizeof(int), icmp);
	}

     	/* now perform crossover as follows: 				*/
	/* Assume i is an even index of a crossover-point. Then the 	*/
	/* bits from Pts[i] to Pts[i+1]-1 are exchanged between the	*/
	/* parent individuals.						*/

	for (i = 0; i < NbrPts; i += 2) { /* get actual and next point	*/

		Act = Pts[i];
		Nxt = (i+1 < NbrPts) ? Pts[i+1] : Len;

		for (; Act < Nxt; Act++) {	/* exchange information */
			Tmp 	  = Kid1[Act];
			Kid1[Act] = Kid2[Act];
			Kid2[Act] = Tmp;
		}

	} /* end for */

    	Trace("PntXvr completed");

} /* end PntXvr */



void
UfmXvr(Kid1, Kid2, Len)
register char		*Kid1;		/* pointer to crossover partners */
register char		*Kid2;
register int 		 Len;		/* length of crossover partners */

{
	/*
	 *	perform uniform crossover on Kid1 and Kid2
	 */

	register int	Tmp,
			i;

	for (i = 0; i < Len; i++) {
		if (Randint(0,1)) {	/* exchange information */
			Tmp 	= Kid1[i];
			Kid1[i] = Kid2[i];
			Kid2[i] = Tmp;
		}
	}

} /* end UfmVxr */


void
DctXvr(Kid1, Kid2, Len, Nbr)
register char		*Kid1;		/* pointer to crossover partners */
register char		*Kid2;
register int 		 Len,		/* length of chromosome information */
			 Nbr;		/* number of chromosomes */

{

	/*
	 *	perform discrete crossover on Kid1 and Kid2
	 */

	register int	Tmp,
			Pos,		/* basic position in individuals */
			i,
			j;

	for (i = 0; i < Nbr; i++) {
		if (Randint(0,1)) {	/* exchange chromosome */
			Pos = i * Len;
			for (j = 0; j < Len; j++, Pos++) {
				Tmp       = Kid1[Pos];
				Kid1[Pos] = Kid2[Pos];
				Kid2[Pos] = Tmp;
			}
		}
	} /* end for */

} /* end DctXvr */



void
ImdXvr(Ind1, Ind2, Prp, Tgt)
register STRUCTURE	*Ind1;		/* pointer to crossover partners */
register STRUCTURE	*Ind2;
register double		 Prp;		/* weights of individuals */
register int		 Tgt;		/* indicator for mutation rates, */
					/* object variables, or both */

{
	/*
	 * 	perform intermediate crossover on individuals
	 */

	static int	Flg = 1;

	static double 	Mrt,		/* maximum integer rate */
			Xvl;		/* maximum integer object variable */

	char		Buf[LGTH];

	register int	i,
			k;

	register double	Val,
			IVal;

	double		pow();

	void		Gray(),
			Itoc();

	if (Flg) {
		Mrt = pow(2.0, (double) M_bits) - 1.0;
		Xvl = pow(2.0, (double) ChrLen) - 1.0;
		Flg = 0;
	}

	if (Tgt) {			/* recombine object variables */

		for (k = Randint(0,1), i = 0; i < FctDim; i++) {
			Val  = (Ind1->ObjVar[i]) * Prp + 	
			       (Ind2->ObjVar[i]) * (1.0 - Prp);
			IVal = (Val - f_tab[F_nbr].umin) * Xvl 
				/ (f_tab[F_nbr].umax - f_tab[F_nbr].umin);
			Itoc((int) IVal, Buf, ChrLen);
			if (k == 1) {
				Ind1->ObjVar[i] = Val;
				Gray(Buf, &(Ind1->Gene[i * ChrLen]), ChrLen);
			}
			else {
				Ind2->ObjVar[i] = Val;
				Gray(Buf, &(Ind2->Gene[i * ChrLen]), ChrLen);
			}
		}
	}
	else {				/* recombine mutation rates */

		for (k = Randint(0,1), i = 0; i < NbrMttRts; i++) {
			Val  = (Ind1->MttRts[i]) * Prp + 	
			       (Ind2->MttRts[i]) * (1.0 - Prp);
			IVal = Val * Mrt / MAXMRATE;
			Itoc((int) IVal, Buf, M_bits);
			if (k == 1) {
				Ind1->MttRts[i] = Val;
				Gray(Buf, &(Ind1->MttGen[i * M_bits]), M_bits);
			}
			else {
				Ind2->MttRts[i] = Val;
				Gray(Buf, &(Ind2->MttGen[i * M_bits]), M_bits);
			}
		}
	}

} /* end ImdXvr */



int
icmp(i,j)			/* compare two integer values for qsort	*/
register int 	i, 
		j;

{
	return (i - j);

} /* end icmp */

/** end of file **/
