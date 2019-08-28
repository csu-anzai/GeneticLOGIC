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
 *	$Id$
 *	$Log$
 *
 *
 *      file:   select.c
 *
 *    author:   Thomas Baeck
 *
 *   created:   22 feb 91
 *
 *   purpose:   Extract the actions common to all selection schemes.
 *
 */
 
#include "extern.h"

extern FUNCTION		f_tab[]; 




void
GenSlt()			/* generic selection function */

{ 
	extern void 	Select(),
			Select_by_Ranking(),
			RnkWht(),
			Boltzmann(),
			Select_mu_best();

	switch (SltScm) {		/* selection schemes */

		case PRP_SLT:		/* proportional */
			Select();
			break;

		case BZM_SLT:		/* Boltzmann selection */
			Boltzmann();
			break;

		case BRK_SLT:		/* Baker's ranking */
		case IRK_SLT: 		/* inverse Baker's ranking */
			Select_by_Ranking();
			break;

		case WRK_SLT:		/* Whitley's ranking */
			RnkWht();
			break;

		case MLR_SLT:		/* (m,l) */
		case MLC_SLT:		/* (m,l) with copying */
			Select_mu_best();
			break;

		default:
			Select();
			break;
		
	} /* end switch */

} /* end GenSlt */




int
CmmSltAct(NewPop, OldPop, IdxSpl, IndLgt, PopSiz, Fct, GapSiz)
STRUCTURE	NewPop[],	/* new population */
		OldPop[];	/* old population */
int		IdxSpl[];	/* index sample array */
register int	IndLgt,		/* individual length */
		PopSiz,		/* population size */
		Fct;		/* function number */
double		GapSiz;		/* generation gap size */

{

	register int	i,
			j,
			k;

	void		Gap();

	Trace("CmmSltAct entered");

    	if (GapSiz < 1.0)      /* generation gap */
        	Gap(IdxSpl);

    	for (i = 0; i < PopSiz; i++) {	/* shuffle population */
        	j         = Randint(i, PopSiz - 1);
        	k	  = IdxSpl[j];
        	IdxSpl[j] = IdxSpl[i];
        	IdxSpl[i] = k;
    	}

	for (i = 0; i < PopSiz; i++) {	/* move population information */

		k = IdxSpl[i];
		for (j = 0; j < IndLgt; j++) {
			NewPop[i].Gene[j] = OldPop[k].Gene[j];
		}	

		if (NbrMttRts > 0) {
			for (j = 0; j < MttLen; j++) {
				NewPop[i].MttGen[j] = OldPop[k].MttGen[j];
			}
			for (j = 0; j < NbrMttRts; j++) {
				NewPop[i].MttRts[j] = OldPop[k].MttRts[j];
			}
		}

		if (FctDim > 0) {
			for (j = 0; j < FctDim; j++) {
				NewPop[i].ObjVar[j] = OldPop[k].ObjVar[j];
			}
		}

		if (f_tab[Fct].MrkFct == PERM) {
			for (j = 0; j < FctDim; j++) {
				NewPop[i].PerMut[j] = OldPop[k].PerMut[j];
			}
		}

		NewPop[i].Perf 		   = OldPop[k].Perf;
		NewPop[i].SltPrb 	   = OldPop[k].SltPrb;
		NewPop[i].Needs_evaluation = 0;
	
	} /* end for */

	Trace("CmmSltAct completed");

	return(0);

} /* end CmmSltAct() */

 
/*** end of file ***/
