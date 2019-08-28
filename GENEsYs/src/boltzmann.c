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
 *	file:	boltzmann.c
 *
 *    author: 	Thomas Baeck	
 *		(Algorithm by David E. Golberg:
 *		A note on Boltzmann tournament selection, Complex Systems,
 *		No.4 (1990), pp. 445-460
 *
 *  modified:	03 mar 91
 *			FctThsHld incorporated to calculate new threshold for
 *			the nest comparison with respect to the actual results.
 *		
 *	EXTERNAL VALUES:
 *		InzTmp	: initial temperature value
 *		ChnLgt  : markov chain length
 *		CtlPar  : control parameter alpha (0.8 <= CtlPar <= 0.99)
 *
 */

#include "extern.h"

#define CHECK		0.1	/* check part of population */
#define LIMIT		11.513	/* limit for logistic function */
#define THSLD		1.0E10	/* threshold default */

void
Boltzmann()

{
	static 	int 	Flg = 1;
	static  int    *SplArr;		/* selected structures */
	static  int	MkvChn;		/* markov chain length */
	static  double	Tmp,		/* temperature for annealing */
			ThsHld;		/* threshold between fitness values */

	register int	i,
			FstIdx,		/* individual indices */
			SndIdx,
			ThdIdx;

	double		FstPrb,		/* selection probabilities, logistic */
			SndPrb,
			LogFct(),
			FctThsHld();

	int		ChsAlt();
	
	void		CmmSltAct();
	
	Trace("Boltzmann entered");

	if (Flg) {
		if ((SplArr = (int *) calloc((unsigned) Popsize, sizeof(int)))
								== NULL) {
			Error("Boltzmann: No space");
		}
		Tmp    = 10.0;		   /* temperature initialization */
		MkvChn = ChnLgt * Popsize; /* markov chain initialization */
		ThsHld = 0.5;
		Flg    = 0;
	}

	if (Trials % MkvChn == 0) { /* temperature update */
		Tmp   *= CtlPar;
	}

	for (i = Rho - 1; i < Mu; i++) {	/* perform selection */

		FstIdx = i;		/* choose first */
		SndIdx = ChsAlt(Rho, Mu, Old, 		    /* choose scnd */ 
				Old[FstIdx].Perf,  Old[FstIdx].Perf, ThsHld);
		if (Randint(0,1)) {			    /* choose third */
			ThdIdx = ChsAlt(Rho, Mu, Old, Old[FstIdx].Perf,
					Old[SndIdx].Perf, ThsHld);
		}
		else {
			ThdIdx = ChsAlt(Rho, Mu, Old, Old[FstIdx].Perf,
					Old[FstIdx].Perf, ThsHld);
		}
		FstPrb = LogFct((Old[ThdIdx].Perf - Old[SndIdx].Perf) / Tmp);
		if (Rand() < FstPrb) {
			SndIdx = ThdIdx;
		}
		SndPrb = LogFct((Old[SndIdx].Perf - Old[FstIdx].Perf) / Tmp);
		if (Rand() < SndPrb) {
			SplArr[i] = FstIdx;
		}
		else {
			SplArr[i] = SndIdx;
		}
		ThsHld = FctThsHld(SndPrb - FstPrb, Tmp); /* threshold update */
		
	} /* end for */

	CmmSltAct(New, Old, SplArr, Length, Popsize, F_nbr, Gapsize);

	Trace("Boltzmann completed");

} /* end Boltzmann() */




int
ChsAlt(rho, mu, OldPop, Pf1, Pf2, ThsHld)	/* choose alternative ind. */
register int	rho,		/* selected range of individuals */
		mu;		
STRUCTURE	OldPop[];	/* population */
double		Pf1,		/* performance values */
		Pf2,
		ThsHld;		/* threshold value */

{

	register  int	i = rho - 1,		 /* lower bound */
			Alt,			 /* alternative individual */
			ChkSiz = CHECK * (mu - 1);   /* check part of pop. */

	double		fabs();

	do {

		Alt = Randint(rho - 1, mu - 1);
		i++;

	} while (  ((fabs(Pf1 - OldPop[Alt].Perf) <= ThsHld)
		 || (fabs(Pf2 - OldPop[Alt].Perf) <= ThsHld))
		 && (i <= ChkSiz));

	return(Alt);

} /* end ChsAlt */




double
LogFct(x)
double		x;

{
	double		exp();

	if (x > LIMIT)
		return(1.0);

	if (x < -LIMIT)
		return(0.0);

	return(1.0 / (1.0 + exp(-x)));

} /* end LogFct */




double
FctThsHld(Gap, Tmp)
double		Gap,
		Tmp;

{
	double		Prb,
			fabs(),
			log();

	Prb = 0.5 + fabs(Gap) * 0.5;

	if (Prb >= 1.0)
		return(THSLD);

	return(-Tmp * log(1.0 / Prb - 1.0));

} /* end FctThsHld */



/*** end of file ***/
