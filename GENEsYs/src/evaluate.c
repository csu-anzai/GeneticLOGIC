/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
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
 *      file:   evaluate.c
 *
 *    author:   John J. Grefenstette
 *
 *   created:   1981
 *
 *   purpose:   evaluate the current population by
 * 	        calling the user-defined function, which is given by a
 *		index 'F_nbr' into the function table 'f_tab[]'.
 *
 *  modified:   13 feb 86
 *
 *		Thomas Baeck, 28 nov 90
 *			Removed the packing of bits to characters in order
 *			to simplify the implementation of genetic operators
 *			as well as to avoid the unnecessary amount of time
 *			for Packing and Unpacking. Our machine has enough 
 *			memory to work with chararacter-bitstrings.
 *
 *		Thomas Baeck, 28 oct 91
 *			fixed a bug in handling of permutations.
 */
 
#include "extern.h"

extern 	FUNCTION	f_tab[];
 
void 
Evaluate()

{
	extern 	void 	Savebest();

	static int	Flg = 1;

    	register double Val;

    	register int 	i;

	char 	 	Msg[NSZ];
	int		ApyObjFct();
 
    	Trace("Evaluate entered");

	if (Flg) {	/* length of individuals sufficient ?	*/
 
    		if (Length < INDEX(FctDim)) {
			sprintf(Msg, "length error in %s.c", f_tab[F_nbr].fnm);
        		Error(Msg);
    		}
		Flg = 0;
	}

    	for (i = 0; i < Popsize; i++) {
        	if ( New[i].Needs_evaluation ) {
			if (ApyObjFct(F_nbr, &New[i]) != 0) {
				Error("Evaluate: Error in function evaluation");
			}

			New[i].Needs_evaluation = 0;
            		Spin 			= 0;   	/* making progress */
			Val 			= New[i].Perf;

            		if (Savesize) {
				Savebest(i);
			}
            		Trials++;
            		if ((Val < Best) || (Trials == 1)) {
                		Best = Val;
			}
            		Onsum  += Val;
            		Offsum += Best;
        	}
    	}
 
    	Trace("Evaluate completed");

} /* end Evaluate */




int 
ApyObjFct(Idx, Ind)
register int		 Idx;
register STRUCTURE	*Ind;

{
	static int	 Flg = 1;
	static double	 MaxNbr;

	register int	 i,
			 Dim;
	
	char		 Buf[LGTH];

	double		 Min,
			 Max,
			 Val,
			 pow(),
		         (*Fct)();

	if (Flg) {
		MaxNbr = pow(2.0, (double) ChrLen) - 1.0;
		Flg    = 0;
	}
	Fct = f_tab[Idx].f;
	switch (f_tab[Idx].MrkFct) {

		case REAL:
			for (Dim = f_tab[Idx].dim, 
		     	     Min = f_tab[Idx].umin,
		     	     Max = f_tab[Idx].umax,
		     	     i   = 0; i < Dim; i++) {

				Degray(&(Ind->Gene[INDEX(i)]), Buf, ChrLen);
				Val = (unsigned) Ctoi (Buf, ChrLen);
				Val = Min + ((Max - Min) / MaxNbr) * Val;
				Ind->ObjVar[i] = Val;
			}
			Ind->Perf = Fct(Ind->ObjVar, Dim);
			break;

		case BINY:
			Ind->Perf = Fct(Ind->Gene, Length);
			break;

		case PERM:
			for (Dim = f_tab[Idx].dim, 
		     	     Min = f_tab[Idx].umin,
		     	     Max = f_tab[Idx].umax,
		     	     i   = 0; i < Dim; i++) {

				Degray(&(Ind->Gene[INDEX(i)]), Buf, ChrLen);
				Val = (unsigned) Ctoi (Buf, ChrLen);
				Val = Min + ((Max - Min) / MaxNbr) * Val;
				Ind->ObjVar[i] = Val;
			}
			Ind->Perf = Fct(Ind->ObjVar, Ind->PerMut, Dim);
			break;

		default:
			break;

	}

	return(0);

} /* end ApyObjFct */

 
/** end of file **/
