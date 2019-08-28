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
 *	$Id: generate.c,v 1.2 1992/06/16 12:11:11 baeck Exp baeck $
 *	$Log: generate.c,v $
 * Revision 1.2  1992/06/16  12:11:11  baeck
 * Added the mutation rate check
 *
 * Revision 1.1  1992/06/12  11:34:07  baeck
 * Initial revision
 *
 *
 *
 *  file:    	generate.c
 *
 *  author:    	John J. Grefenstette
 *
 *  created:    1981
 *
 *  purpose:    One generation consists of
 *            	(1) forming a new population of structures.
 *            	(2) evaluating the population.
 *            	(3) gathering performance statistics.
 *
 *  modified:    7 feb 86
 *
 *        	2 dec 86: call Measure() before Done() so that
 *        	we can quit upon convergence.  Measure() nows
 *        	calls Converge().
 *
 *	  	Thomas Baeck, 18 jun 90	
 *			if `C_points == 1', call `One_Pt_Crossover()' instead of
 *			`Crossover()'.
 *
 *	  	Thomas Baeck, 06 sep 90	
 *			criteria for selection variants specified exactly.
 *
 *		Thomas Baeck, 20 feb 91
 *			Selection and mutation mechnanism switches altered.
 *
 *		Thomas Baeck, 19 dec 90
 *			The population is sorted according to the fitness 
 *			values. The reason is, that static selection mechanisms
 *			need a sorted population in any case. Furthermore,
 *			statistics are simplified.
 *
 *		Thomas Baeck, 04 mar 91
 *			Boltzmann selection.
 */
 
#include "extern.h"

extern FUNCTION 	f_tab[];

 
void 
Generate()

{
	extern void	GenXvr(),
			GenMut(),
			GenSlt(),
			Converge(),
			Elitist(),
			Evaluate(),
			Initialize(),
			IntLog(),
			DmpInf();

	extern int	Done();

	STRUCTURE      *Tmp;    	/* for swapping pop. pointers   */

    	register int 	i;		/* for marking structures	*/
	int 		dcmp();

    	if (Traceflag) {
        	IntLog(stdout, "Generation", Gen);
	}

    	Trace("Generate entered");
 
	if (Gen == 0) { 	/* this is a fresh experiment */
        	Initialize();   /* form an initial population 	*/
        	Spin++;
    	}
    	else {
		GenSlt();		/* select */
		if (M_rate > 0.0)
			GenMut();	/* mutate */
		if (C_rate > 0.0) {	/* perform crossover */
			GenXvr();
		}
        	if (Eliteflag) {	/* Elitist variant ?	*/
			if (Popsize == 1) 
				Evaluate();
            		Elitist();
		}
        	if (Allflag) {    	/* mark structures 	*/
            		for (i = 0; i < Popsize; i++) {
				New[i].Needs_evaluation = 1;
			}
		}
        	Spin++;
	}
 
    	Evaluate();			/* evaluate the new population	*/
    	Doneflag = Done(); 		/* check termination criterion */

	qsort(	(char *)New, 		/* sort population*/
		Popsize, 
		sizeof(STRUCTURE), 
		dcmp); 

    	Measure();			/* gather performance statistics */
	DmpInf();			/* dump informations */

    	Tmp = Old;			/* swap pointers */
    	Old = New;
    	New = Tmp;
    	Gen++;				/* update generation counter */

    	Trace("Generate completed");

} /* end Generate */




void
DmpInf()

{
	extern void 	Checkpoint(),
			Printbest(),
			PrtPmt(),
			DmpPop(),		/* population dumps */
			BitMap();		/* bitmap dumps */

	FILE	       *fp;

	char		PmtFil[NSZ];

	if ((PgmFrq > 0) && (Gen % PgmFrq == 0)) {	/* best bitmap */
		BitMap(Best_guy);	
	}

    	if ((Dump_freq > 0) && (Gen % Dump_freq == 0)) { 	/* dump */
            	sprintf(Dumpfile, "%s.%03d", Ckptfile, Curr_dump);
            	Curr_dump++;
		if (Printpopflag) {	/* dump phenotype information */
			DmpPop(New, Gen, Popsize, Dumpfile);	
			if (f_tab[F_nbr].MrkFct == PERM) {
				sprintf(PmtFil, "%s.%s.%d%d", "Pmt", 
					Sfx, Experiment + 1, Gen);
				if ((fp = fopen(PmtFil, "a")) != NULL) {
					PrtPmt(fp);
					fclose(fp);
				}
			}
		}
    	}
    	else {
        	if (Doneflag) {
            		if (Lastflag) {
                		Checkpoint(Ckptfile);
			}
            		else {
                		if (Savesize) {
                    			Printbest();
				}
			}
        	}
    	}

} /* end DmpInf */ 



 
int 	
dcmp(x,y)
STRUCTURE *x,*y;

/* return value > 0 , if x.Perf > y.Perf				*/
/*	        < 0 , if x.Perf < y.Perf				*/

{   
    	double a,b;

    	a = x->Perf;
    	b = y->Perf;
    	if (a < b)
      		return -1;
    	if (b < a)
      		return 1;
    	return 0;

} /* end dcmp */


/*** end of file ***/
