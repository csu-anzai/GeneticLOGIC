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
 *      file:   measure.c
 *
 *    author:   John J. Grefenstette, Thomas Baeck
 *
 *   created:   1981/1991
 *
 *   purpose:   calculate performance measures and append them
 *        	to the output file.
 *
 *  modified:   26 mar 86
 *
 *        2 dec 86: call Converge() right before output,
 *        and fake remainder of output if Bias > 0.99
 *
 *
 *		Thomas Baeck, 16 july 90  
 *			Bias > 0.99 may be activated only, if Notermflag is 
 *			not set.
 *
 *		Thomas Baeck, 19 jul 90   
 *			Respect fopen() errors.
 *
 *		Thomas Baeck, 24 jul 90   
 *			Write adaptive mutation data into outfile.
 *
 *		Thomas Baeck, 18 oct 90
 *			Calculate the proportion of the best individual in
 *			the population, if 'Proportionflag' is set.
 *
 *		Thomas Baeck, 03 dec 90
 *			Buffer output into a general data buffer, which 
 *			is written to the outfile regularly.
 *
 */
 
#include "extern.h"

#define fopen(a,b)	fOpen((a),(b))
#define EPSILON		1.0e-4

extern 	FILE 	       *fOpen();
 
void 
Measure()

{
	extern	void	Error(),
			IntLog(),
			Converge(),
			Schema(),
			WrtPfmBuf();

	static	int	Flg = 1;

	static double  *MAvg, 
		       *VAvg,
		       *MVar, 
		       *VVar,
		       *MSkw, 
		       *VSkw,
		       *Hlp;

    	void 		EntBuf(),
			FshBuf();

	int		Siz,
			GtpCmp();

    	double 		AvgVal(),
			VarVal(),
			SkwVal(),
			New_worst();

	char		Msg[NSZ],
		       *calloc();
 
    	FILE 	       *fp;

    	register int 	i,
			j,
    			w,
    			Cnt;	/* counter for identical individuals 	*/

    	Trace("Measure entered");

	if (Flg) {				/* collect mutation data */
		if ((NbrMttRts > 0) && MttFlg) {
			if ((MAvg = (double *) calloc ((unsigned) NbrMttRts, 
						sizeof(double))) == NULL
			||  (MVar = (double *) calloc ((unsigned) NbrMttRts,
						sizeof(double))) == NULL
			||  (MSkw = (double *) calloc ((unsigned) NbrMttRts,
						sizeof(double))) == NULL) 
				Error("Measure: No space");
		}
		if ((FctDim > 0) && VarFlg) {
			if ((VAvg = (double *) calloc((unsigned) FctDim,
						sizeof(double))) == NULL
			||  (VVar = (double *) calloc((unsigned) FctDim,
						sizeof(double))) == NULL
			||  (VSkw = (double *) calloc((unsigned) FctDim,
						sizeof(double))) == NULL) 
				Error("Measure: No space");
		}
		if ((FctDim > 0) || (NbrMttRts > 0) || PrbFlg) {
			if ((Hlp = (double *) calloc ((unsigned) Popsize,
						sizeof(double))) == NULL) 
				Error("Measure: No space");
		}
		Flg = 0;
	}
					
	Best_guy 	   = 0;
	Best_current_perf  = New[Best_guy].Perf;
	Worst_current_perf = New[Popsize - 1].Perf;

	for (Ave_current_perf = 0.0, 	/* calc. performance averages */
	     AvgBstPfr 	      = 0.0, 
	     i = 0; i < Popsize; i++) {		
		Ave_current_perf += New[i].Perf;
		if ((i >= Rho - 1) && (i <= Mu - 1)) {
			AvgBstPfr += New[i].Perf;
		}
	}
    	Ave_current_perf /= Popsize;
	AvgBstPfr        /= ((double) (Mu - Rho + 1));

    	if (Proportionflag) { 		/* calculate proportion of best	*/
		for (Cnt = 0, i = 0; i < Popsize; i++) {
			if (GtpCmp(New[Best_guy].Gene, New[i].Gene)) {
				Cnt++;
			}
		}
		Best_prop = ((double) Cnt) / ((double) Popsize);
    	}
    	if (Windowsize) { /* Worst = worst in last (Windowsize) generations */
        	w 	  = Gen % Windowsize;
        	Window[w] = New_worst();
        	Worst 	  = Window[0];
        	for (i = 1; i < Windowsize; i++) {
            		if (Window[i] > Worst) Worst = Window[i];
		}
    	}
    	else {
        	if ( Worst < Worst_current_perf ) {
            		Worst = New_worst();
		}
	}
    	Online  = Onsum  / Trials; 		/* update overall performance */
    	Offline = Offsum / Trials;

    	if (Traceflag) {			/* trace activity */
       		IntLog(stdout, "Generation", Gen);
		IntLog(stdout, "Trials", Trials);
    	}
 
    	if ( Interval && ((Trials >= Plateau) || Doneflag)) {

		/* 
		 *	Add measured data to the output file. 
 		 *	The data and filename are stored in the general data
		 *	buffer PfmBuf, which is maintained here.
		 * 	Also mutation rate measures are collected and stored
		 *	here.
		 */

		Converge();

		if (NbrMttRts > 0) {	/* calc. mutation measures */
			for (	AvgRat = 0.0,
				MinRat = 1.0,
				MaxRat = 0.0,
				i = 0; i < NbrMttRts; i++) { 
				for (j = 0; j < Popsize; j++) {
					Hlp[j] = New[j].MttRts[i];
					if (Hlp[j] > MaxRat)
						MaxRat = Hlp[j];
					if (Hlp[j] < MinRat)
						MinRat = Hlp[j];
					AvgRat += Hlp[j] / (NbrMttRts*Popsize);
				}
				if (MttFlg) {
					MAvg[i] = AvgVal(Hlp, Popsize);
					MVar[i] = VarVal(Hlp, Popsize);
					MSkw[i] = SkwVal(Hlp, Popsize, MAvg[i]);
				}
			}
			if (MttFlg) {
				EntBuf(MAvg, MttBuf[0], NbrMttRts, _SGLLIN);
				EntBuf(MVar, MttBuf[1], NbrMttRts, _SGLLIN);
				EntBuf(MSkw, MttBuf[2], NbrMttRts, _SGLLIN);
		
				if (Doneflag) {		/* flush buffers */
					for (i = 0; i < BUFCNT; i++) {
						FshBuf(MttBuf[i], NbrMttRts,
								  _SGLLIN);
					}
				}
			}

		} /* end if */

		if ((FctDim > 0) && VarFlg) {	/* calc. objective measures */
			for (i = 0; i < FctDim; i++) {
				for (j = 0; j < Popsize; j++) {
					Hlp[j] = New[j].ObjVar[i];
				}
				VAvg[i] = AvgVal(Hlp, Popsize);
				VVar[i] = VarVal(Hlp, Popsize);
				VSkw[i] = SkwVal(Hlp, Popsize, VAvg[i]);
			}
			EntBuf(VAvg, ValBuf[0], FctDim, _SGLLIN);
			EntBuf(VVar, ValBuf[1], FctDim, _SGLLIN);
			EntBuf(VSkw, ValBuf[2], FctDim, _SGLLIN);

			if (Doneflag) {		/* flush buffers */
				for (i = 0; i < BUFCNT; i++) {
					FshBuf(ValBuf[i], FctDim, _SGLLIN);
				}
			}

		} /* end if */

		if (PrbFlg && (Gen > 0)) { /* store selection probabilities */
			for (j = 0; j < Popsize; j++) {	
				Hlp[j] = New[j].SltPrb;
			}
			EntBuf(Hlp, PrbBuf, Popsize, _MULLIN);

			if (Doneflag) {		/* flush buffer */
				FshBuf(PrbBuf, Popsize, _MULLIN);
			}
		}

		WrtPfmBuf(&Siz, _SGLLIN);	/* store performance data */
        	if ((Bias > 0.99) && (!Notermflag)) {
			for (	i =  Trials; 
				i <  Totaltrials; 
				i += Interval) {
                		Gen  += (Interval / Popsize) ? 
					(Interval / Popsize) : 1;
				WrtPfmBuf(&Siz, _SGLLIN);
            		}
            		Spin = Maxspin;
        	}
		if (Doneflag) {			/* flush buffer	*/
			FshBuf(PfmBuf, Siz, _SGLLIN);
		}

        	Plateau = (Trials / Interval) * Interval + Interval;

	} /* end if */

    	if (Spin >= Maxspin) {
        	if ((fp = fopen(Logfile, "a")) != NULL) {
            		IntLog(fp, "Experiment", Experiment);
            		IntLog(fp, "SPINNING at Gen", Gen);
            		IntLog(fp, "Trials", Trials);
            		fclose(fp);
		}
		else {
			sprintf(Msg, "Measure: can't open %s", Logfile);
			Error(Msg);
		}
    	}
    	if (Schemflag) {
        	Schema();
	}

    	Trace("Measure completed");

} /* end Measure */
 



double
AvgVal(x, n)
double			x[];
register int		n;

{
	extern void		Error();

	register int		i;
	register double		Avg;

	if (n == 0) {
		Error("AvgVal: Division by Zero");
	}
	for (Avg = 0.0, i = 0; i < n; i++) {
		Avg += x[i];
	}
	return(Avg / ((double) n));


} /* end AvgVal */




double 
VarVal(x, n)
double			x[];
register int		n;

{
	extern void		Error();

	register int		i;
	register double		Ex1,
				Ex2;

	if (n < 2) {
		Error("VarVal: Illegal Number of Values");
	}
	for (Ex1 = Ex2 = 0.0, i = 0; i < n; i++) {
		Ex1 += x[i];
		Ex2 += x[i] * x[i];
	}
	Ex1 = Ex1 * Ex1 / ((double) n);
	return((Ex2 - Ex1) / ((double) (n - 1)));

} /* end VarVal */




double
SkwVal(x, n, Avg)
double			x[],
			Avg;
register int		n;

{
	register int 		i;
	register double		Val,
				Mo2,	/* for calculation of 2nd moment */
				Mo3;	/* for calculation of 3rd moment */
	double			sqrt();

	for (Mo2 = Mo3 = 0.0, i = 0; i < n; i++) {
		Val  = x[i] - Avg;
		Mo2 += Val * Val;
		Mo3 += Val * Mo2;
	}
	Mo2 /= ((double) n);
	Mo3 /= ((double) n);

	return(Mo3 / sqrt(Mo2 * Mo2 * Mo2));

} /* end SkwVal */



 
double 
New_worst() 

{
    	if (Worst_current_perf == 0.0) {
		return (EPSILON);
	}
    	if (Worst_current_perf > 0.0) {
        	return (Worst_current_perf * (1.0 + EPSILON));
	}
    	return (Worst_current_perf * (1.0 - EPSILON));

} /* end New_worst */
 


int 	
GtpCmp(g1, g2)
char 	g1[],
	g2[];


{
	register int 	j    = 0,
			Stop = 0;

	while ((!Stop) && (j < Length)) {
		Stop  = (g1[j] != g2[j]);
		j++;
	}

	return (!Stop);

} /* end GtpCmp */


/*** end of file ***/
