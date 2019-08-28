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

/*
 *  file:	measure.c
 *
 *  author:	John J. Grefenstette
 *
 *  created:	1981
 *
 *  purpose:	calculate performance measures and append them
 *		to the output file.
 *
 *  modified:	26 mar 86
 *
 *		2 dec 86: call Converge() right before output,
 *		and fake remainder of output if terminating
 */

#define   EXTERN
#include "global.h"

extern void Converge();
extern void Schema();

Measure()
{
	double New_worst();
	extern double sqrt();
	FILE *fp, *f2, *fopen();
	register int i;
	register int w;
	register double performance;
	register double sigma;

	Trace("Measure entered");
	for (i = 0; i < Popsize; i++)
	{
		/* update current statistics */
		performance = New[i].Perf;
		if (i > 0)
		{
			Ave_current_perf += performance;
			sigma += performance*performance;
			if (performance < Best_current_perf)
			{
				Best_current_perf = performance;
				Best_guy = i;
			}
			if (performance > Worst_current_perf)
				Worst_current_perf = performance;
		}
		else
		{
			Ave_current_perf = performance;
			sigma = performance*performance;
			Best_current_perf = performance;
			Best_guy = 0;
			Worst_current_perf = performance;
		}

	}
	Ave_current_perf /= Popsize;

	/* update Worst */
	if (Windowsize < 0) 	/* sigma scaling */
	{
		sigma -= Popsize*Ave_current_perf*Ave_current_perf;
		sigma /= Popsize - 1;

		/* hack to fix numerical problems */
		if (sigma < 0.0) sigma = 0.0;

		sigma = sqrt(sigma);
		Worst = New_worst(Ave_current_perf + Sigfact*sigma);
	}
	else if (Windowsize > 0) 	/* window scaling */
	{
		/* Worst = worst in last (Windowsize) generations */
		w = Gen % Windowsize;
		Window[w] = New_worst(Worst_current_perf);
		Worst = Window[0];
		for (i = 1; i < Windowsize; i++)
			if (Window[i] > Worst) Worst = Window[i];
	}
	else if (Worst < Worst_current_perf)
		Worst = New_worst(Worst_current_perf);

	/* update overall performance measures */
	Online = Onsum / Trials;
	Offline = Offsum / Trials;

	if (Traceflag)
	{
		printf("     Gen %d     Trials %ld\n",Gen,Trials);
		if (Onlnflag) printf("     Online %e\n", Online);
		if (Offlnflag) printf("     Offline %e\n", Offline);
	}

	if (Interval && Collectflag)
	{
		if (Trials >= Plateau)
		{
			Doneflag = (Plateau >= Totaltrials);
			/* add measures to the output file */
			Converge();
			fp = fopen(Outfile, "a");
			fprintf(fp, OUT_F2, OUT_V2);

			if (Bias >= Maxbias || Conv >= Maxconv || Spin >= Maxspin)
			{
				long temp = Plateau;

				if (Logflag)
				{
					f2 = fopen(Logfile, "a");
					fprintf(f2, "Experiment %1d: ", Experiment);
					if (Bias >= Maxbias)
						fprintf(f2, "BIASED (%0.2lf) ", Maxbias);
					if (Spin >= Maxspin)
						fprintf(f2, "SPINNING (%d) ", Maxspin);
					if (Conv >= Maxconv)
						fprintf(f2, "CONVERGED (%d) ", Maxconv);
					fprintf(f2, "at Gen %1d, ", Gen);
					fprintf(f2, "after %1ld Trials\n", Trials);
					fclose(f2);
				}
				while (temp < Totaltrials)
				{
					temp += Interval;
					Gen += (Interval/Popsize) ? (Interval/Popsize) : 1;
					fprintf(fp, OUT_F2, OUT_V2);
				}
				Trials = temp;
				Doneflag = 1;
			}

			fclose(fp);
			Plateau = (Trials/Interval)*Interval + Interval;
		}
	}
	else Doneflag = (Trials >= Totaltrials);

	if (Schemflag) Schema();

	Trace("Measure completed");
}

#define epsilon 1.0e-4

double New_worst(bad)
double bad; 	/* correct for numerical instabilities */
{
	if (bad == 0.0) return(epsilon);

	if (bad > 0.0) return(bad*(1.0 + epsilon));

	return(bad*(1.0 - epsilon));
}

/** end of file **/

