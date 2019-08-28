
/*
 *  file:	measure.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	calculate performance measures and append them
 *		to the output file.
 *
 *  modified:	26 mar 86
 *
 *		2 dec 86: call Converge() right before output,
 *		and fake remainder of output if Bias > 0.99
 *
 *		10 sep 90: print statistics in display mode,
 *              handle max or min problems.
 */

#include "Pextern.h"
extern void PSchema();

#define DISPMEAS 4

PMeasure()
{
  double New_worst();
  FILE *fp, *fopen();
  register int i;
  register int w;
  int j;
  STRUCTURE Best_current_structure;
  
  Trace("Measure entered");
  Dtrace("measure");
  Time(0,"");

  with(physical)
    where(Index < Popsize) {
      Ave_current_perf = 0.0;
      Ave_current_perf += ParallelNew.Perf;
      
      Best_current_perf = [0]ParallelNew.Perf;
      Worst_current_perf = Best_current_perf;
      if (Maxflag)
	Best_current_perf >?= ParallelNew.Perf;
      else
	Best_current_perf <?= ParallelNew.Perf;
      where(ParallelNew.Perf == Best_current_perf)
	Best_guy = (int)Index;
      
      if (Maxflag)
	Worst_current_perf <?= ParallelNew.Perf;
      else
	Worst_current_perf >?= ParallelNew.Perf;

      Best_current_structure = [Best_guy]ParallelNew;
    }
  
  Ave_current_perf /= Popsize;
  
  /* update Worst */
  if (Windowsize)
    {
      /* Worst = worst in last (Windowsize) generations */
      w = Gen % Windowsize;
      Window[w] = New_worst();
      Worst = Window[0];
      for (i=1; i < Windowsize; i++)
	if (BETTER(Worst, Window[i])) Worst = Window[i];
    }
  else
    if (BETTER(Worst, Worst_current_perf))
      Worst = New_worst();
  
  /* update overall performance measures */
  Online = Onsum / Trials;
  Offline = Offsum / Trials;
  
  if (Traceflag)
    {
      printf("     Gen %d     Trials %d\n",Gen,Trials);
      if (Onlnflag) printf("     Online %e\n", Online);
      if (Offlnflag) printf("     Offline %e\n", Offline);
    }
  
  if (Displayflag)
    {
      static firstflag = 1;
      if (firstflag)
	{
	  firstflag = 0;
	  move(DISPMEAS - 1,0);
	  printw("Gens Trials Lost ");
	  printw("Conv  Bias       Online      ");
	  printw("Offline         Best      Average");
	}
      
      move(DISPMEAS,0);
      clrtoeol();
      PConverge();
      printw("%4d %6d %4d ",
	     Gen, Trials, Lost);
      printw("%4d %5.3f  %11.4f  ",
	     Conv, Bias, Online);
      printw("%11.4f  %11.4f  %11.4f",
	     Offline, Best, Ave_current_perf);
      
      move(DISPMEAS+2,0);
      clrtoeol();
      printw("Current Best Structure: %3d  ", Best_guy);
      printw("Performance: %0.4f ", Best_current_structure.Perf);
      move(DISPMEAS+3,0);
      clrtoeol();
      Unpack(Best_current_structure.Gene, Bitstring, Length);
      if (Floatflag)
	{
	  FloatRep(Bitstring, Vector, Genes);
	  for (j=0; j<Genes; j++)
	    printw(Gene[j].format, Vector[j]);
	}
      else
	{
	  printw("%s", Bitstring);
	}
      refresh();
    }
  
  if ( Interval && Collectflag && ((Trials >= Plateau) || Doneflag))
    {
      /* add measures to the output file */
      PConverge();
      fp = fopen(Outfile, "a");
      fprintf(fp,OUT_F2, OUT_V2);
      
      if (Bias > 0.99)
	{
	  fprintf(fp,OUT_F2,OUT_V2);
	  Spin = Maxspin;
	}
      
      fclose(fp);
      Plateau = (Trials/Interval)*Interval + Interval;
    }

  if (Logflag  && (Spin >= Maxspin))
    {
      fp = fopen(Logfile, "a");
      fprintf(fp, "Experiment %1d  ", Experiment);
      fprintf(fp, "SPINNING at Gen %1d, ",Gen);
      fprintf(fp, "after %1d Trials\n", Trials);
      fclose(fp);
    }
  
  if ( Interflag && (Spin >= Maxspin))
    {
      move(22,0);
      clrtoeol();
      printw("SPINNING at Gen %1d, ",Gen);
      printw("after %1d Trials\n", Trials);
      refresh();
    }
 
  if (Schemflag)
    PSchema(); 

  Time(1,"Measure");
  Trace("Measure completed");
}


double New_worst()
{
  double delta;
  
  /* return a value a little worse than Worst_current_perf */
  
  if (Maxflag)
    delta = 1.0e-4;
  else
    delta = -1.0e-4;
  
  if (Worst_current_perf == 0.0) return (-delta);
  
  if (Worst_current_perf > 0.0)
    return (Worst_current_perf*(1.0 - delta));
  
  return (Worst_current_perf*(1.0 + delta));
}



static char BIT[CHARSIZE] ={ '\200', '\100', '\040', '\020',
			       '\010', '\004', '\002', '\001'};


PConverge()			/* measure population convergence	*/
{
  register int j;	/* loop control				*/
  register int ones;	/* number of ones in a given position	*/
  int focus;		/* index of current byte		*/
  int bit;		/* index of current bit			*/
  FILE *fp, *fopen();
  
  Trace("Converge entered");
  Time(0,"");
  
  Bias = 0.0;
  Lost = Conv = 0;
  if (!Convflag) return;
  
  for (j = 0; j < Length; j++)
    {
      focus = j / CHARSIZE;
      bit = j % CHARSIZE;
      ones = 0;

      with(physical)
	where(Index < Popsize)
	  ones += ((ParallelNew.Gene[focus] & BIT[bit]) != 0);

      Lost += (ones == 0) || (ones == Popsize);
      Conv += (ones <= FEW) || (ones >= Popsize - FEW);
      Bias += (ones > Popsize/2) ? ones : (Popsize - ones);
    }
  
  Bias /= (Popsize*Length);
  
  if (Logflag && (Lost==Length))
    {
      fp = fopen(Logfile, "a");
      fprintf(fp, "CONVERGED at Gen %1d, ",Gen);
      fprintf(fp, "after %1d Trials\n", Trials);
      fclose(fp);
    }

  Time(1,"Converge");
  Trace("Converge completed");
}

/** end of file **/








