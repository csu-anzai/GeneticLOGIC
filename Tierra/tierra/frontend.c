/* frontend.c   9-9-92  Tierra Simulator frontend routines */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/* frontend rationale:  to have a common set of functions for passing info
   from Tierra to the front end.  Any particular front end will likely
   use only a subset of all frontend functions.  Those functions that a
   particular front end does not use, will either have to be implemented
   here as dummy functions, or they will have to be ifdef'ed where they
   are called from Tierra */

#ifndef lint
static char     sccsid[] = "@(#)frontend.c	1.5 7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"
#ifdef ALCOMM
#include "tmonitor.h"
#include "trequest.h"
#endif

/* ======================================================================*/
/* include macros, functions */
/* ======================================================================*/

#if FRONTEND == STDIO
#include "tstdio.c"
#endif

#if FRONTEND == BASIC
#ifdef __TURBOC__
#include "tturbo.c"

#else

#include "tcurses.c"
#endif
#endif



#ifdef MEM_CHK
#include <memcheck.h>
#endif

/* ======================================================================*/

void  FEMessage(n,pbuf)
I32s  n;
I8s  **pbuf;
{   I8s  i;
    I16s msgy;

#ifdef MEM_PROF
    if (IMode == PLN_STATS) msgy = MSG_Y + 11; 
    else msgy =  MSG_Y ;
#else 
    if (IMode == PLN_STATS) msgy = MSG_Y + 8; 
    else msgy =  MSG_Y ;
#endif /* else of  MEM_PROF */

    for(i = 0; i < n; i++)
	{
	   FEPrintf(MSG_X,msgy + i,0,"%s\n", mes[i]);
           if(Log) fprintf(tfp_log,"%s\n", mes[i]);
	}
#if FRONTEND != STDIO
   FEClrmsg(msgy+i); 
if ((IMode == SIZ_HIST) || (IMode == SIZM_HIST) || (IMode == GEN_HIST))
   {
   sleep(2);
   query_species(fe_lines);
   }
#endif
    if(Log) fflush(tfp_log);
}

/* ======================================================================*/

#ifdef __TURBOC__
void FEError (I32s err_no, I32s err_exit, I32s err_write, ... )
#else /* else of __TURBOC__ */
#ifdef __GNUC__
void FEError (I32s err_no, I32s err_exit, I32s err_write, ... )
#else
void FEError (err_no,err_exit,err_write, va_alist)
I32s err_no,
     err_exit,
     err_write;
va_dcl
#endif /* __GNU__ */
#endif /* else of __TURBOC__ */
{
    va_list ap;

    I8s *err_str,
	buf[85],
	ch;

#ifdef __TURBOC__
    va_start(ap,err_write);
#else /* else of __TURBOC__ */
#ifdef __GNUC__
    va_start(ap,err_write);
#else
    va_start(ap);
#endif /* __GNUC__ */
#endif /* else of __TURBOC__ */

    if ((err_str = va_arg(ap, I8s *)) != NULL)
       {
	vsprintf(buf,err_str,ap);
        FEPrintf(ERR_X,ERR_Y,1,buf);
#if FRONTEND == STDIO
        FEPrintf(ERR_X,ERR_Y,0,"\n");
#endif
       if(Log && (err_no > -200) && (err_no != -110)) 
	  {
	  vfprintf(tfp_log,err_str,ap);
	  fprintf(tfp_log,"\n");
	  }
       }

    va_end(ap);
#if FRONTEND != STDIO
       if(strlen(buf) > 1 )
	  {
          FEPrintf(HLP_X,HLP_Y,1 ,
"                      Tierra ERROR Press any key to continue                ");
          ch = FEGetc(); 
#ifdef __TURBOC__
          if (ch == '~') FEMenu(); /* so that we can examin params */
#endif
	  }
#endif

    if (err_write == WRITE)
       {IMode = 69;
        sprintf(mes[0],"Writing soup ...");
	FEMessage(1,mes);
       WriteSoup(err_exit);
       IMode = PLN_STATS;
       }
    if (err_exit == EXIT)
       {IMode = 69;
        sprintf(mes[0],"Tierra going down ... "); 
	FEMessage(1,mes);
       if(Log) { fclose(tfp_log);Log=0; }
       while (hangup);
       FEPrintf(HLP_X,HLP_Y,1 ,
"                      Tierra exiting  (PRESS ANY KEY)                   \n"); 
       FEGetc(); 
       FEExit(err_no);
       }
    if(Log) fflush(tfp_log);
}

/* ------------------------------------------------------------------------ */

#ifdef MEM_PROF

void FEMemProf(SizSoup, SizCells, SizFreeMem, SizSl, SizSli,
        SizGl, SizGli, SizGen)
I32s SizSoup, SizCells, SizFreeMem, SizSl, SizSli,
        SizGl, SizGli, SizGen;
{
    if ((IMode == SIZ_HIST) || (IMode == SIZM_HIST) || (IMode == GEN_HIST)) 
        return ;
    FEPrintf(PLN_X,PLN_Y+7,0,
        "    TotMemUse = %6ld  SizFreeMem   = %6ld  SizCells  = %6ld\n",
        TotMemUse, SizFreeMem, SizCells);
    FEPrintf(PLN_X,PLN_Y+8,0,
        "    SizSoup   = %6ld  SizSl        = %6ld  SizSli    = %6ld\n",
        SizSoup, SizSl, SizSli);
    FEPrintf(PLN_X,PLN_Y+9,0,
        "    SizGl     = %6ld  SizGli       = %6ld  SizGen    = %6ld\n",
        SizGl, SizGli, SizGen);
}

#endif /* MEM_PROF */

/* ------------------------------------------------------------------------ */

void  FEPlan(MaxPop, MaxMem, MaxGenPop, MaxGenMem)
I32s  MaxPop, MaxMem;
Genotype  *MaxGenPop, *MaxGenMem;
{   long int  tp;
    tp = time(NULL);

#ifdef __TURBOC__
    if (Log) fprintf(tfp_log,"core = %ld\n",(I32u) coreleft());
#endif
#if FRONTEND != STDIO
    if ((IMode == SIZ_HIST) || (IMode == SIZM_HIST)||(IMode == GEN_HIST)) 
       query_species (fe_lines);
    else
#endif
    {
    FEPrintf(PLN_X,PLN_Y,0,
	"InstExeC      = %6ld  Generations  = %6.0f  %s",
        InstExe.m, Generations, ctime(&tp));
    if (GeneBnker)
        {
    FEPrintf(PLN_X,PLN_Y+1,0,
	"    NumCells  = %6ld  NumGenotypes = %6ld  NumSizes  = %6ld\n",
        NumCells, NumGenotypes, NumSizes);
    FEPrintf(PLN_X,PLN_Y+2,0,
	"    AvgSize   = %6ld  NumGenDG     = %6ld  NumGenRQ  = %6ld\n", 
	AverageSize,NumGenDG,NumGenRQ);
        }
    else /* (GeneBnker) */
        {
    FEPrintf(PLN_X,PLN_Y+1,0, "    NumCells  = %6ld\n", NumCells);
    FEPrintf(PLN_X,PLN_Y+2,0, "    AvgSize   = %6ld\n", AverageSize);
        }
    if (InstExe.m)
	{
        FEPrintf(PLN_X,PLN_Y+3,0,
	    "    AvgPop    = %6.0f  Births       = %6ld  Deaths    = %6ld\n",
            TimePop,TimeBirth, TimeDeath);
        if (GeneBnker)
	  {
         FEPrintf(PLN_X,PLN_Y+5,0,"    MaxGenPop = %6ld  (%4.4ld%3.3s)",
               MaxPop,MaxGenPop->size, MaxGenPop->label);
         FEPrintf(PLN_X+33,PLN_Y+5,0,"    MaxGenMem = %6ld (%4.4ld%3.3s)\n",
               MaxMem / MaxGenMem->size, MaxGenMem->size, MaxGenMem->label);
          }
        }
    FEPrintf(PLN_X,PLN_Y+((InstExe.m)?4:3),0,
	"    RateMut   = %6ld  RateMovMut   = %6ld  RateFlaw  = %6ld\n",
        RateMut, RateMovMut, RateFlaw);
   } /* end of else on IMode */
if (Log)
    {
    fprintf(tfp_log,"\nie%ld gn%.0f %s",
        InstExe.m, Generations, ctime(&tp));
    fprintf(tfp_log, "nc%ld ng%ld ns%ld\n",
        NumCells, NumGenotypes, NumSizes);
    fprintf(tfp_log,"as%ld rq%ld dg%ld\n",
        AverageSize, NumGenRQ, NumGenDG);
    if (InstExe.m)
        {
        fprintf(tfp_log,"bi%ld de%ld ap%.0f\n",
            TimeBirth, TimeDeath,TimePop);
        if (GeneBnker)
          {
           fprintf(tfp_log,"mp%ld @ %4.4ld%3.3s",
               MaxPop,MaxGenPop->size, MaxGenPop->label);
           fprintf(tfp_log," mg%ld @ %4.4ld%3.3s\n",
               MaxMem / MaxGenMem->size, MaxGenMem->size, MaxGenMem->label);
          }
        }
    fprintf(tfp_log,
        "rm%ld mm%ld rf%ld\n",
        RateMut, RateMovMut, RateFlaw);
       fprintf(tfp_log,"\n");
    fflush(tfp_log);
    }
#ifdef ALCOMM
         if ( MIsDFEnabled( TrtPlanEvent ) )
              {
              TPlan();
              }
#endif /* ALCOMM */

}



/* ======================================================================*/
/* ======================================================================*/
/* ======================================================================*/

