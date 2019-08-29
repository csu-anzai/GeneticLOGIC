/* ======================================================================*/
/* ttools.c  9-9-92  support for data analysis of the 
   Tierra Simulator V4.0: Copyright (c) 1992 Dan Pirone & Virtual Life
   written by Daniel pirone
   v 2.0
*/
/* ======================================================================*/

#ifndef lint
static char ttools_sccsid[] = "@(#)ttools.c	1.7 7/21/92";
#endif

#include "tierra.h"
#include "extern.h"
#include <signal.h>

#ifdef ALCOMM
#include "tmonitor.h"
#include "trequest.h"
#include <mlayer.h>
#endif



#ifdef MEM_CHK
#include <memcheck.h>
#endif

/*-----------------------------------------------------------------------*/
/*-----------------------------------------------------------------------*/
/* routines for query_species */
/*-----------------------------------------------------------------------*/
I16s hg_compare(i,j)         
HistType   *i,*j;
   {
   if(i->size == j->size)
     return(i->lbl - j->lbl);
   return(i->size - j->size);
   }
/*-----------------------------------------------------------------------*/
I16s hs_compare(i,j)         
HistType   *i,*j;
   {
   return(i->size - j->size);
   }
/*-----------------------------------------------------------------------*/
I16s hc_compare(i,j)         
HistType   *i,*j;
   {
   return(j->count - i->count);
   }
/*-----------------------------------------------------------------------*/
/* routines for query_size */
/*-----------------------------------------------------------------------*/
I16s bc_compare(i,j)         
struct size_buf {
   I32s count;
   I32s lbl;
   I32s moves;
   I32s flags;
   I32s bits;
   } *i,*j;
   {
   return(j->count - i->count);
   }
/*-----------------------------------------------------------------------*/
I16s bg_compare(i,j)         
struct size_buf {
   I32s count;
   I32s lbl;
   I32s moves;
   I32s flags;
   I32s bits;
   } *i,*j;
   {
     return(i->lbl - j->lbl);
   }
/*-----------------------------------------------------------------------*/

void query_size(size_class)
I32s size_class;
{
I32s nnn,num_spec,top_buf,tc,tl,ci,t,thit;
typedef struct size_buf {
   I32s count;
   I32s lbl;
   I32s moves;
   I32s flags;
   I32s bits;
   } bf;
   bf * buf;
   Pcells  ce;

if((size_class < 1) || (size_class > siz_sl)|| (!GeneBnker)
   || (sl[size_class]<(SList *)4)) return;

num_spec = sl[size_class]->num_g;

if ((buf = (bf *) thcalloc((num_spec+1) , sizeof(bf) )) == (bf *) NULL)
   {
   FEError(-1500,NOEXIT,NOWRITE,
      "Tierra query_size() Not enough memory to create size table");
    return;
   }
for(tc = 0,t= 0; tc < sl[size_class]->num_g,t < sl[size_class]->a_num; t++)
   {
   if (!((I32s) sl[size_class]->g[t] > 4)) continue;
       buf[tc].lbl = tc; 
   buf[tc].count =  sl[size_class]->g[t]->pop;
   buf[tc].moves =  sl[size_class]->g[t]->d1.mov_daught;
   buf[tc].flags =  sl[size_class]->g[t]->d1.flags;
   buf[tc].bits =  sl[size_class]->g[t]->bits;
   tc++; 
   }

tc = (tc > fe_lines-MSG_Y)? fe_lines - MSG_Y -1 : tc;

qsort(buf,num_spec-1,sizeof(bf),bc_compare); /*sort all, by count*/
qsort(buf,tc, sizeof(bf),bg_compare); /* sort top num, by size */

 FEPrintf(MSG_X,MSG_Y,0,"Gene:    #  Mem Errs   Move  Bits\n");
 if (Log && HistPrint) fprintf(tfp_log,"Gene:    #  Mem Errs   Move  Bits\n");
 
   for(t= 0; t < tc; t++)
      {
      if (buf[t].count < 1) break;
      WritEcoB(buf[t].bits ,mes[9]);
      FEPrintf(MSG_X,MSG_Y+t+1,0,"%3.3s %6ld %3ld %3ld %6ld    %s\n",
        Int2Lbl(buf[t].lbl),
        buf[t].count,
        (I32s) (100.0 * buf[t].count * size_class / SoupSize),
        buf[t].flags ,
        buf[t].moves, 
        mes[9]
        );
      if (Log && HistPrint) 
        fprintf(tfp_log,"%3.3s %6ld %3ld %3ld %6ld    %s\n",
        Int2Lbl(buf[t].lbl),
        buf[t].count,
        (I32s) (100.0 * buf[t].count * size_class / SoupSize),
        buf[t].flags ,
        buf[t].moves, 
        mes[9]
        );
      }
   if (Log && HistPrint) {fprintf(tfp_log,"\n"); fflush(tfp_log);}
   if (buf)
   {  thfree(buf);
      buf = NULL;
   }
} /* end query_size */

/*-----------------------------------------------------------------------*/

void query_species(num)
I32s num;
{

I32s num_ent = 0,
     nnn = 0,
     ci,t,n_star = 0;
     Pcells ce;


if ((!GeneBnker)|| (NumSizes < 1) || (num < 2)) return; 

if (Hist != NULL)
   {  
   thfree(Hist);
   Hist = (HistType *)NULL;
   }
Max_hits = -1;

if (IMode == GEN_HIST) 
   num_ent = NumGenotypes;
else                                 /* IMode == SIZ_HIST  || SIZM_HIST*/
   num_ent = NumSizes;

if ((Hist  = (HistType *) thcalloc((num_ent+1) , sizeof(HistType))) 
    == (HistType *) NULL)
   {
   FEError(-1501,NOEXIT,NOWRITE,
      "Tierra query_species() Not enough memory to create %s table",
          (IMode == GEN_HIST)? "genotype":
       (IMode == SIZM_HIST)? "memory" : "size");
    return;
   }

for(nnn= 0,ci = 1; (ci < siz_sl && nnn < num_ent); ci++)
   {
   if(((I32u) sl[ci] > 4L) && (sl[ci]->num_c > 0))
      {
         if (IMode == GEN_HIST)
            {
            for(n_star = 0,t= 0; 
               (n_star < sl[ci]->num_g && t < sl[ci]->a_num); t++)
               {
               if(((I32u) sl[ci]->g[t] < 5L) || (sl[ci]->g[t]->pop < 1))
                  continue;
               Hist[nnn].size = ci;
               Hist[nnn].lbl =  t; /* int form of label */
               Hist[nnn].count =  sl[ci]->g[t]->pop;
               if (Hist[nnn].count >= Max_hits) 
                  {Max_hits = Hist[nnn].count;}
               nnn++; n_star++;
               }
            }
         else /* IMode == SIZ_HIST  || SIZM_HIST */
           {
           Hist[nnn].size = ci;
           Hist[nnn].count =  sl[ci]->num_c;
           if (Hist[nnn].count >= Max_hits) 
              {Max_hits = Hist[nnn].count;}
           nnn++;
           }
    } /* end of valid size class */
}

if (( Max_hits ) < 1) return;
nnn = ((nnn > num_ent)? num_ent : nnn);
nnn = ((nnn > num)? num : nnn);
nnn = ((nnn > fe_lines - MSG_Y)? fe_lines - MSG_Y :nnn);

if (IMode == SIZM_HIST) 
   for (t = 0; t < nnn; t++)
     {
     Hist[t].count *= Hist[t].size;
     if (Hist[t].count >= Max_hits) 
        {Max_hits = Hist[t].count;}
    }

qsort(Hist,num_ent-1,sizeof(HistType),hc_compare); /*sort all, by count*/
if (IMode == GEN_HIST)
   qsort(Hist,nnn, sizeof(HistType),hg_compare); /* sort top num, by size */
else /* IMode == SIZ_HIST  || SIZM_HIST*/
   qsort(Hist,nnn, sizeof(HistType),hs_compare); /* sort top num, by size */

if ((Hist  = (HistType *) threcalloc(Hist,((nnn+1)*sizeof(HistType)),
                                          ((num_ent+1) * sizeof(HistType)))) 
    == (HistType *) NULL)
   {
   FEError(-1502,NOEXIT,NOWRITE,
      "Tierra query_species() problem reallocing %s table",
          (IMode == GEN_HIST)? "genotype":
       (IMode == SIZM_HIST)? "memory" : "size");
    return;
   }

HistSize = nnn;
HistNStars =  ((float)(fe_width -20) /(float) Max_hits);

for(t= 0; t < nnn; t++)
    {
         if (Hist[t].count < 1) continue;
         Buff[0]=  '*'; Buff[1]=  '\0';
         Hist[t].nstar = n_star =  HistNStars * Hist[t].count;
         if (IMode == GEN_HIST)
            {
            if ((t > 0) && (Hist[t].size == Hist[t-1].size))
               {
               FEPrintf(MSG_X,MSG_Y+t,0,"      %3s %6ld | ",
                  Int2Lbl(Hist[t].lbl) ,Hist[t].count);
               if (Log && HistPrint) fprintf(tfp_log,"    %3s %6ld |",
                  Int2Lbl(Hist[t].lbl) ,Hist[t].count);
               }
            else
               {
               FEPrintf(MSG_X,MSG_Y+t,0," %5hd%3s %6ld | ",
                  Hist[t].size,Int2Lbl(Hist[t].lbl) ,Hist[t].count);
               if (Log && HistPrint) fprintf(tfp_log,"%5hd%3s %6ld |",
                  Hist[t].size,Int2Lbl(Hist[t].lbl) ,Hist[t].count);
               }
            }
         else /* IMode == SIZ_HIST || SIZM_HIST */
            {
            FEPrintf(MSG_X,MSG_Y+t,0,"%5hd %3hd %6ld | ",
               Hist[t].size,sl[Hist[t].size]->num_g,Hist[t].count);
            if (Log && HistPrint) fprintf(tfp_log,"%5hd %3hd %6ld |",
               Hist[t].size,sl[Hist[t].size]->num_g,Hist[t].count);
            }
        for(ci= 0;ci < n_star;ci++,strcat(Buff, "*")); 
        FEPrintf((MSG_X+19),MSG_Y+t,0,"%s\n",Buff); 
        if (Log && HistPrint) fprintf(tfp_log,"%s\n",Buff); 
    }
FEClrmsg(MSG_Y+t);
if (Log && HistPrint) {fprintf(tfp_log,"\n"); fflush(tfp_log);}
}        /* end of query_species */

/*-----------------------------------------------------------------------*/

#if FRONTEND == BASIC

void query_spec_d(size,lbl)
I32s size,lbl;
{
I32s l,n,nnn, tcount, SigSaveSet;
I16s empty = 0, a = 0, b = 1;
I8s old_n_star, ns_dlta;

if((size > siz_sl) || (size < 0)) return;
if((Hist == NULL)) {query_species(fe_lines);return;}

#ifndef __TURBOC__
 SigSaveSet=sigblock(SigBlockSet); 
#endif

Buff[0]=  '*'; Buff[1]=  '\0';
/*
if (HistSize > 4)
  {
  l = (int) HistSize/2; 
  n = (size < Hist[l].size)?0:(l-1); 
  }
else
  n=0L;
*/
/* for(; n < HistSize; n++) */
for(n =0; n < HistSize; n++)
   {
   if ((I32u)sl[Hist[n].size] < 4L) {empty++; continue;}
   if (IMode == GEN_HIST) 
      {
      if ((I32u)sl[Hist[n].size]->g[Hist[n].lbl] < 4L)
         {
         empty++;
         tcount = 0;
         }
      else tcount = sl[Hist[n].size]->g[Hist[n].lbl]->pop;
      }
   else if (IMode == SIZM_HIST) 
      {
   	tcount = size * sl[Hist[n].size]->num_c;
      }
   else
   	tcount = sl[Hist[n].size]->num_c;

   if ((tcount < 1L)&& (++empty > 3))
      {
      query_species(fe_lines);
#ifdef unix
      (void)sigsetmask(SigSaveSet); 
#endif
      return;
      }

   if (Hist[n].size == size)
   {
      if ((IMode == GEN_HIST) && (Hist[n].lbl != lbl)) continue;
      if( tcount > 0L )
      {
	 old_n_star    = Hist[n].nstar;
         l             = HistNStars * tcount;
         Hist[n].nstar = (l > 255)?255 :   l;
         ns_dlta = Hist[n].nstar - old_n_star;
      }
      else
      {
         old_n_star = 0L;
         Hist[n].nstar = 0;
         ns_dlta = 0L;
         Buff[0] = ' ';
      }
      if (IMode == GEN_HIST) 
         FEPrintf(MSG_X+10,MSG_Y+n,0,"%6ld", tcount);
      else
         FEPrintf(MSG_X+6,MSG_Y+n,0,"%3hd %6ld",sl[Hist[n].size]->num_g,
               tcount);
      if (Hist[n].nstar && !ns_dlta)  break;
      if (Hist[n].nstar > (fe_width))
         {
         Buff[0] = '+';
         old_n_star = 1+fe_width; 
         ns_dlta = 0;
         Hist[n].nstar=fe_width;
         }
      else
         {
         if ( ns_dlta > 1L) 
            { 
            for(l= 0;l < ns_dlta;l++,strcat(Buff, "*")); 
            ns_dlta=0;
            }
         }
      FEPrintf((MSG_X+19+old_n_star+ns_dlta),MSG_Y+n,0,"%s\n",Buff); 
      break;
   }
}
#ifndef __TURBOC__
 (void)sigsetmask(SigSaveSet); 
#endif
}

#endif /* FRONTEND == BASIC */
/*-----------------------------------------------------------------------*/
#ifdef unix
/*-----------------------------------------------------------------------*/
/* THIS CODE IS NOT FINISHED  */

void T_sig_read(sig,code,scp,addr)
    I32s  sig,code;
    /* struct sigcontext *scp; */
    I32s *scp; /* DO NOT USE !!!!! */
    I8s  *addr;
{
    FILE *inf;
    I8s data[85];
    I32s i;
    struct s_list **tsl;
    signal(SIGUSR1, T_sig_read);
    FEError(-1506,NOEXIT,NOWRITE,
        "==========================================================");
    FEError(-1507,NOEXIT,NOWRITE,"TIERRA: re-read soup_in (%s) ...",soup_fn);

    inf = fopen(soup_fn, "r");
    if (inf == NULL)
    {   FEError(-1508,EXIT,NOWRITE,
            "Tierra T_sig_read() GetSoupIn: file %s not opened, exiting",
                soup_fn);
    }
    fgets(data, 84, inf);
    sscanf(data, "tierra core:");       /* file header line */
    fgets(data, 84, inf);  /* blank line */
    fgets(data, 84, inf);
    while (strlen(data) > 3)
    {   if (!GetAVar(data))
            FEError(-1509,NOEXIT,NOWRITE,
                "Tierra T_sig_read() bad soup_in line: %s", data);
        fgets(data, 84, inf);
    }
 
 FEError(-1510,NOEXIT,NOWRITE,
         "==========================================================");
 ToggleLog(0L);
}
#endif /* unix */

/*-----------------------------------------------------------------------*/
#ifdef unix
/*-----------------------------------------------------------------------*/
/* THIS CODE IS NOT FINISHED  */

void T_sig_write(sig,code,scp,addr)
    I32s  sig,code;
    /* struct sigcontext *scp; */
    I32s *scp; /* DO NOT USE !!!!! */
    I8s  *addr;
{

signal(SIGHUP,  T_sig_write);
 FEError(-1511,NOEXIT,NOWRITE,
         "==========================================================");
 FEError(-1512,NOEXIT,WRITE,"TIERRA: writing soup ...");
 FEError(-1513,NOEXIT,NOWRITE,
         "==========================================================");

}
#endif /* unix */

/*-----------------------------------------------------------------------*/
#ifdef unix
/*-----------------------------------------------------------------------*/
/* THIS CODE IS NOT FINISHED  */

void T_sig_info(sig,code,scp,addr)
    I32s  sig,code;
    /* struct sigcontext *scp; */
    I32s *scp; /* DO NOT USE !!!!! */
    I8s  *addr;
{
I32s t_hist_print = HistPrint;
signal(SIGUSR2, T_sig_info);
if(!HistPrint) HistPrint = 1L;
  sprintf(mes[0], 
           "---------------------------------------------------------");
   sprintf(mes[1],
"InstExe.m    = %6ld  InstExec.i = %6ld  NumCells = %4ld ",
           InstExe.m,InstExe.i,NumCells);
  FEMessage(2,mes);
  if(GeneBnker)
     {
   sprintf(mes[0],
"NumGenotypes =   %4ld  NumSizes   =   %4ld", NumGenotypes, NumSizes);
  FEMessage(1,mes);
     }
   sprintf(mes[0],
           "---------------------------------------------------------");
  FEMessage(1,mes);
 query_species(20);
   sprintf(mes[0],
           "---------------------------------------------------------");
  FEMessage(1,mes);
HistPrint = t_hist_print;
}
#endif /* unix */

/*-----------------------------------------------------------------------*/

void ToggleLog(mode)
I32s mode;
{
             if (Log  && (tfp_log == NULL))
                {
                if(!new_soup) tfp_log = fopen("tierra.log","a");
                else tfp_log = fopen("tierra.log","w");
                if(tfp_log  == NULL)
                   {
                   FEError(-1514,NOEXIT,NOWRITE,
                       "Tierra ToggleLog() Problem opening - tierra.log ");
                   }
                }
            if(!Log && ( tfp_log )) {fclose(tfp_log);tfp_log = NULL;} 
            sprintf(mes[0],"TIERRA: LOG = %s, Histogram Logging = %s \n",
                   (Log)? "on" : "off",
                   (HistPrint)? "on" : "off");
            FEMessage(1,mes);
}
/* ----------------------------------------------------------------------*/
#ifdef MICRO
void Micro_Spy(ce)
Pcells ce;
{
I32s tlog,off_int;
I8s  off_char = ' ',d_data[80],cc;
            IMode= PLN_STATS;
            tlog = Log;
            Log = 0;      /*note: file not closed, we just wont scream at it */

	    if ((MicroSlice) && (ThisSlice != MicroSlice)) return;

            off_int = ce->c.ip - ce->mm.p  ;
            if (off_int > (ce->mm.s)) off_char = '+';
            if (ce->md.s >= MinCellSize)
               sprintf(d_data,"Daughter @ %-6ld + %-6ld",ce->md.p,ce->md.s);
            else
               sprintf(d_data,"NO Daughter");
            sprintf(mes[0],
         "Cell %2d:%3d %04ld%3s @ %7ld Slice=%4d  Stack[%7ld]              ",
                 ce->q.this.a, ce->q.this.i,
                 ce->mm.s,Int2Lbl( ce->d.gi),ce->mm.p,
                 ce->d.ib,ce->c.st[0]);
            sprintf(mes[1],
            "IP [%7ld](%c%-7ld) = 0x%02x %9.9s        [%7ld]         ",
                 ce->c.ip, off_char, off_int,
                 soup[ad(ce->c.ip)].inst,
                 id[soup[ad(ce->c.ip)].inst].mn, ce->c.st[1]);
            sprintf(mes[2],
        "AX [%7ld]                                   [%7ld]              ",
                    ce->c.re[0], ce->c.st[2]);
            sprintf(mes[3],
        "BX [%7ld]                                   [%7ld]              ",
                    ce->c.re[1], ce->c.st[3]);
            sprintf(mes[4],
        "CX [%7ld]                                   [%7ld]              ",
                    ce->c.re[2], ce->c.st[4]);
            sprintf(mes[5],
        "DX [%7ld]                                   [%7ld]              ",
                    ce->c.re[3], ce->c.st[5]);
            sprintf(mes[6],
   "Flag = %1.1d                                       [%7ld]             ",
            (int) ce->c.fl,ce->c.st[6]);
            sprintf(mes[7],
        "%-30.30s                 [%7ld]             ",
            d_data, ce->c.st[7]);
            sprintf(mes[8],
        "                                               [%7ld]             ",
            ce->c.st[8]);
            sprintf(mes[9],
        "                                               [%7ld]             ",
            ce->c.st[9]);
            mes[ce->c.sp][57] = '<';
            FEMessage(10,mes);
            Log= tlog;
            if (MC_step > 0L)
               {
               FEPrintf(HLP_X,HLP_Y,1,
"MICRO  |  %15.15s  ESC-Main Menu  n-Next step            ",
	       (MicroSlice)?"t-Untrack cell": "T-Track cell");
#if FRONTEND == STDIO
               printf("\n");
#endif /* FRONTEND == STDIO */
               }
            if(( MC_step  > 0L) && ((cc = FEGetc()) == 0x1b)) FEMenu();
	    if (cc == 'T') MicroSlice = ThisSlice;
	    else if (cc == 't') MicroSlice = NULL;
            if( MC_step  == 0L)
               {
               if (KEYHIT()) FEMenu();
               else sleep(1);
               }
#ifdef ALCOMM
        if ( MIsDFEnabled( TrtIPEvent ) )
           {
           TMoveIP( ThisSlice->mm.p,ThisSlice->c.ip);
           }
#endif /* ALCOMM */
}
#endif /* MICRO */

/*-----------------------------------------------------------------------*/

