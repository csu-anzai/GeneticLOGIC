/* tierra.c   9-9-92  main module of Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char     sccsid[] = "@(#)tierra.c        1.5 7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "declare.h"
#include "soup_in.h"
#include <sys/types.h>
#include <fcntl.h>
#ifdef unix
#include <unistd.h>
#endif

#ifdef MEM_CHK
#include <memcheck.h>
#endif

#ifdef ALCOMM

#include "tmonitor.h" 
#include "trequest.h"
#include <mlayer.h>
#include <alcomm.h>

#define                                                 _MFnCount       2
static MtDefaultRoutines        _message_fns[] = {
  { TrtPauseSim,        TSimRuncontrol },
  { TrtResumeSim,       TSimRuncontrol }
};

#define                                                 _QFnCount       2
static MtDefaultRoutines        _query_fns[] = {
  { TrtGeneralStats,    TQueryGeneralStats },
  { TrtQueryOrg,        TQueryOrganism }
};

#define                                                 _DIFnCount      4
static MtDefaultRoutines        _dfinit_fns[] = {
  { TrtOrgLifeEvent,    TInitOrgLifeEvents},
  { TrtIPEvent,         TMoveIP},
  { 1,         TMoveD} ,
  { TrtPlanEvent,       TPlan}
};

#endif         /*ALCOMM */

I32s   FindCell;
I32s   itime, mtime;
Event  FindTime;

int main(argc,argv)
    int   argc;
    char  *argv[];
{   FEStartup();         

#ifdef MEM_CHK
    mc_startcheck(FEMemCheck); /* set memcheck=on */
#endif

#ifdef ALCOMM
    _t_init_alcomm();
#endif /* ALCOMM */
    GetSoup(argc,argv); 
    if(argc > 2) FEMenu();
    life();
    WriteSoup(1);

#ifdef MEM_CHK
mc_endcheck();
#endif

    if(Log)
        fclose(tfp_log);
    FEExit(0);
}

void life() /* doles out time slices and death */
{
/*  while(InstExe.m < alive) */
    while(Generations < alive)
    {

if (KEYHIT()) FEMenu();
#ifdef __TURBOC__
    if (GoDown)
        FEError(-1000,EXIT,WRITE,
      "Tierra life() memory fragmented & running low, saving system to disk");
#endif /* __TURBOC__ */

#ifdef ALCOMM
      if ( AL_run_flag == 1 )
        {   (*slicer)();
            ReapCheck();
        }
      _t_life_bookeep();

#else /* ALCOMM */

        (*slicer)();
        ReapCheck();
#endif /* ALCOMM */
    }
}

void TimeSlice(ce, size_slice)
Pcells  ce;
I32s    size_slice;
{   I8s     a = 0, b = 0;
    I16s    di;  /* decoded instruction */
    I16s    gi;
    I32s    si, ar, ci, NumAnc, value;
    GList  Fp pgl;
    SList  Fp Fp tsl, Fp psl;
    Pcells  tce, sce;

    ce->d.ib += size_slice;
    for(is.ts = ce->d.ib; is.ts > 0; )
    {   di = FetchDecode(ce);
#ifdef EXECPROT
        if (is.eins->exec && !IsInsideCell(ce,is.oip))
            SetFlag(ce);
        else
#endif /* EXECPROT */
        (*id[di].execute)(ce);
        IncrementIp(ce);
/*                       FOR DEBUGGING PURPOSES */
        if (debug && InstExe.m >= FindTime.m && InstExe.i >= FindTime.i)
        {
                    a = b;
        }
/* */
        SystemWork(ce);
    }
}

I16s FetchDecode(ce)
Pcells  ce;
{   I16s    di;

#if PLOIDY == 1
    is.eins = &soup[ce->c.ip];
#else /* PLOIDY > 1 */
    is.eins = &soup[ce->c.ip][ce->d.tr];
#endif /* PLOIDY > 1 */
    di = is.eins->inst;
    is.oip = ce->c.ip;
    (*id[di].parse)(ce);

#ifdef MICRO
if( MC_step  > -1L) Micro_Spy(ce);
#endif

    return di;
}

void IncrementIp(ce)
Pcells  ce;
{   ce->c.ip += is.iip;
    ce->c.ip = ad(ce->c.ip);
    ce->d.ib -= is.dib;
    is.ts -= is.dib;
    if (WatchExe)
        GenExExe(ce, is.oip);
}

void SystemWork(ce)
Pcells  ce;
{   ce->d.inst += is.dib;
    if(ce->c.fl)
    {   ce->d.flags++;
        if(!ce->d.dm)
            UpRprIf(ce);
    }
    CountMutRate++;
    if(CountMutRate >= RateMut && RateMut)
    {   mutate();
        TotMut++;
        CountMutRate = tlrand() % RateMut;
    }
    if(isolate) extract(&cells[extr.a][extr.i]);
    InstExe.i++;
    if(InstExe.i > 1000000L)
    {   InstExe.i %= 1000000L; InstExe.m++;
        if(DropDead && (InstExe.m > LastDiv.m + DropDead))
        {   FEError(-1001,EXIT,WRITE,
                "Tierra SystemWork() soup has died, saving system to disk");
        }
        if(!(InstExe.m % SaveFreq)) WriteSoup(0);
        plan();
    }
}

void mutate()
{   I32s  i;

    i = tlrand() % SoupSize;
#if PLOIDY == 1
    mut_site(soup + i, tcrand());
#else /* PLOIDY > 1 */
    mut_site(soup + i, tcrand() % PLOIDY);
#endif /* PLOIDY > 1 */
    MutBookeep(i);
}

void mut_site(s, t)
HpInst  s;
I32s     t;
{   
#if PLOIDY == 1
s[0].inst ^= (1 << (tirand() % (I16s) INSTBITNUM)); 
#else /* PLOIDY > 1 */
s[0][t].inst ^= (1 << (tirand() % (I16s) INSTBITNUM)); 
#endif /* PLOIDY > 1 */
}


void ReapCheck() /* kill some cells if necessary */
{   I32s  i, t, dtime;
    Event  result;

    if(DistFreq < -.00001 || !reaped || (!DistNext.m && !DistNext.i))
        return;
    dtime = SubEvent(&InstExe, &DistNext, &result);
    if(dtime > 0)
    {   Disturb = InstExe;
        DistNext.m = DistNext.i = 0L;
        t = (I32s) (DistProp * (float) NumCells);
        if(t == NumCells)
            t--;
        for(i = 0; i < t; i++)
            reaper(0,-1);
    }
}

void reaper(ex, sad)
I32s  ex;  /* is a creature executing now ? */
I32s  sad; /* suggested address for reaping */
{   Pcells  ce; /* cell to be reaped */
    Pcells  nc; /* daughter of cell to be reaped */
    Event   result;
    I32s    reap_range,i, j, ll, ul, goon, rtime, found = 0;

    if (MalReapTol && (sad >= 0 && sad < SoupSize))
    {   ce = TopReap; i = 1; goon = 1;
        ll = sad - MalLimit;
        ul = sad + MalLimit + 1;
        while (goon)
        {   goon = 0;
            if (ex && ce == ThisSlice)
                goon = 1;
            if (ce->md.s)
            {   if ((((ce->mm.p + ce->mm.s) < ll) || (ce->mm.p > ul))
                    && (((ce->md.p + ce->md.s) < ll) || (ce->md.p > ul)))
                goon = 1;
            }
            else
            {   if (((ce->mm.p + ce->mm.s) < ll)
                    || (ce->mm.p > ul))
                goon = 1;
            }
            if (goon)
            {   ce = &cells[ce->q.n_reap.a][ce->q.n_reap.i];
                i++;
            }
            if (i > NumCells || (!ce)
                || ((ce->q.this.a == 0) && (ce->q.this.i < 2))) 
                break;
            if (!goon)
            {   found = 1;
                break;
            }
        }
    }
    if (!found)
    {   reap_range =  ReapRndProp * NumCells;
        if(reap_range < 2)
            ce = TopReap;
        else        /* pick rnd cell in top reap_range */
        {   j = tlrand() % reap_range;
            for(ce = TopReap, i = 0; i < j; i++)
            {   ce = &cells[ce->q.n_reap.a][ce->q.n_reap.i];
#ifdef ERROR
                if ((!ce) || ((ce->q.this.a ==0) && (ce->q.this.i < 2))) 
        FEError(-1002,EXIT,WRITE,"Tierra reaper() error, queues corrupted!");
#endif
            }
        }
    }
    if(ex && ce == ThisSlice)
    {   if(ce == TopReap)
            ce = &cells[ce->q.n_reap.a][ce->q.n_reap.i];
        else
            ce = &cells[ce->q.p_reap.a][ce->q.p_reap.i];
    }
    if(ex && DistFreq > -.00001 && !DistNext.m && !DistNext.i)
    {   rtime = SubEvent(&InstExe, &Disturb, &result);
        rtime = (I32s) (DistFreq * (float) rtime);
        DistNext = Disturb = InstExe;
        DistNext.m += rtime / 1000000L;
        DistNext.i += rtime % 1000000L;
        DistNext.m += DistNext.i / 1000000L;
        DistNext.i %= 1000000L;
    }
    if(NumCells == 1)
    {   FEError(-1003,EXIT,WRITE,
            "Tierra reaper() error 0, attempt to reap last creature");
    }
    /* DAN old ce = TopReap; l_top = TopReap; */
#ifdef ERROR
    if(!ce->ld || !NumCells || (!ce->mm.s && !ce->md.s))
    {   FEError(-1004,EXIT,WRITE,
            "Tierra reaper() error 1, attempt to reap non-existant cell");
    }
#endif
    if(ce->mm.s)
    {
#ifdef ERROR
        if(ce->mm.p < 0 || ce->mm.p >= SoupSize)
        {   FEError(-1005,EXIT,WRITE,
   "Tierra reaper() error 2: attemp to deallocate mother memory not in soup");
        }
#endif
        chmode(ce, ce->mm.p,ce->mm.s,MemModeFree);
              /* DAN should check return */
        MemDealloc(ce->mm.p,ce->mm.s);
    }
    if(ce->md.s && (ce->md.p > -1))
    {
#ifdef ERROR
        if(ce->md.p < 0 || ce->md.p >= SoupSize)
        {   FEError(-1006,EXIT,WRITE,
"Tierra reaper() error 3: attemp to deallocate daughter memory not in soup");
        }
#endif
        if(ce->d.ne.a != ce->q.this.a || ce->d.ne.i != ce->q.this.i)
            /* cleanup daughter cpu */
        {   nc = &cells[ce->d.ne.a][ce->d.ne.i];
            if(nc->d.is) /* cleanup daughter instruction pointer */
                RmvFrmSlicer(nc);
            NumCells--;
            InitCell(nc->q.this.a, nc->q.this.i, nc);
        }
        chmode(ce, ce->md.p,ce->md.s,MemModeFree); 
               /* DAN should check return */
        MemDealloc(ce->md.p,ce->md.s);
    }
    if(ce->md.s && ce->md.p == -1)
    {    if (ce->d.genome)
         {   tfree(ce->d.genome);
             ce->d.genome = soup + ce->mm.p;
         }
    }
    RmvFrmSlicer(ce);
    RmvFrmReaper(ce);
    ReapBookeep(ce);
/*  InitCell(ci); done in ReapBookeep(ci); */
}

I32s SubEvent(event1, event2, result) /* subtract e2 from e1 */
Event  *event1, *event2, *result;
{   result->m =  event1->m - event2->m;
    result->m += (event1->i - event2->i) / 1000000L;
    result->i =  (event1->i - event2->i) % 1000000L;
    if(result->m <= 0)
        return result->i + (result->m * 1000000L);
    if(result->i < 0)
    {   --result->m;
        result->i += 1000000L;
    }
    return result->i + (result->m * 1000000L);
}

/* --------------------------------------------------------------------- */
#ifdef ALCOMM
void _t_init_alcomm()
{
  MtStatus      iRet;
  AL_run_flag = 1;

    VPORT = 7001;
    if (( iRet = MInitialise( 0, 0,
                              _message_fns, _MFnCount,
                              _query_fns, _QFnCount,
                              M_NoFns, 0,
                              _dfinit_fns, _DIFnCount )
        ) != MsOK )
      {FEError(-1007,NOEXIT,NOWRITE,
          "Tierra _t_init_alcomm() main MInitialise error (%d)\n", iRet );}

    for(VPORT=7001;VPORT < 8000;VPORT++)
       {
       if (( iRet = MOpenPublicPort( VPORT )) != MsOK )
         {FEError(-1008,NOEXIT,NOWRITE,
             "Tierra _t_init_alcom() main MOpenPublicPort error (%d) on # %d",
             iRet, VPORT);
         }
       else break;
       }
}

void _t_life_bookeep()
{
I32s         acount=50;
MtStatus     iRet;
      if ( AL_run_flag == 1 )
        {
         if ( MIsDFEnabled( TrtIPEvent ) )
              {
              TMoveIP( ThisSlice->mm.p,ThisSlice->c.ip);
              }
        }
        else
        {
        if(acount++ > 30)
          {
          FEPrintf(0,0,1,"ALMOND: holding\n");
          acount = 0;
          }
        sleep( 1 );
        }
      if (( iRet = MServiceRequests( M_NoWait )) != MsOK )
      {sprintf(mes[0], "life MServiceRequests error (%d)\n", iRet );
      FEMessage(1,mes);}
}
#endif /* ALCOMM */

/* ----------------------- END OF TIERRA.C ----------------------------- */
