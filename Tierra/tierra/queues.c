/* queues.c   9-9-92  Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char     sccsid[] = "@(#)queues.c	1.5     7/21/92";
#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"

#ifdef MEM_CHK
#include <memcheck.h>
#endif

void IncrSliceQueue() /* increment slicer queue */
{   ThisSlice = &cells[ThisSlice->q.n_time.a][ThisSlice->q.n_time.i];
    while(!ThisSlice->ld) /* get dead cells out of queue */
        RmvFrmSlicer(ThisSlice);
}

void EntBotSlicer(ce)
Pcells  ce; /* new cell being added to bottom of slicer queue */
{   Pcells  tc; /* ThisSlice cell */
    Pcells  pc; /* previous slice cell */

    tc = ThisSlice; pc = &cells[tc->q.p_time.a][tc->q.p_time.i];
#ifdef ERROR
    if((ce->q.n_time.a != ce->q.p_time.a || ce->q.n_time.i != ce->q.p_time.i)
        && NumCells > 1)
    {   FEError(-800,EXIT,WRITE,
            "Tierra EntBotSlicer() error: cell already in slicer queue");
    }
#endif
    pc->q.n_time = ce->q.this; /* previous slice points to ce */
    ce->q.p_time = tc->q.p_time; /* ce points back at previous slice */
    ce->q.n_time = ThisSlice->q.this; /* ce points to this slice */
    tc->q.p_time = ce->q.this; /* this slice points back at ce */
}

void UpReaper(ce) /* move cell up the reaper queue */
Pcells  ce; /* ce is cell to be moved up in reaper */
{   Pcells  pe2, pe, ne;

    if(ce == TopReap) return;
#ifdef ERROR
    if(ce->q.n_reap.a == ce->q.p_reap.a && ce->q.n_reap.i == ce->q.p_reap.i
        && NumCells > 1)
    {   FEError(-801,EXIT,WRITE,
        "Tierra UpReaper() error: cell not in reaper queue");
    }
#endif
    ne = &cells[ce->q.n_reap.a][ce->q.n_reap.i];/*ne is next cell in reaper */
    pe = &cells[ce->q.p_reap.a][ce->q.p_reap.i];
        /* pe is previous cell in reaper */
    pe2 = &cells[pe->q.p_reap.a][pe->q.p_reap.i];
        /* pe2 is 2nd previous cell in reaper */
    ne->q.p_reap = pe->q.this;  /* ne points back to pe */
    ce->q.p_reap = pe2->q.this; /* ce points back to pe2 */
    ce->q.n_reap = pe->q.this;  /* ce points ahead to pe */
    pe->q.p_reap = ce->q.this;  /* pe points back to ce */
    pe->q.n_reap = ne->q.this;  /* pe points ahead to ne */
    pe2->q.n_reap = ce->q.this; /* pe2 points ahead to ce */
    if(ce == BottomReap)
        BottomReap = pe;
    if(pe == TopReap)
        TopReap = ce;
}

void DownReaper(ce)
Pcells  ce; /* ce is cell to be moved down in reaper */
{   Pcells  pe, ne, ne2;

    if(ce == BottomReap) return;
#ifdef ERROR
    if(ce->q.n_reap.a == ce->q.p_reap.a && ce->q.n_reap.i == ce->q.p_reap.i
        && NumCells > 1)
    {   FEError(-802,EXIT,WRITE,
        "Tierra DownReaper() error: cell not in reaper queue");
    }
#endif
    ne = &cells[ce->q.n_reap.a][ce->q.n_reap.i];/*ne is next cell in reaper */
    ne2 = &cells[ne->q.n_reap.a][ne->q.n_reap.i];
        /* ne2 is 2nd next cell in reaper */
    pe = &cells[ce->q.p_reap.a][ce->q.p_reap.i];
        /* pe is previous cell in reaper */
    pe->q.n_reap = ne->q.this;  /* pe points ahead to ne */
    ce->q.n_reap = ne2->q.this; /* ce points ahead to ne2 */
    ce->q.p_reap = ne->q.this;  /* ce points back to ne */
    ne->q.n_reap = ce->q.this;  /* ne points ahead to ce */
    ne->q.p_reap = pe->q.this;  /* ne points back to pe */
    ne2->q.p_reap = ce->q.this; /* ne2 points back to ce */
    if(ce == TopReap)
        TopReap = ne;
    if(ne == BottomReap)
        BottomReap = ce;
}

void UpRprIf(ce)
Pcells  ce;
{   if(ce->d.flags >= cells[ce->q.p_reap.a][ce->q.p_reap.i].d.flags)
        UpReaper(ce);
}

void DownReperIf(ce)
Pcells  ce;
{   if(ce->d.flags <= cells[ce->q.n_reap.a][ce->q.n_reap.i].d.flags)
        DownReaper(ce);
}

void EntBotReaper(ce)
Pcells  ce; /* cell to be added to the bottom of reaper queue */
{   Pcells  be = BottomReap; /* cell presently at bottom of reaper queue */

#ifdef ERROR
    if((ce->q.n_reap.a != ce->q.p_reap.a || ce->q.n_reap.i != ce->q.p_reap.i)
        && NumCells > 1)
    {   FEError(-803,EXIT,WRITE,
            "Tierra EntBotReaper() error: cell already in reaper queue");
    }
#endif
    ce->q.p_reap = BottomReap->q.this;
        /* new cell points back to old BottomReap */
    ce->q.n_reap = BottomDummy->q.this;
        /* new cell points ahead to dummy bottom */
    be->q.n_reap = ce->q.this;/* old BottomReap cell now points ahead to ce */
    BottomDummy->q.p_reap = ce->q.this;
    BottomReap = ce; /* BottomReap is now ce */
}

void RmvFrmReaper(ce)
Pcells  ce; /* cell to be removed from reaper queue */
{   Pcells  nc; /* next cell in reaper queue */
    Pcells  pc; /* previous cell in reaper queue */

    nc = &cells[ce->q.n_reap.a][ce->q.n_reap.i];
    pc = &cells[ce->q.p_reap.a][ce->q.p_reap.i];
#ifdef ERROR
    if(ce->q.n_reap.a == ce->q.p_reap.a && ce->q.n_reap.i == ce->q.p_reap.i
        && NumCells > 1)
    {   FEError(-804,EXIT,WRITE,
            "Tierra RmvFrmReaper() error: cell not in reaper queue");
    }
#endif
    if(ce == TopReap) /* TopReap changed to next cell in queue */
        TopReap = &cells[ce->q.n_reap.a][ce->q.n_reap.i];
    if(ce == BottomReap) /* BottomReap changed to previous cell in queue */
        BottomReap = &cells[ce->q.p_reap.a][ce->q.p_reap.i];
        /* previous cell points ahead to next cell: */
    pc->q.n_reap = ce->q.n_reap;
        /* next cell points back to previous cell: */
    nc->q.p_reap = ce->q.p_reap;
    ce->q.p_reap = ce->q.n_reap = ce->q.this;
        /* initialize reap queue this cell */
#ifdef ERROR
    if((ce->q.n_time.a != ce->q.p_time.a || ce->q.n_time.i != ce->q.p_time.i)
        && NumCells > 1)
    {   FEError(-805,EXIT,WRITE,
            "Tierra RmvFrmReaper() error: cell still in slicer queue");
    }
#endif
}

void RmvFrmSlicer(ce)
Pcells  ce; /* cell to be removed from slicer queue */
{   Pcells  nc; /* next cell in slicer queue */
    Pcells  pc; /* previous cell in slicer queue */

    nc = &cells[ce->q.n_time.a][ce->q.n_time.i];
    pc = &cells[ce->q.p_time.a][ce->q.p_time.i];
#ifdef ERROR
    if(ce->q.n_time.a == ce->q.p_time.a && ce->q.n_time.i == ce->q.p_time.i
        && NumCells > 1)
    {   FEError(-806,EXIT,WRITE,
            "Tierra RmvFrmSlicer() error: cell not in slicer queue");
    }
#endif
        /* previous cell points ahead to next cell: */
    pc->q.n_time = ce->q.n_time;
        /* next cell points back to previous cell: */
    nc->q.p_time = ce->q.p_time;
    if(ce == ThisSlice)
        ThisSlice = &cells[ce->q.n_time.a][ce->q.n_time.i];
    ce->q.n_time = ce->q.p_time = ce->q.this;
         /* initialize slice queue this cell */
}
