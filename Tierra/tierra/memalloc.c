/* memalloc.c   9-9-92 memory allocation routines for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char     memalloc_sccsid[] = "@(#)memalloc.c        1.5     7/21/92";
#endif

#include <sys/types.h>
#include "license.h"
#include "tierra.h"
#include "extern.h"

#ifdef ALCOMM
#include "tmonitor.h"
#include "trequest.h"
#include <mlayer.h>
#endif


#ifdef MEM_CHK
#include <memcheck.h>
#endif

/* check to see if cell has write privelage at address */
I8s  IsPriv(ce, a)
Pcells  ce;
I32s  a;
{
#ifdef ERROR
    if(a >= SoupSize || a < 0)
        FEError(-600,EXIT,WRITE,
            "Tierra IsPriv() error: address %ld not in soup", a);
#endif
    if(IsInsideCell(ce, a)) return 1;
    return IsFree(a);
}

I8s IsBitPriv(ce,a,mode,track)
Pcells  ce;
I32s     a; /* address being checked */
I32s    mode, track; /* modes: 1 bit = execute, 2 bit = write, 4 bit = read */
{   if(a < 0 || a >= SoupSize)
        return 0;
    if(IsInsideCell(ce,a))
        return 1;
    else
        switch(mode)
        {
#if PLOIDY == 1
            case 1: return !soup[a].exec;
            case 2: return !soup[a].write;
            case 4: return !soup[a].read;
            case 6: return (!soup[a].read) && (!soup[a].write);
#else /* PLOIDY > 1 */
            case 1: return !soup[a][track].exec;
            case 2: return !soup[a][track].write;
            case 4: return !soup[a][track].read;
            case 6: return (!soup[a][track].read) && (!soup[a][track].write);
#endif /* PLOIDY > 1 */
            default: return 0;
        }
}

/* check to see if address is inside allocated memory cell ce */
I8s  IsInsideCell(ce, a)
Pcells  ce;
I32s  a;
{
#ifdef ERROR
    if(a >= SoupSize || a < 0)
        FEError(-601,EXIT,WRITE,
            "Tierra IsInsideCell() error: address %ld not in soup", a);
#endif
    if((ce->mm.p <= a && a < ce->mm.p + ce->mm.s) ||
       (ce->md.s > 0 &&
       (ce->md.p <= a && a < ce->md.p + ce->md.s)))
        return 1;
    return 0;
}

void  WhichCell(a, ce, md) /* find cell with address a */
I32s        a;    /* note: a must be in a cell!, call IsFree() before */
Pcells Fp  ce;   /* WhichCell() to find out if a is in a cell or not */
I8s        *md;
{   I32s  ar, ci;
    Pcells  te;

    for(ar = 0; ar < NumCelAr; ar++) for(ci = 0; ci < CelArSiz; ci++)
    {   if (ar == 0 && ci < 2)
            continue;
        te = &cells[ar][ci];
        if (te->ld)
        {   if(te->mm.p <= a && (te->mm.p + te->mm.s) > a)
            {   *ce = te; *md = 'm'; return; }
            if(te->md.p <= a && (te->md.p + te->md.s) > a)
            {   *ce = te; *md = 'd'; return; }
        }
    }
    FEError(-601,EXIT,NOWRITE,
        "Tierra WhichCell() error: address %ld not found in a cell", a);
}

/* ----------------------------------------------------------------------- */
 /* 1 bit = execute, 2 bit = write, 4 bit = read */
 /* only owner of memory has chmod privelages */
 /* return 0 on success, return 1 on error */

I8s chmode(ce, start, size, mode)
    Pcells  ce;
    I32s start, /* where in the soup to start */
         size,  /* how far to go, (will wrap around end of soup) */
         mode;  /* chmod bits, like unix, see above */
{
    I32s a = 0, t;
    I8s exec, write, read, ret = 0;

    exec =  IsBit(mode, 0);
    write = IsBit(mode, 1);
    read =  IsBit(mode, 2);
    while (a < size)
    {   t = ad(start + a);
        if (IsInsideCell(ce, t))
        {
#if PLOIDY == 1
            soup[t].exec = exec;
            soup[t].write = write;
            soup[t].read = read;
#else  /* PLOIDY > 1 */
            soup[t][ce->c.tr].exec = exec;
            soup[t][ce->c.tr].write = write;
            soup[t][ce->c.tr].read = read;
#endif  /* PLOIDY > 1 */
        }
        else ret = 1;
        a++;
    }
    return ret;
}

/* ----------------------------------------------------------------------- */

I32s mal(ce,sug_addr,sug_size,mode) /* allocate space for a new cell */
    Pcells  ce;
    I32s *sug_addr, /* returns actuall address of block, */
                    /* also suggested address for mal */
         sug_size,  /* size of block to get */
                    /* function returns actual size, or 0 on failure */
         mode;      /* which mode to use, see switch below */
{
    I32s p;
    I32s size, osize, sad;

    if (sug_size <= 0 || sug_size == ce->md.s || 
        sug_size > MaxMalMult * ce->mm.s)
        return 0;
    size = (I32s) sug_size + flaw(ce);
    if (!size)
        return 0;
    if (ce->md.s)
    {
#ifdef ERROR
        if (ce->md.p < 0 || ce->md.p >= SoupSize)
            FEError(-613,EXIT,WRITE, "Tierra mal() error 1");
#endif  /* DAN should check return val */
        chmode(ce, ce->md.p, ce->md.s, MemModeFree); 
        MemDealloc(ce->md.p, ce->md.s);
        ce->d.mov_daught = 0;
        ce->md.s = 0;
    }
    switch (mode)
    {   case 0: /* first fit */
        {   while ((p = MemAlloc(size, 0, SoupSize - 1)) < 0)
                reaper(1,0);
            break;
        }
        case 2: /* random preference */
        {   while ((p = MemAlloc(size, sad = tlrand() % (SoupSize - size),
                    MalLimit)) < 0)
                reaper(1,sad);
            break;
        }
        case 3: /* preference for mother's address */
        {   while ((p = MemAlloc(size, ce->mm.p, MalLimit)) < 0)
                reaper(1,ce->mm.p);
            break;
        }
        case 4: /* preference for dx address */
        {   while ((p = MemAlloc(size, sad = mo(ce->c.re[3], SoupSize - size),
                    MalLimit)) < 0)
                reaper(1,sad);
            break;
        }
        case 5: /* preference for top of stack address */
        {   while ((p = MemAlloc(size, sad = mo(ce->c.st[ce->c.sp],
                    SoupSize - size), MalLimit)) < 0)
                reaper(1,sad);
            break;
        }
        case 6: /* preference for suggested address (*sug_addr) */
        {   while ((p = MemAlloc(size, sad = mo(*sug_addr, SoupSize - size),
                    MalLimit)) < 0)
                reaper(1,sad);
            break;
        }
        case 1: default: /* better fit */
        {   while ((p = MemAlloc(size, -1, 0)) < 0)
                reaper(1,-1);
        }
    }
#ifdef ERROR
    if (p < 0 || p >= SoupSize)
        FEError(-614,EXIT,WRITE, "Tierra mal() error 2");
#endif
    if (!size)
        return 0;

    /* got a block, pass location (sug_addr) and size back  */

    *(sug_addr) = ce->md.p = ad(p);
    ce->md.s = size;
    ce->c.fl = 0;
    DownReperIf(ce);
    return size;
}

/* ----------------------------------------------------------------------- */

