/* diskbank.c   9-9-92  disk genebank manager for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)diskbank.c	1.0        7/21/92";

#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"
#include <errno.h>
#include <sys/types.h>
#ifdef unix
#include <dirent.h>
#endif               /* unix */
#ifdef __TURBOC__
#include <dir.h>
#include <dos.h>
#define d_name ff_name
#endif               /* __TURBOC__ */

#ifdef MEM_CHK
#include <memcheck.h>
#endif

void Inject(g, size, sad, tol, disk, rrpi)
FpInst  g;     /* pointer to genome */
I32s    size;  /* size of genome */
I32s    sad;   /* suggested address for placement of genome */
I32s    tol;   /* tolerance placement of genome */
I32s    disk;  /* 1 = this genome comes from the disk */
float   *rrpi; /* reap rand prop for injection */
{   float   tReapRndProp = ReapRndProp;
    I32s    osize, j, k;
    I16s   gi;
    Pcells  ce;
    FpInst  si;
    Event    SizGen;
    GlIndex  GiHash;

    ce = GetFreeCell();   /* get a cell structure */
    ce->ld = 1;
    ce->d.gen.size = ce->mm.s = size;
    ReapRndProp = *rrpi;   /* allocate the needed memory */
    while ((ce->mm.p = MemAlloc(size, sad, tol)) < 0)
        reaper(1,sad);
    ReapRndProp = tReapRndProp;
    ce->c.ip = ce->mm.p;
    si = ce->d.genome = soup + ce->c.ip;

    if (ce == &cells[0][2])
    {   ce->q.p_reap = TopDummy->q.this;
        ce->q.n_reap = BottomDummy->q.this;
        TopDummy->q.n_reap = ce->q.this;
        BottomDummy->q.p_reap = ce->q.this;
    }
    else
    {   EntBotSlicer(ce);
        EntBotReaper(ce);
    }
    ce->d.is = 1;

    for (j = 0; j < size; j++, si++) /* put genome in soup */
#if PLOIDY == 1
        si[0] = g[j];
#else  /* PLOIDY == 1 */
    for (k = 0; k < PLOIDY; k++)
        si[0][k] = g[j][k];
#endif /* PLOIDY == 1 */

    if (GeneBnker) /* determine genotype, record in genebanker */
    {   GiHash = CheckGenotype(ce->d, 21);    /* check .gen files */
        ce->d.hash = GiHash.si;
        ce->d.gi = gi = GiHash.gi;
        strcpy(ce->d.gen.label, Int2Lbl(GiHash.gi));
        ce->d.parent = sl[size]->g[gi]->parent;
        SizGen = DivGenBook(NULL, ce, InstExe, reaped, 0, 0, disk);
        NumGenotypes += SizGen.i;
        NumSizes += SizGen.m;
    }
    OutDisk((I32s) 'b', ce);
}

void InjectFromBank(crit, sad, tol)
    I8s     *crit;
    I32s    sad;   /* suggested address for placement of genome */
    I32s    tol;   /* tolerance placement of genome */
{
    float  rrpi = 1;
    I32s   size;
    GList  *g;

    g = GetAGen(crit);
    sscanf(crit, "%4ld", &size);
    Inject(g->genome, size, sad, tol, 1, &rrpi);
    FreeGen(g);
}

void FreeGen(g)
    GList  *g;
{   if (g)
    {   if (g->genome)
        {   tfree(g->genome);
            g->genome = NULL;
        }
        if (g->gbits)
        {   tfree(g->gbits);
            g->gbits = NULL;
        }
        tfree(g);
        g = NULL;
    }
}

GList * GetAGen(crit)
    I8s     *crit;
{
    I32s   size;
    I16s   n;
    GList  *g;
    char   cpath[128], gen[4];
    FILE   *fp;
    head_t head;
    indx_t *indx, *tindx, gindx;

    sscanf(crit, "%4ld%3s", &size, gen);
    sprintf(cpath, "%s%04ld.gen", GenebankPath, size);
    if (!(fp = open_ar(cpath, size, GFormat, 0)))
    {   FEError(-1306,EXIT,NOWRITE,
            "Tierra InjectFromBank() unable to open genome file %s\n",cpath);
    }
    head = read_head(fp);
#ifdef __TURBOC__
    indx = &gindx;
    n = find_gen(fp, indx, gen, head.n);
    tindx = indx;
#else  /* __TURBOC__ */
    indx = read_indx(fp, &head);
    n = find_gen(fp, indx, gen, head.n);
    tindx = &indx[n];
#endif /* __TURBOC__ */
    g = get_gen(fp, &head, tindx, n);
    fclose(fp);
#ifndef __TURBOC__
    if (indx)
    {   thfree(indx);
        indx = NULL;
    }
#endif  /* __TURBOC__ */
    return g;
}

void extract(ce)
    Pcells ce;
{
    I16u i, j;
    I32s size;
    I32s ip;
    Pgl g;
    FILE *fp;
    head_t head;
    indx_t *indx, gindx;

    if (!GeneBnker)
        return;
    isolate = 0;
#ifdef IBM3090
    sprintf(Buff, "%04ld.gen.d", ce->d.gen.size);
#else
    sprintf(Buff, "%s%04ld.gen", GenebankPath, ce->d.gen.size);
#endif
    size = ce->d.gen.size;
    g = sl[size]->g[ce->d.gi];
    sprintf(ExtrG, "%04ld%s @ %ld", g->gen.size, g->gen.label, g->pop);
#if FRONTEND == STDIO
sprintf(mes[0], "extract: %s", ExtrG);
FEMessage(1,mes);
#else /* FRONTEND == STDIO */
    if (Log) fprintf(tfp_log, "ex = %s\n", ExtrG);
#endif /* FRONTEND == STDIO */

/* DAN open an archive, if it does not exist, create it */
    if (!(fp = open_ar(Buff, ce->d.gen.size, GFormat, -1)))
    {   FEError(-200,EXIT,NOWRITE,
            "Tierra extract() Unable to open extract file %s",Buff);
    }
    head = read_head(fp);
#ifdef __TURBOC__
    indx = &gindx;
#else  /* __TURBOC__ */
    indx = read_indx(fp, &head);
#endif /* __TURBOC__ */
    add_gen(fp, &head, &indx, g);
#ifndef __TURBOC__
    if (indx)
    {   thfree(indx);
        indx = NULL;
    }
#endif /* __TURBOC__ */
    fclose(fp);
    NumGenDG++;
}
