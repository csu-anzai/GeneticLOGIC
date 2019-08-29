/* rambank.c   9-9-92  rambank manager for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)rambank.c	1.0        7/21/92";

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

/*
 * CheckGenotype(ce, flags)
 * Check if cell ce is a new genotype.  If it is a new genotype, it will be
 * assigned a unique label.  If this genotype is not present in the RAM
 * genebank, it will be placed there, but there will be no demographic
 * information updated (this is not assumed to be a birth or death).
 * 
 *  flags: bit 0  (1): check .gen files
 *  flags: bit 1  (2): check .tmp files
 *  flags: bit 2  (4): check files even if rambank does not indicate presence
 *      of genotype on disk (used for startup of old or new soups).
 *  flags: bit 3  (8): use all files in the genebank to assemble the list in
 *      the rambank.  Each time a new size is checked, all genomes of that
 *      size in the genebank will become listed in the rambank as being on
 *      the disk (used for startup of old soups and for cumulative genebanks).
 *      However, the genomes will remain on disk until they are actually
 *      accessed, at which time they will be placed in the genequeue in RAM.
 *  flags: bit 4 (16): when reading a file from the disk genebank,
 *      zero bit 1 of the tgl->bits field, otherwise preserve it.
 * 
 *      if soup_in variable CumGeneBnk is non zero, bit 3 is forced ON 
 * 
 *      On the CM5, this function resides on the genebank processor.
 */

GlIndex CheckGenotype(ce, flags)
    Dem   ce;  /* demography structure of cell to be checked */
    I32s  flags;
{
    I32s si;
    GlIndex  GiHash;

    if(CumGeneBnk)
        SetBit(&flags,3,ONE);
    si = ce.gen.size;
    GiHash.si = Hash(si, ce.genome);
	ce.gi = Lbl2Int(ce.gen.label);
    if (IsNewSize(si))
        NewSize(&ce, flags);
    if ((GiHash.gi = IsInGenQueue(&ce, GiHash.si)) >= 0)
    {   gq_movtop(sl[si]->g[GiHash.gi]);
        return GiHash;
    }
#ifndef CM5
    if ((GiHash.gi = IsInGenBank(&ce, GiHash.si, flags)) >= 0)
        return GiHash;
#endif /* CM5 */
    GiHash.gi = NewGenotype(&ce, GiHash.si);
        /* register new genotype in the lists */
    return GiHash;
}

Event DivGenBook(ce, nc, InstExe, reaped, mom, same, disk)
    Pcells  ce, nc; /* ce = mother cell, nc = daughter cell */
    Event   InstExe; /* current time */
    I32s    reaped; /* 1 = the reaper has acted */
    I32s    mom;    /* 1 = do bookeeping on mother as well */
    I32s    same;   /* 1 = daughter is same genotype as mother */
    I32s    disk;   /* 1 = this creature originated from the disk (Inject) */
{   GList   *tgl, *tcgl;
    float   maxp, maxi;
    int     si, gi;
    Event   SizGen;

    SizGen.i = SizGen.m = 0;
    if (mom) /* this code deals only with the mother of the cell being born */
    {   tcgl = sl[si = ce->d.gen.size]->g[gi = ce->d.gi]; /* mother GList */
        if (tcgl && (I32u) tcgl <= 4)
        {
#ifdef CM5
            FEError(-100,EXIT,NOWRITE, 
                "Tierra DivideBookeep() mother genotype missing\n");
#endif /* CM5 */
            tcgl = sl[si]->g[gi] = gq_read(si, gi); /* mother GList */
        }
        if ((I32u) tcgl <= 4)
            FEError(-100,EXIT,NOWRITE, 
                "Tierra DivideBookeep() mother genotype missing\n");
        if (ce->d.fecundity == 1 && !ce->d.mut && !ce->d.flaw)
            tcgl->d1 = ce->d.d1;
        else if (ce->d.fecundity == 2 && !ce->d.mut && !ce->d.flaw)
        {   tcgl->d2.inst = ce->d.inst + 1 - ce->d.d1.inst;
            tcgl->d2.flags = ce->d.flags - ce->d.d1.flags;
            tcgl->d2.mov_daught = ce->d.mov_daught;
            tcgl->d2.BreedTrue = same;
        }
    }

/* the following code deals only with the cell being "born" */

    tgl = sl[si = nc->d.gen.size]->g[nc->d.gi];    /* new cell GList */
    if (!tgl->pop)
    {   SizGen.i = 1;
/*      NumGenotypes++; */
        sl[si]->num_g++;
        if (disk)
        {   NumGenDG++;
            SetBit(&tgl->bits, 0, 1);
            SetBit(&tgl->bits, 1, 0);
        }
    }
    tgl->pop++;
    if (!sl[si]->num_c)
        SizGen.m = 1;
/*      NumSizes++; */
    sl[si]->num_c++;
#if FRONTEND != STDIO
    if ((IMode == SIZ_HIST)|| (IMode == SIZM_HIST) || (IMode == GEN_HIST))
        query_spec_d(si,nc->d.gi);
#endif /* FRONTEND != STDIO */
/* this might be a good place to keep track of multiple parental genotypes. */
    if (reaped)
    {   maxp = (float) tgl->pop / (float) NumCells;
        if (maxp > tgl->MaxPropPop)
        {   tgl->MaxPropPop = maxp;
            tgl->mpp_time   = InstExe;         
        }
        maxi = (float) tgl->pop * nc->d.gen.size / (float) SoupSize;
        if (maxi > tgl->MaxPropInst)
            tgl->MaxPropInst = maxi;
    }
#ifndef CM5
    /* criteria for saving genotype to disk */
    if (reaped && tgl->pop >= SavMinNum
        && ((!IsBit(tgl->bits, 0) && (tgl->MaxPropPop > SavThrPop
        || tgl->MaxPropInst > SavThrMem * .5))
        || (!IsBit(tgl->bits, 1) && (maxp > SavThrPop
        || maxi > SavThrMem * .5))))
    {   if (!IsBit(tgl->bits, 0))
        {   SetBit(&tgl->bits, 0, 1);
            SetBit(&tgl->bits, 1, 1);
            extract(nc);
        }
        else
        {   SetBit(&tgl->bits, 1, 1);
            sprintf(ExtrG, "%04ld%s @ %ld v", tgl->gen.size, tgl->gen.label,
                (GeneBnker)? tgl->pop : 0L);
#if FRONTEND == STDIO
            sprintf(mes[0], "extract: %s", ExtrG);
            FEMessage(1,mes);
#else /* FRONTEND == STDIO */
            if (Log)
                fprintf(tfp_log, "ex = %s\n", ExtrG);
#endif /* FRONTEND == STDIO */
        }
    }
#endif /* CM5 */
    return SizGen;
}

Event ReapGenBook(ce)
    Pcells  ce;
{   Pgl tgl;
    I32s  si = ce->d.gen.size;
    I16s  gi = ce->d.gi;
    Event   SizGen;

    SizGen.i = SizGen.m = 0;
    tgl = sl[si]->g[gi];
#ifdef ERROR
    if (gi >= sl[si]->a_num)
        FEError(-101,EXIT,NOWRITE, 
            "Tierra ReapBookeep() genotype %hd out of range\n", gi);
    if ((I32u) tgl <= 4)
        FEError(-102,EXIT,NOWRITE, 
            "Tierra ReapBookeep() genotype %hd not in genebank\n", gi);
#endif /* ERROR */
    tgl->pop--;    /* this is a segmentation fault waiting to happen! */
    if (!tgl->pop)
    {   if ((I32u) tgl > 4 && !IsBit(tgl->bits, 0))
        {   if (tgl->genome)
            {   tfree(tgl->genome);
                tgl->genome = NULL;
            }
            if (tgl->gbits)
            {   tfree(tgl->gbits);
                tgl->gbits = NULL;
            }
            gq_rem(tgl);
            tfree(tgl);
            sl[si]->g[gi] = NULL;
        }
        else
            SetBit(&tgl->bits, 1, 0);
/*      NumGenotypes--; */
        SizGen.i = -1;
        sl[si]->num_g--;
    }
    sl[si]->num_c--;
    if (!sl[si]->num_c)
    {   SizGen.m = -1;
/*      NumSizes--; */
#ifdef ERROR
        if (sl[si]->num_g)
            FEError(-103,NOEXIT,NOWRITE, 
                "Tierra ReapBookeep() genotypes but no individuals\n");
#endif /* ERROR */
    }
#if FRONTEND != STDIO
    if ((IMode == SIZ_HIST)|| (IMode == SIZM_HIST) || (IMode == GEN_HIST))
        query_spec_d(si,gi);
#endif /* FRONTEND != STDIO */
    return SizGen;
}

I8s IsNewSize(si)
    I32s si;
{
    if (si < siz_sl && sl[si])
        return 0;
    return 1;
}

void NewSize(ce, flags)
    Dem   *ce;
    I32s  flags;
{
    I32s i, j, si;
    I16s gi;
    SList Fp Fp  tsl;
    GList Fp Fp  tgl;
    Pgl sgl = NULL;
#ifndef CM5
    FILE *afp;
    head_t head;
    indx_t *indx, *tindx, gindx;
#endif /* CM5 */

    si = ce->gen.size;
    if (si >= siz_sl)
    {   tsl = (SList Fp Fp) trecalloc(sl,
            sizeof(SList Fp) * (si + 1),
            sizeof(SList Fp) * siz_sl);
        if (tsl)
            sl = tsl;
        else if (sl)
        {   tfree(sl);
            sl = NULL;
            FEError(-300,EXIT,WRITE, "Tierra NewSize() sl trecalloc error");
        }
#ifndef __TURBOC__
        for (i = siz_sl; i <= si; i++)
            sl[i] = NULL;
#endif /* __TURBOC__ */
        siz_sl = si + 1;
#ifdef ERROR
        sprintf(mes[0], "genebank: recalloc, siz_sl = %ld", siz_sl - 1);
        FEMessage(1,mes);
#endif
    }
    sl[si] = (SList *) tcalloc(1, sizeof(SList));
    sl[si]->a_num = 20;
    sl[si]->g = (GList **) tcalloc(20, sizeof(GList *));
#ifndef CM5
    if (IsBit(flags, 3))  /* use for old soups, and cumulative genebanks */
    {
#ifdef IBM3090
        sprintf(Buff, "%04ld.gen.d", si);
#else /* IBM3090 */
        sprintf(Buff, "%s%04ld.gen", GenebankPath, si);
#endif /* IBM3090 */
        afp = fopen(Buff, "rb");
        if (!afp) return;
        head = read_head(afp);
#ifdef __TURBOC__
        indx = &gindx;
#else  /* __TURBOC__ */
        indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
        for (i = head.n - 1; i >= 0; i--)
        {
#ifdef __TURBOC__
            find_gen(afp, indx, "---", i);
            tindx = indx;
#else  /* __TURBOC__ */
            tindx = &indx[i];
#endif /* __TURBOC__ */
            sgl = get_gen(afp, &head, tindx, i);
            gi = Lbl2Int(sgl->gen.label);
            if (gi >= sl[si]->a_num)
            {   tgl = (GList Fp Fp) trecalloc(sl[si]->g,
                    sizeof(GList *) * (gi + 1),
                    sizeof(GList *) * sl[si]->a_num);
                if (tgl)
                    sl[si]->g = tgl;
                else if (sl[si]->g)
                {   tfree(sl[si]->g);
                    sl[si]->g = NULL;
                    FEError(-301,EXIT,WRITE,
                        "Tierra NewSize() sl[si]->g trecalloc error");
                }
#ifndef __TURBOC__
                for (j = sl[si]->a_num; j <= gi; j++)
                    sl[si]->g[j] = NULL;
#endif /* __TURBOC__ */
                sl[si]->a_num = gi + 1;
            }
            sl[si]->g[gi] = (Pgl)1;    /* permanent genotype name */
            if (sgl)
            {   if (sgl->genome)
                {   tfree(sgl->genome);
                    sgl->genome = NULL;
                }
                if (sgl->gbits)
                {   tfree(sgl->gbits);
                    sgl->gbits = NULL;
                }
                tfree(sgl);
                sgl = NULL;
            }
        }
        fclose(afp);
#ifndef __TURBOC__
        if (indx)
        {   thfree(indx);
            indx = NULL;
        }
#endif /* __TURBOC__ */
    }
#endif /* CM5 */
}

I16s IsInGenQueue(ce, hash)/* returns the index of the genotype in the list */
    Dem   *ce;
    I32s  hash;
{
    I32s si = ce->gen.size;
    I16s gi = ce->gi, i;
    GList  *tgl;

    if (gi >= 0)
    {   if (gi < sl[si]->a_num && (I32u) sl[si]->g[gi] > 4)
            return gi;
        return -1;
    }
    for (i = 0; i < sl[si]->a_num; i++)
    {   tgl = sl[si]->g[i];
#ifdef ERROR
        if ((I32u) tgl > 4)
        {   if (IsSameGen(si, ce->genome, tgl->genome))
            {   if (hash != tgl->hash)
                    FEError(-1501,EXIT,WRITE,
                "Tierra IsInGenQueue() error: IsSameGen, but not same hash");
            }
        }
#endif /* ERROR */
        if ((I32u) tgl > 4 && hash == tgl->hash &&
            IsSameGen(si, ce->genome, tgl->genome))
                return i;
    }
    return -1;
}

/*
 * Check to see if ce is in the disk genebank.  This will require the reading
 * of successive genotypes of this size from the .gen files on disk.
 * Each genotype that is read will be placed in the genequeue and the complete
 * genome will be placed in the rambank.
 */

#ifndef CM5

I16s IsInGenBank(ce, hash, flags)
    Dem   *ce;
    I32s  hash, flags;
{
    static char *ext[] = {"gen", "tmp"};
    I32s i, si = ce->gen.size;
    I32u t, j, n;
    I16s gi = ce->gi;
    Pgl g;
    FILE *afp;
    head_t head;
    indx_t *indx, *tindx, gindx;
    GList Fp Fp  tgl;

    /*
     * return -1 if we are looking for a specific geneotype, and it does not
     * appear in the genequeue list, and we are not starting up a soup
     */
    if(gi >= 0 && (gi >= sl[si]->a_num || !sl[si]->g[gi]) && !IsBit(flags, 2))
        return -1;
    for (i = 0; i < 2; i++) if (IsBit(flags, i))
    {
#ifdef IBM3090
        sprintf(Buff, "%04ld.%s.d", si, ext[i]);
#else
        sprintf(Buff, "%s%04ld.%s", GenebankPath, si, ext[i]);
#endif
        if (afp = fopen(Buff, "rb"))
        {   head = read_head(afp);
#ifdef __TURBOC__
            indx = &gindx;
#else  /* __TURBOC__ */
            indx = read_indx(afp, &head);
#endif /* __TURBOC__ */
        }
        else continue;
        if (gi >= 0)  /* if we know what genotype we are looking for */
        {   if (gi >= sl[si]->a_num)
            {   tgl = (GList Fp Fp) trecalloc(sl[si]->g,
                    sizeof(GList Fp) * (gi+1),
                    sizeof(GList Fp) * sl[si]->a_num);
                if (tgl)
                    sl[si]->g = tgl;
                else if (sl[si]->g)
                {   tfree(sl[si]->g);
                    sl[si]->g = NULL;
                    FEError(-302,EXIT,WRITE,
                        "Tierra IsInGenBank() sl[si]->g trecalloc error");
                }
#ifndef __TURBOC__
                for (j = sl[si]->a_num; j <= gi; j++)
                    sl[si]->g[j] = NULL;
#endif /* __TURBOC__ */
                sl[si]->a_num = gi + 1;
            }
            n = find_gen(afp, indx, Int2Lbl(gi), head.n);
            if (n < head.n)
            {
#ifdef __TURBOC__
                tindx = indx;
#else  /* __TURBOC__ */
                tindx = &indx[n];
#endif /* __TURBOC__ */
                sl[si]->g[gi] = g = get_gen(afp, &head, tindx, n);
                if (IsBit(flags, 4))
                    SetBit(&g->bits, 1, 0);
                gq_add(g);
#ifdef ERROR
                if (IsSameGen(si, (FpInst) (ce->genome), g->genome))
                {   if (hash != g->hash)
                        FEError(-1503,EXIT,WRITE,
                 "Tierra IsInGenBank() error: IsSameGen, but not same hash");
                }
#endif /* ERROR */
                /* if disk genotype matches soup genotype */
                /* name cell and put genotype in genequeue */
                if (hash == g->hash &&
                    IsSameGen(si, (FpInst) (ce->genome), g->genome))
                {   ce->gen = g->gen;
                    ce->gi = gi;
#ifndef __TURBOC__
                    if (indx)
                    {   thfree(indx);
                        indx = NULL;
                    }
#endif /* __TURBOC__ */
                    fclose(afp);
                    return gi;
                }
            }
        }
        else /* we don't know what genotype we are looking for */
        /*
         * check only genotypes that are listed in the rambank as on
         * disk, but whose genomes are not held in the rambank
         * (0 < sl[si]->g[j] <= 4); or which are not listed in the
         * rambank at all (!sl[si]->g[j]), if bit 2 is set, which
         * means we are starting a new or old soup and don't have
         * a list of what is on disk. 
         */
        for (j = 0; j < sl[si]->a_num; j++)
        {   if (!((I32s) sl[si]->g[j] > 0 && (I32s) sl[si]->g[j] <= 4
                    || !sl[si]->g[j] && IsBit(flags, 2)))
                continue;
            n = find_gen(afp, indx, Int2Lbl(j), head.n);
            if (n < head.n)
            {
#ifdef __TURBOC__
                tindx = indx;
#else  /* __TURBOC__ */
                tindx = &indx[n];
#endif /* __TURBOC__ */
                sl[si]->g[j] = g = get_gen(afp, &head, tindx, n);
                if (IsBit(flags, 4))
                    SetBit(&g->bits, 1, 0);
                gq_add(g);
                /* if disk genotype matches soup genotype */
                /* name cell and put genotype in genequeue */
                if (hash == g->hash &&
                    IsSameGen(si, ce->genome, g->genome))
                {   ce->gen = g->gen;
                    ce->gi = j;
#ifndef __TURBOC__
                    if (indx)
                    {   thfree(indx);
                        indx = NULL;
                    }
#endif /* __TURBOC__ */
                    fclose(afp);
                    return j;
                }
            }
        }
        if (afp)
        {
#ifndef __TURBOC__
            if (indx)
            {   thfree(indx);
                indx = NULL;
            }
#endif /* __TURBOC__ */
            fclose(afp);
        }
    }
    return -1;
}

#endif /* CM5 */

/*
 * add a new genotype to the RAM list
 */

I16s NewGenotype(ce, hash)
    Dem   *ce;
    I32s  hash;
{
    GList  *tgl;
    I32s   i, j, size = ce->gen.size;
    I16s   gi;
    I8s    found = 0;
    GList Fp Fp  tglp;

    /* find a free name if there is one */
    for (i = 0; i < sl[size]->a_num; i++) if (!sl[size]->g[i])
    {   gi = i;
        found = 1;
        break;
    }
    if (!found)
    {   gi = sl[size]->a_num;
        tglp = (GList Fp Fp) trecalloc(sl[size]->g,
            sizeof(GList Fp) * (gi + 4),
            sizeof(GList Fp) * gi);
        if (tglp)
            sl[size]->g = tglp;
        else if (sl[size]->g)
        {   tfree(sl[size]->g);
            sl[size]->g = NULL;
            FEError(-303,EXIT,WRITE,
                "Tierra NewGenotype() sl[size]->g trecalloc error");
        }
#ifndef __TURBOC__
        for (i = gi; i < gi + 4; i++)
            sl[size]->g[i] = NULL;
#endif /* __TURBOC__ */
        sl[size]->a_num += 4;
    }
    sl[size]->g[gi] = tgl =
        (GList *) tcalloc(1, sizeof(GList));
    tgl->gen.size = ce->gen.size = size;
    tgl->genome = (FpInst) tcalloc(size, sizeof(Instruction));
    if (tgl->genome == NULL)
        FEError(-304,EXIT,WRITE, "Tierra NewGenotype() tcalloc error 1");
    tgl->gbits = (FpGenB) tcalloc(size, sizeof(GenBits));
    if (tgl->gbits == NULL)
        FEError(-305,EXIT,WRITE, "Tierra NewGenotype() tcalloc error 2");
    gq_add(tgl);
    for (i = 0; i < size; i++) 
#if PLOIDY == 1
        tgl->genome[i] = ce->genome[i];
#else
        for (j = 0; j < PLOIDY; j++)
        tgl->genome[i][j] = ce->genome[i][j];
#endif
    strcpy(tgl->gen.label, Int2Lbl(gi));
    tgl->originC = time(NULL);
    tgl->originI = InstExe;
    tgl->parent = ce->parent;
    tgl->bits = 0;
    tgl->hash = hash;
    tgl->pop = 0;
    if (reaped)
    {   tgl->MaxPropPop = (float) 1 / (float) NumCells;
        tgl->MaxPropInst = (float) size / (float) SoupSize;
        tgl->mpp_time = InstExe;
    }
    tgl->ploidy = ce->ploidy;
    tgl->track = ce->tr;
    return gi;
}

void gq_add(p)
    GList  *p;
{
    if (!NumGenRQ++)
    {   gq_top = gq_bot = p->a = p->b = p;
        return;
    }
    /* NumGenotypes hasn't been updated yet so add 1 in test */
    while (NumGenRQ > RamBankSiz && NumGenRQ > NumGenotypes + 1)
        gq_swap();
    p->b = gq_top;
    gq_top = gq_top->a = p->a = p;
}

I8s gq_swap()
{   GList  *p;
    I8s saved = 0;
    FILE *fp;
    head_t head;
    indx_t *indx, gindx;

    p = gq_bot;
    while (p->pop > 0 && p != gq_top)
        p = p->a;
    if (p->pop > 0)
    {   if (gq_bot != gq_top)
        {   p = gq_bot;
            GoDown = 1;
            IMode = PLN_STATS;
            sprintf(mes[0], "gq_swap: NumGenRQ = %ld  NumGenotypes = %ld\n",
                NumGenRQ, NumGenotypes);
            sprintf(mes[1],
                "         all genotypes extant, living genome deleted\n");
            sprintf(mes[2],
  "system coming down, then bring back up, to defragment memory, or:\n");
            sprintf(mes[3],
"try higher SavThrMem & SavThrPop, lower SoupSize, or turn off genebanker\n");
            FEMessage(4,mes);
        }
        else
        {   sprintf(mes[0], "gq_swap: NumGenRQ = %ld  NumGenotypes = %ld\n",
                NumGenRQ, NumGenotypes);
            sprintf(mes[1],
                "         attempt to swap out last living genome\n");
            FEMessage(2,mes);
            FEError(-306,EXIT,NOWRITE,
"try higher SavThrMem & SavThrPop, lower SoupSize, or turn off genebanker\n");
            return 0;
        }
    }
    saved = IsBit(p->bits, 0);
    sprintf(Buff,
#ifdef IBM3090
        "%04ld.%s.d",
#else  /* IBM3090 */
        "%s%04ld.%s", GenebankPath,
#endif /* IBM3090 */
        p->gen.size, "gen");
    if (!(fp = open_ar(Buff, p->gen.size, GFormat, -1)))
        FEError(-307,EXIT,WRITE,    
            "Tierra gq_swap() unable to open genome file %s",Buff);
    head = read_head(fp);
#ifdef __TURBOC__
    indx = &gindx;
#else  /* __TURBOC__ */
    indx = read_indx(fp, &head);
#endif /* __TURBOC__ */
    add_gen(fp, &head, &indx, p);
#ifndef __TURBOC__
    if (indx)
    {   thfree(indx);
        indx = NULL;
    }
#endif /* __TURBOC__ */
    fclose(fp);
    gq_rem(p);
    if (p)
    {   if (p->gbits)
        {   tfree(p->genome);
            p->genome = NULL;
        }
        if (p->gbits)
        {   tfree(p->gbits);
            p->gbits = NULL;
        }
        sl[p->gen.size]->g[Lbl2Int(p->gen.label)] = (Pgl) saved;
        tfree(p);
        p = NULL;
    }
    return 1;
}

void gq_rem(p)
    GList  *p;
{
    if (gq_top == gq_bot)
        gq_top = gq_bot = 0;
    else if (p == gq_top)
        gq_top = p->b->a = p->b;
    else if (p == gq_bot)
        gq_bot = p->a->b = p->a;
    else
    {   p->a->b = p->b;
        p->b->a = p->a;
    }
    NumGenRQ--;
}

void gq_movtop(p)
    GList  *p;
{
    if (p == gq_top)
        return;
    gq_rem(p);
    gq_add(p);
}

GList *gq_read(si, gi)
{   GList *p = sl[si]->g[gi];
    I16s n;
    FILE *fp;
    head_t head;
    indx_t *indx, *tindx, gindx;

    if ((I32u) p > 4)
        return p;
    sprintf(Buff,
#ifdef IBM3090
        "%04ld.%s.d",
#else
        "%s%04ld.%s", GenebankPath,
#endif
        si, (I32u) p == 1 ? "gen" : "mem");
    if (!(fp = open_ar(Buff, si, GFormat, 0)))
        FEError(-308,EXIT,WRITE,    
            "Tierra gq_read() unable to open genome file %s",Buff);
    head = read_head(fp);
#ifdef __TURBOC__
    indx = &gindx;
#else  /* __TURBOC__ */
    indx = read_indx(fp, &head);
#endif /* __TURBOC__ */
    n = find_gen(fp, indx, Int2Lbl(gi), head.n);
#ifdef __TURBOC__
    tindx = indx;
#else  /* __TURBOC__ */
    tindx = &indx[n];
#endif /* __TURBOC__ */
    p = get_gen(fp, &head, tindx, n);
    fclose(fp);
#ifndef __TURBOC__
    if (indx)
    {   thfree(indx);
        indx = NULL;
    }
#endif /* __TURBOC__ */
    gq_add(p);
    return p;
}

void printq()
{   GList  *p;
    int    i = 1;

    printf("%ld:B ", NumGenRQ);
    if (p = gq_bot)
    {   printf("%ld%s[%ld] ", p->gen.size, p->gen.label, p->pop);
        while (p != gq_top)
        {   p = p->a, i++;
            printf("%ld%s[%ld] ", p->gen.size, p->gen.label, p->pop);
        }
    }
    printf("%d:T\n", i);
}

#ifdef ERROR

void VerifyGB() /* verify genebank */
{   I32s  gNumSizes = 0, cNumSizes = 0, cgNumSizes = 0;
    I32s  gNumGenot = 0, cNumGenot = 0, cgNumGenot = 0;
    I32s  gNumCells = 0, cNumCells = 0, cgNumCells = 0;
    I32s  cgsNumGenot = 0, ggNumGenot = 0;
    I32s  cgsNumCells = 0, ggNumCells = 0;
    I32s  tsiz_sl = 1, si, ar, ci;
    I16s  gi;
    Pcells ce;
    GList  Fp pgl;
    SList  Fp Fp tsl, Fp psl;

    /* begin cells array check */
    tsl = (SList Fp Fp) tcalloc(1, sizeof(SList Fp));
    for (ar = 0; ar < NumCelAr; ar++) for (ci = 0; ci < CelArSiz; ci++)
    {   if (ar == 0 && ci < 2)
            continue;
        ce = &cells[ar][ci];
        if (ce->ld)
        {   cNumCells++;
            si = ce->d.gen.size;
            if (si >= siz_sl)
                FEError(-110,EXIT,WRITE,
                 "Tierra VerifyGB() size %ld out of range in genebank\n", si);
            psl = sl[si];
            if (!psl)
                FEError(-111,EXIT,WRITE,
                 "Tierra VerifyGB() sl[%ld] not allocated in genebank\n", si);
            gi = ce->d.gi;
            if (gi >= psl->a_num)
                FEError(-112,EXIT,WRITE,
               "Tierra VerifyGB() genome %hd out of range in genebank\n", gi);
            pgl = psl->g[gi];
            if ((I32u) pgl < 4)
                FEError(-113,EXIT,WRITE,
                 "Tierra VerifyGB() gl[%hd] not allocated in genebank\n", gi);
            if (!IsSameGen(si, soup + ce->mm.p, pgl->genome))
                FEError(-114,EXIT,WRITE,
                    "Tierra VerifyGB() cell and genebank do not match\n");
            if (si >= tsiz_sl)
            {   tsl = (SList Fp Fp) trecalloc(tsl,
                    (si + 1) * sizeof(SList Fp), tsiz_sl * sizeof(SList Fp));
                tsiz_sl = si + 1;
            }
            if (!tsl[si])
            {   tsl[si] = (SList Fp) tcalloc(1, sizeof(SList));
                tsl[si]->g = (GList Fp Fp) tcalloc(gi + 1, sizeof(GList Fp));
                tsl[si]->a_num = gi + 1;
            }
            if (!tsl[si]->num_c)
            {   if (tsl[si]->num_g)
                    FEError(-115,NOEXIT,NOWRITE,
                    "Tierra VerifyGB() !tsl[si]->num_c but tsl[si]->num_g\n");
                cNumSizes++;
            }
            tsl[si]->num_c++;
            if (gi >= tsl[si]->a_num)
            {   tsl[si]->g = (GList Fp Fp) trecalloc(tsl[si]->g,
                    (gi + 1) * sizeof(GList Fp),
                    tsl[si]->a_num * sizeof(GList Fp));
                tsl[si]->a_num = gi + 1;
            }
            if ((I32u) tsl[si]->g[gi] < 4)
            {   tsl[si]->g[gi] = (GList Fp) tcalloc(1, sizeof(GList));
                cNumGenot++;
                tsl[si]->num_g++;
            }
            tsl[si]->g[gi]->pop++;
        }
    } /* check and free temporary genebank */
    for (si = 0; si < tsiz_sl; si++)
    {   if (tsl[si])
        {   if (tsl[si]->num_c != sl[si]->num_c)
                FEError(-116,NOEXIT,NOWRITE,
             "Tierra VerifyGB() tsl[%ld]->num_c != sl[%ld]->num_c\n", si, si);
            if (tsl[si]->num_g != sl[si]->num_g)
                FEError(-117,NOEXIT,NOWRITE,
             "Tierra VerifyGB() tsl[%ld]->num_g != sl[%ld]->num_g\n", si, si);
            if (tsl[si]->num_c && tsl[si]->g)
            {   cgNumSizes++;
                cgsNumCells += tsl[si]->num_c;
                cgsNumGenot += tsl[si]->num_g;
                for (gi = 0; gi < tsl[si]->a_num; gi++)
                {   if ((I32u) tsl[si]->g[gi] > 4)
                    {   if (tsl[si]->g[gi]->pop != sl[si]->g[gi]->pop)
                            FEError(-118,NOEXIT,NOWRITE,
          "Tierra VerifyGB() tsl[%ld]->g[%hd]->pop != sl[%ld]->g[%hd]->pop\n",
                                si, gi, si, gi);
                        cgNumGenot++;
                        cgNumCells += tsl[si]->g[gi]->pop;
                        tfree(tsl[si]->g[gi]);
                    }
                }
                tfree(tsl[si]->g);
                tfree(tsl[si]);
            }
        }
    }
    tfree(tsl);
    if (NumCells != cNumCells || NumCells != cgNumCells ||
        NumCells != cgsNumCells)
        FEError(-119,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumCells cells array inconsistency\n");
    if (NumGenotypes != cNumGenot || NumGenotypes != cgNumGenot ||
        NumGenotypes != cgsNumGenot)
        FEError(-120,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumGenot cells array inconsistency\n");
    if (NumSizes != cNumSizes || NumSizes != cgNumSizes)
        FEError(-121,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumSizes cells array inconsistency\n");
    /* end cells array check */

    /* begin genebank check */
    for (si = 0; si < siz_sl; si++)
    {   psl = sl[si];
        if (!psl)
            continue ;
        if (!psl->num_c || !psl->num_g)
            FEError(-122,NOEXIT,NOWRITE,
                "Tierra VerifyGB() !sl[si]->num_c or !sl[si]->num_g\n");
        if (sl[si]->num_c)
        {   gNumSizes++;
            ggNumCells += sl[si]->num_c;
        }
        if (sl[si]->num_g)
            ggNumGenot += sl[si]->num_g;
        for (gi = 0; gi < sl[si]->a_num; gi++)
        {   pgl = psl->g[gi];
            if ((I32u) pgl < 4 || !pgl->pop)
                continue ;
            gNumGenot++;
            gNumCells += pgl->pop;
        }
    }
    if (NumCells != gNumCells || NumCells != ggNumCells)
        FEError(-123,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumCells genebank inconsistency\n");
    if (NumGenotypes != gNumGenot || NumGenotypes != ggNumGenot)
        FEError(-124,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumGenot genebank inconsistency\n");
    if (NumSizes != gNumSizes)
        FEError(-125,NOEXIT,NOWRITE,
            "Tierra VerifyGB() NumSizes genebank inconsistency\n");
    /* end genebank check */
}

#endif /* ERROR */

void GarbageCollectGB()
{   I32s  i, j, maxsiz = 0, tail;
    GList  Fp Fp tgl, Fp pgl;
    SList  Fp Fp tsl;
    I8s     path[80];
    FILE    *fp;
    head_t  head;
    indx_t  *indx, gindx;

    for (i = siz_sl - 1; i >= 0; i--)      /* for each allocated size class */
    {   if (sl[i])
        {   if (sl[i]->num_c)
            {   if (!maxsiz)                     /* find largest size class */
                    maxsiz = i;
                tail = -1;
                for (j = sl[i]->a_num - 1; j >= 0; j--)
                {   if ((I32u) (pgl = sl[i]->g[j]) > 4 && !pgl->pop
                        && !IsBit(pgl->bits, 0))
                    {   gq_rem(pgl);
                        if (pgl->genome)
                        {   tfree(pgl->genome);
                            pgl->genome = NULL;
                        }
                        if (pgl->gbits)
                        {   tfree(pgl->gbits);
                            pgl->gbits = NULL;
                        }
                        tfree(sl[i]->g[j]);
                        sl[i]->g[j] = NULL;
                    }
                    if (tail < 0 && sl[i]->g[j])
                        tail = j;    /* skip empty geotypes at end of array */
                }
                if (tail < sl[i]->a_num - 1)
                {   if (tail < 0)             /* no genotypes in size class */
                    {   if (sl[i]->g)
                        {   tfree(sl[i]->g);
                            sl[i]->g = NULL;
                        }
                        if (sl[i])
                        {   tfree(sl[i]);
                            sl[i] = NULL;
                        }
                    }
                    else           /* shorten g arrays to avoid empty tails */
                    {   tgl = (GList Fp Fp) trecalloc(sl[i]->g,
                            (tail + 1) * sizeof(GList Fp),
                            sl[i]->a_num * sizeof(GList Fp));
                        if (tgl)
                            sl[i]->g = tgl;
                        else if (sl[i]->g)
                        {   tfree(sl[i]->g);
                            sl[i]->g = NULL;
                            FEError(-126,EXIT,WRITE,
                      "Tierra GarbageCollectGB() sl[i]->g trecalloc error\n");
                        }
                        sl[i]->a_num = tail + 1;
                    }
                }
            }
            else /* no creatures of this size, free sl[i] and sl[i]->g */
            {   sprintf(path, "%s%04ld.gen", GenebankPath, i);
                fp = open_ar(path, i, GFormat, -1);
                head = read_head(fp);
#ifdef __TURBOC__
                indx = &gindx;
#else  /* __TURBOC__ */
                indx = read_indx(fp, &head);
#endif /* __TURBOC__ */

                for (j = sl[i]->a_num - 1; j >= 0; j--)
                    if ((I32u) (pgl = sl[i]->g[j]) > 4)
                    {   if (pgl->pop)
                            FEError(-127,NOEXIT,NOWRITE,
 "Tierra GarbageCollectGB(), pgl = %ld, pgl->pop not zero, can't free\n",
     (I32s) pgl);
                        if (IsBit(pgl->bits, 0)) /* save genome to disk */
                            add_gen(fp, &head, &indx, pgl);
                        if (pgl->genome)
                        {   tfree(pgl->genome);
                            pgl->genome = NULL;
                        }
                        if (pgl->gbits)
                        {   tfree(pgl->gbits);
                            pgl->gbits = NULL;
                        }
                        gq_rem(pgl);
                        tfree(sl[i]->g[j]);
                        sl[i]->g[j] = NULL;
                    }
                fclose(fp);
                if (!head.n)
                    unlink(path);
#ifndef __TURBOC__
                if (indx)
                {   thfree(indx);
                    indx = NULL;
                }
#endif /* __TURBOC__ */
                if (sl[i]->g)
                {   tfree(sl[i]->g);
                    sl[i]->g = NULL;
                }
                if (sl[i])
                {   tfree(sl[i]);
                    sl[i] = NULL;
                }
            }
        }
    }
    if (maxsiz < siz_sl - 1)
    {   tsl = (SList Fp Fp) trecalloc(sl, (maxsiz + 1) * sizeof(SList Fp),
            siz_sl * sizeof(SList Fp));
        if (tsl)
            sl = tsl;
        else if (sl)
        {   tfree(sl);
            sl = NULL;
   FEError(-128,EXIT,WRITE, "Tierra GarbageCollectGB() sl trecalloc error\n");
        }
        siz_sl = maxsiz + 1;
    } /* end garbage collect for genebank */
}

#ifdef CM5

struct MaxGen FindMaxGen()
{   I32s  i, j, pop, mem, MaxPop = 0, MaxMem = 0;
    Genotype  MaxGenPop, MaxGenMem;

    for (i = siz_sl - 1; i >= 0; i--)      /* for each allocated size class */
    {   if (sl[i] && sl[i]->num_c)
        {   for (j = sl[i]->a_num - 1; j >= 0; j--)
            {   if ((I32u) (pgl = sl[i]->g[j]) > 4 && pop = pgl->pop)
                {   mem = pop * ce->d.gen.size;
                    if (pop > MaxPop)
                    {   MaxPop = pop;
                        MaxGenPop.size = i;
                        strcpy(MaxGenPop.gen, Int2Lbl(j));
                    }
                    if (mem > MaxMem)
                    {   MaxMem = mem;
                        MaxGenMem.size = i;
                        strcpy(MaxGenMem.gen, Int2Lbl(j));
                    }
                }
            }
        }
    }
}

#endif /* CM5 */

void GenExTemp(adrt, ce, tsize)
    I32s     adrt;  /* address of beginning of template */
    Pcells  ce;    /* ce = cell executing instruction */
    I32s    tsize; /* template size */
{
    I32s  i;
    I32u  who;  /* 0 same cell; 1 daughter cell; 2 other cell; */
                /* 3 free memory; 4 daughter of other cell */
    I32s   dist;
    Pgl   tgl, ogl;
    Pcells  ct;

    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    for (i = 0; i < tsize; i++)
    {   ct = ce;  /* WHAT TO DO WITH THIS? */
        who = WhoIs(&ct, ad(ce->c.ip + 1 + i)); /* who has template pattern */
        if (who < 4) tgl->bits |= (I32u) (ONE << (I32u) (12 + who));
        else tgl->bits |= (I32u) (ONE << (I32u) (12 + 2));
        if (!who)
        {   dist = ad(ce->c.ip + 1 + i) - ce->mm.p;
            dist = ad(dist);
#ifdef ERROR
            if (tgl->genome == NULL || dist < 0 || dist >= tgl->gen.size)
                FEError(-131,EXIT,WRITE, "Tierra GenExTemp() error 0\n");
#endif /* ERROR */
#if PLOIDY == 1
            tgl->gbits[dist] |= 1;
#else /* PLOIDY == 1 */
            tgl->gbits[dist][ce->d.tr] |= 1;
#endif /* PLOIDY == 1 */
        }
        if (who == 2)
        {   ogl = sl[ct->d.gen.size]->g[ct->d.gi];
            if (IsBit(ogl->bits, 0))
            {   ogl->bits |= (I32u) (ONE << (I32u) (12 + 4));
                dist = ad(ce->c.ip + 1 + i) - ct->mm.p;
                dist = ad(dist);
#ifdef ERROR
                if (ogl->genome == NULL || dist < 0 || dist >= ogl->gen.size)
                   FEError(-132,EXIT,NOWRITE, "Tierra GenExTemp() error 1\n");
#endif /* ERROR */
#if PLOIDY == 1
                ogl->gbits[dist] |= (1 << 1);
#else /* PLOIDY == 1 */
                ogl->gbits[dist][ce->d.tr] |= (1 << 1);
#endif /* PLOIDY == 1 */
            }
        }
        ct = ce;
        who = WhoIs(&ct, ad(adrt + i)); /* who has complementary template */
        if (who < 4) tgl->bits |= (I32u) (ONE << (I32u) (7 + who));
        else tgl->bits |= (I32u) (ONE << (I32u) (7 + 2));
        if (!who)
        {   dist = ad(adrt + i) - ce->mm.p;
            dist = ad(dist);
#ifdef ERROR
            if (tgl->genome == NULL || dist < 0 || dist >= tgl->gen.size)
                FEError(-133,EXIT,WRITE, "Tierra GenExTemp() error 2\n");
#endif /* ERROR */
#if PLOIDY == 1
            tgl->gbits[dist] |= 1;
#else /* PLOIDY == 1 */
            tgl->gbits[dist][ce->d.tr] |= 1;
#endif /* PLOIDY == 1 */
        }
        if (who == 2)
        {   ogl = sl[ct->d.gen.size]->g[ct->d.gi];
            if (IsBit(ogl->bits, 0))
            {   ogl->bits |= (I32u) (ONE << (I32u) (7 + 4));
                dist = ad(adrt + i) - ct->mm.p;
                dist = ad(dist);
#ifdef ERROR
                if (ogl->genome == NULL || dist < 0 || dist >= ogl->gen.size)
                    FEError(-134,EXIT,WRITE, "Tierra GenExTemp() error 3\n");
#endif /* ERROR */
#if PLOIDY == 1
                ogl->gbits[dist]|= (1 << 1);
#else /* PLOIDY == 1 */
                ogl->gbits[dist][ce->d.tr] |= (1 << 1);
#endif /* PLOIDY == 1 */
            }
        }
    }
}

void GenExMov(ce, to, from)
    Pcells  ce;
    I32s    to, from;
{
    Pcells  ct;
    Pgl     tgl, ogl;
    I32u    who;  /* 0 same cell; 1 daughter cell; 2 other cell; */
                  /* 3 free memory; 4 daughter of other cell */

    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    if (ce->d.flaw || ce->d.mut || !IsBit(tgl->bits, 0))
        return;
    /* the mov instruction being executed is within your own genome */
    if (ce->mm.p <= ce->c.ip && ce->c.ip < (ce->mm.p + ce->mm.s))
    {   ct = ce;
        who = WhoIs(&ct, from);    /* who is it moved from */
        if (who < 4) tgl->bits |= (I32u) (ONE << (I32u) (17 + who));
        else tgl->bits |= (I32u) (ONE << (I32u) (17 + 2));
        if (who == 2)
        {   ogl = sl[ct->d.gen.size]->g[ct->d.gi];
            if (IsBit(ogl->bits, 0))
            ogl->bits |= (I32u) (ONE << (I32u) (17 + 4));
        }
        ct = ce;
        who = WhoIs(&ct, to); /* who is it moved to */
        if (who < 4)
            tgl->bits |= (I32u) (ONE << (I32u) (22 + who));
        else tgl->bits |= (I32u) (ONE << (I32u) (22 + 2));
        if (who == 2)
        {   ogl = sl[ct->d.gen.size]->g[ct->d.gi];
            if (IsBit(ogl->bits, 0))
            ogl->bits |= (I32u) (ONE << (I32u) (22 + 4));
        }
    }
    else   /* these are moved from while executing instructions that */
    {   ct = ce;       /* are not your own */
        who = WhoIs(&ct, from);    /* who is it moved from */
        if (who < 4)
            tgl->bits |= (I32u) (ONE << (I32u) (27 + who));
        else tgl->bits |= (I32u) (ONE << (I32u) (27 + 2));
        if (who == 2)   /* ct is cell from which inst is moved */
        {   ogl = sl[ct->d.gen.size]->g[ct->d.gi];
            if (IsBit(ogl->bits, 0))
                ogl->bits |= (I32u) (ONE << (I32u) (27 + 4));
        }
    }
}

void GenExExe(ce, adrt)
    Pcells  ce;
    I32s     adrt;
{
    Pcells  ct = ce;
    Pgl tgl;
    I32u    dist;
    I32u    who;  /* 0 same cell; 1 daughter cell; 2 other cell; */
                  /* 3 free memory; 4 daughter of other cell */

    tgl = sl[ce->d.gen.size]->g[ce->d.gi];
    if (ce->d.flaw || ce->d.mut || !IsBit(tgl->bits, 0))
        return;
    who = WhoIs(&ct, adrt);
    if (who < 4)
        tgl->bits |= (I32u) (ONE << (I32u) (2 + who));
    else tgl->bits |= (I32u) (ONE << (I32u) (2 + 2));
    if (!who)  /* who == 0 == same cell */
    {   dist = adrt - ce->mm.p;
#ifdef ERROR
        if (tgl->gbits == NULL || dist < 0 || dist >= tgl->gen.size)
            FEError(-135,EXIT,WRITE, "Tierra GenExExe() error 0\n");
#endif /* ERROR */
#if PLOIDY == 1
        tgl->gbits[dist]|= 1;
#else /* PLOIDY == 1 */
        tgl->gbits[dist][ce->d.tr] |= 1;
#endif /* PLOIDY == 1 */
    }
    if (who == 2)  /* is other cell */
    {   tgl = sl[ct->d.gen.size]->g[ct->d.gi];
        if (IsBit(tgl->bits, 0))
        {   tgl->bits |= (ONE << (I32u) (2 + 4));
            dist = adrt - ct->mm.p;
#ifdef ERROR
            if (tgl->gbits == NULL || dist < 0 || dist >= tgl->gen.size)
                FEError(-136,EXIT,WRITE, "Tierra GenExExe() error 1\n");
#endif /* ERROR */
#if PLOIDY == 1
            tgl->gbits[dist]|= (1 << 1);
#else /* PLOIDY == 1 */
            tgl->gbits[dist][ce->d.tr] |= (1 << 1);
#endif /* PLOIDY == 1 */
        }
    }
}

/* rationale for the functioning of the genebank:

The term ``rambank'' refers to a collection of genotypes maintained in RAM
The term ``diskbank'' refers to a collection of genotypes maintained on disk
The term ``genebank'' refers to both the rambank and the diskbank

Genotype names have two parts: size-label, for example 0080aaa, 0045eat,
6666god.

1) When a creature is born its genotype will be compared to that of its parent.
   A) if they are the same, the daughter will be given the same name as the
      mother.
   B) if they are not the same, the genebank will be searched.
      a) if the daughter genotype is found in the genebank, it will be given
         the same name as the genotype that it matches in the bank.
      b) if the daughter genotype does not match any genotype in the bank,
         a new name will be created for it, and it will be entered into the
         rambank.
2) For each birth and death a count of the population of both the genotype
   and the size class involved will be incremente or decremented, so that we
   have a count of the current population of each genotype and each size class.
   This information is maintained in rambank.
3) If a genotype frequency crosses a critical threshold, the genotype name
   will become permanent and the genotype will be saved to the diskbank.
   There may be several types of thresholds: proportion of population
   (e.g., 2%), proportion of soup, or just numbers of creatures.
4) When a genotype frequency drops to zero:
   A) If the genotype never crossed the thresholds, the genotype will be
      removed from the genebank, and its unique name will become available for
      reuse.
   B) If the genotype crossed the threshold, gaining a permanent name, it
      should be retained in the genebank.
5) Periodically, Tierra saves the complete state of the machine (e.g., every
   100 million instructions executed).  At that time, the complete rambank
   is saved to the diskbank.  For this reason, 4 A applies also to genotypes
   which never became permanent, and these must be removed from the diskbank
   as well.  The bitfield in the genotype structure tells us if a genotype is
   saved to the diskbank, and if it is permanent.
6) If the rambank becomes ``too full'', some relatively unused portion of it
   should be swapped to the diskbank.  In DOS, ``too full'' could be signaled
   by a malloc failure.  In unix, ``too full'' could be signaled by some
   specified limit on how big the rambank should get, if this seems wise.
   That portion of the rambank to be swapped to the diskbank might consist of
   the least recently accessed size class.  For this reason it might be
   worthwhile to maintain a queue of size classes, ordered by last use.
*/
