/* bookeep.c   9-9-92  bookeeping functions for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)bookeep.c	1.5 7/21/92";
#endif /* lint */

#include "license.h"
#include "tierra.h"
#include "extern.h"


#ifdef MEM_CHK
#include <memcheck.h>
#endif /* MEM_CHK */

void DivideBookeep(ce, nc)
    Pcells   ce, nc; /* ce = mother cell, nc = daughter cell */
{   I8s      same = 0;
    Event    SizGen;
    GlIndex  GiHash;

    LastDiv = InstExe;
    if (!ce->d.fecundity && !ce->d.mut && !ce->d.flaw)
    {   ce->d.d1.flags = ce->d.flags;    /* record metabolic data 1st repl */
        ce->d.d1.inst = ce->d.inst + 1;
        ce->d.d1.mov_daught = ce->d.mov_daught;
    }
    ce->d.fecundity++;
    nc->d.gen.size = nc->mm.s;
    if (GeneBnker)
    {   nc->d.genome = soup + nc->mm.p;
        if (ce->mm.s == nc->mm.s &&    /* if cell breeds true */
            IsSameGen(nc->mm.s, soup + nc->mm.p, soup + ce->mm.p))
        {   if (ce->d.fecundity == 1)
                nc->d.d1.BreedTrue = ce->d.d1.BreedTrue = 1;
            nc->d.parent = ce->d.parent;
            nc->d.gen = ce->d.gen;
            nc->d.gi = ce->d.gi;
            nc->d.hash = ce->d.hash;
            same = 1;
        }
        else     /* if daughter is a new genotype (same = 0) */
        {   nc->d.parent = ce->d.gen; /* this will assign a gen.label */
            GiHash = CheckGenotype(nc->d, 17); /* by checking .gen files */
            nc->d.hash = GiHash.si;
            nc->d.gi = GiHash.gi;
            strcpy(nc->d.gen.label, Int2Lbl(GiHash.gi));
        }

        SizGen = DivGenBook(ce, nc, InstExe, reaped, 1, same, 0);
        NumGenotypes += SizGen.i;
        NumSizes += SizGen.m;

    }
    ce->d.mov_daught = ce->d.mut = 0;
    OutDisk((I32s)'b', nc);
#if FRONTEND != STDIO
    FEStats(); 
#endif /* FRONTEND != STDIO */
}

void ReapBookeep(ce)
    Pcells  ce;
{   Event   SizGen;

    OutDisk((I32s)'d', ce);
    if (GeneBnker)
    {   SizGen = ReapGenBook(ce);
        NumGenotypes += SizGen.i;
        NumSizes += SizGen.m;
    }
#ifdef MICRO
    if (ce == MicroSlice) MicroSlice = NULL;
#endif /*MICRO */
    InitCell(ce->q.this.a,ce->q.this.i,ce);
    NumCells--;
    reaped = 1;
}

void MutBookeep(i)
    I32s i;
{
    I8s    md;
    Pcells ce;
    Event   SizGen;
    GlIndex  GiHash;

    if (!GeneBnker || IsFree(i)) return;
    WhichCell(i, &ce, &md);
    if (md == 'd')
        return;
    SizGen = ReapGenBook(ce);
    NumGenotypes += SizGen.i;

    OutDisk((I32s)'d', ce);
    ce->d.parent = ce->d.gen;    /* assign new genotype */
    ce->d.gi = -1;
    strcpy(ce->d.gen.label, "---");

    GiHash = CheckGenotype(ce->d, 17); /* by checking .gen files */
    ce->d.hash = GiHash.si;
    ce->d.gi = GiHash.gi;
    strcpy(ce->d.gen.label, Int2Lbl(GiHash.gi));

    SizGen = DivGenBook(NULL, ce, InstExe, reaped, 0, 0, 0);
/*  SizGen = DivGenBook(NULL, ce, 1, reaped, InstExe, 0, 0); */
    NumGenotypes += SizGen.i;

    OutDisk((I32s)'b', ce);
    ce->d.d1.flags = ce->d.d1.mov_daught = 0L;
    ce->d.fecundity = ce->d.flags = 0L;
    ce->d.d1.inst = ce->d.inst = 0L;
    ce->d.mut++;
}

void OutDisk(bd, nc)
    I32s bd;
    Pcells nc;
{   I32s ttime;
    I8s label[4];

    if (DiskOut)
    {   if (FirstOutDisk)
        {   FirstOutDisk = 0;
            BrkupCum = 0;
            BrkupCou = 1;
#ifdef IBM3090
            if (BrkupSiz)
                sprintf(Buff, "break.1.d");
            else sprintf(Buff, "tierra.run.d");
            oufr = fopen(Buff, "w");
#else /* IBM3090 */
            if (BrkupSiz)
                sprintf(Buff, "%sbreak.1", OutPath);
            else sprintf(Buff, "%stierra.run", OutPath);
            oufr = fopen(Buff, "w");
#endif /* IBM3090 */
            if (oufr == NULL)
            {   FEError(-108,EXIT,NOWRITE, 
                   "Tierra OutDisk() 1: file %s not opened, exiting\n", Buff);
            }
            sprintf(label, nc->d.gen.label);
#ifdef IBM3090
            Ascii2Ebcdic(label);
#endif /* IBM3090 */
            BrkupCum += fprintf(oufr, "%lx %c %ld", InstExe.i, (I8s) bd,
                nc->d.gen.size);
            if (GeneBnker)
                BrkupCum += 1 + fprintf(oufr, " %s\n", label);
            else BrkupCum += 1 + fprintf(oufr, "\n");
        }
        else
        {   ttime = InstExe.i - lo.time;
            if (ttime < 0)
                ttime += 1000000L;
            BrkupCum += fprintf(oufr, "%lx", ttime);
            if (lo.bd != bd)
                BrkupCum += fprintf(oufr, " %c", bd);
            if (lo.size != nc->d.gen.size)
                BrkupCum += fprintf(oufr, " %ld", nc->d.gen.size);
            if (GeneBnker && strcmp(lo.label, nc->d.gen.label))
            {   sprintf(label, nc->d.gen.label);
#ifdef IBM3090
                Ascii2Ebcdic(label);
#endif /* IBM3090 */
                BrkupCum += fprintf(oufr, " %s", label);
            }
            BrkupCum += 1 + fprintf(oufr, "\n");
            if (BrkupSiz && BrkupCum > BrkupSiz * 1024L)
            {   fclose(oufr);
                BrkupCum = 0;
                BrkupCou++;
#ifdef IBM3090
                sprintf(Buff, "break.%ld.d", BrkupCou);
                oufr = fopen(Buff, "w");
#else /* IBM3090 */
                sprintf(Buff, "%sbreak.%ld", OutPath, BrkupCou);
                oufr = fopen(Buff, "w");
#endif /* IBM3090 */
                if (oufr == NULL)
                {   FEError(-109,EXIT,WRITE,
                   "Tierra OutDisk() 2: file %s not opened, exiting\n", Buff);
                }
            }
        }
    }
    else
    {   if (FirstOutDisk) FirstOutDisk = 0;
        else
        {   ttime = InstExe.i - lo.time;
            if (ttime < 0) ttime += 1000000L;
        }
    }
    lo.bd = bd;
    lo.size = nc->d.gen.size;
    lo.time = InstExe.i;
    strcpy(lo.label, nc->d.gen.label);
    TimePop += (double) ttime *(double) NumCells;
    if ((I8s) bd == 'b')
        TimeBirth++;
    else TimeDeath++;
}

void plan()
{   I32s i, j, n = 0, indiv_gen_time, pop_gen_time;
    I32s MaxPop = 0, MaxMem = 0, pop = 0, mem = 0, ar, ci;
    Genotype MaxGenPop, MaxGenMem;
    double prob_of_hit;
    Pcells ce;
    I8s  *chk;
    Pcells Fp  tcells;
#ifdef MEM_PROF
    I32s  SizSoup, SizCells, SizFreeMem, SizSl, SizSli = 0;
    I32s  SizGl = 0, SizGli = 0, SizGen = 0;
#endif /* MEM_PROF */

#ifndef CM5
    if (GeneBnker && reaped)
    {
        GarbageCollectGB();
#ifdef ERROR
        VerifyGB();
#endif /* ERROR */
    }
#endif /* CM5 */

    /* begin calculate averages */
    AverageSize = 0;
    chk = tcalloc(NumCelAr, sizeof(I8s));
    for (ar = 0; ar < NumCelAr; ar++) for (ci = 0; ci < CelArSiz; ci++)
    {   if (ar == 0 && ci < 2)
            continue;
        ce = &cells[ar][ci];
        if (ce->ld)
        {   n++; chk[ar] = 1;
            AverageSize += ce->d.gen.size;
#ifndef CM5
            if (GeneBnker && InstExe.m)
            {   pop = sl[ce->d.gen.size]->g[ce->d.gi]->pop;
                mem = pop * ce->d.gen.size;
                if (pop > MaxPop)
                {   MaxPop = pop;
                    MaxGenPop = ce->d.gen;
                }
                if (mem > MaxMem)
                {   MaxMem = mem;
                    MaxGenMem = ce->d.gen;
                }
            }
#endif /* CM5 */
        }
    }
#ifdef CM5
    if (GeneBnker && InstExe.m)
        FindMaxGen();
#endif /* CM5 */
/* end calculate averages */

    /* begin garbage collect for cells array */
    if (reaped)
        for(ar = NumCelAr - 1; ar > 0; ar--)
        {   if (chk[ar])
                break;
            if (cells[ar])
            {   tfree(cells[ar]);
                cells[ar] = NULL;
            }
            NumCelAr--;
            tcells = (Pcells Fp) trecalloc((Pcells Fp) cells,
                (I32u) NumCelAr * sizeof(Pcells Fp),
                (I32u) (NumCelAr + 1) * sizeof(Pcells Fp));
            if (tcells)
                cells = tcells;
            else if (cells)
            {   tfree(cells);
                cells = NULL;
             FEError(-129,EXIT,WRITE,"Tierra plan() cells trecalloc error\n");
            }
            CellsSize = NumCelAr * CelArSiz;
        } /* end garbage collect for cells array */

    if (chk)
    {   tfree(chk);
        chk = NULL;
    }

#ifdef MEM_PROF /* calculate memory profile */

    TotMemUse = SizSoup = SoupSize * sizeof(Instruction);
    TotMemUse += SizCells = CellsSize * sizeof(struct cell);
    TotMemUse += SizFreeMem = MaxFreeBlocks * sizeof(MemFr);
    if(GeneBnker)
    {   TotMemUse += SizSl = siz_sl * sizeof(SList Fp);
        for (i = 0; i < siz_sl; i++)
        {   if (sl[i])
            {   TotMemUse += sizeof(SList);
                SizSli += sizeof(SList);
                TotMemUse += sl[i]->a_num * sizeof(GList Fp);
                SizGl += sl[i]->a_num * sizeof(GList Fp);
                for (j = 0; j < sl[i]->a_num; j++)
                {   if ((I32s) sl[i]->g[j] > 4)
                    {   TotMemUse += sizeof(GList);
                        SizGli += sizeof(GList);
                        if (sl[i]->g[j]->genome)
                        {   TotMemUse += i * sizeof(Instruction);
                            SizGen += i * sizeof(Instruction);
                        }
                        if (sl[i]->g[j]->gbits)
                        {   TotMemUse += i * sizeof(GenBits);
                            SizGen += i * sizeof(GenBits);
                        }
                    }
                }
            }
        }
    }

#endif /* MEM_PROF */

    /* begin calculate averages */
    if (n != NumCells)
    {   FEError(-130,EXIT,NOWRITE,
         "Tierra plan() NumCells = %ld  count of cells = %ld\n", NumCells, n);
    }
    AverageSize /= n;
    if (GenPerMovMut)
        RateMovMut = (I32s) 2L *GenPerMovMut * AverageSize;
    indiv_gen_time = 10L * AverageSize;
    if (InstExe.m)
        pop_gen_time = NumCells * indiv_gen_time;
    else pop_gen_time = indiv_gen_time * (SoupSize / (4L * AverageSize));
    prob_of_hit = (double) AverageSize / (double) SoupSize;
    if (GenPerBkgMut)
        RateMut = (I32s) (pop_gen_time * 2L * GenPerBkgMut * prob_of_hit);
    if (GenPerFlaw)
        RateFlaw = (I32s) indiv_gen_time *GenPerFlaw * 2L;
    if (DropDead) DropDead = 1L + AverageSize / 80L;  /* DAN */
    Search_limit = (I32s) (SearchLimit * AverageSize);
    Put_limit = (I32s) (PutLimit * AverageSize);
    MalLimit = MalTol * AverageSize;
    if (MalLimit >= SoupSize)
        MalLimit = SoupSize - 1;
    if (InstExe.m)
    {   TimePop /= 1000000.;
        Generations += (double) (TimeBirth + TimeDeath) / (2. * TimePop);
    }
    /* end calculate averages */

    FEPlan(MaxPop, MaxMem, &MaxGenPop, &MaxGenMem);

#ifdef MEM_PROF

    FEMemProf(SizSoup, SizCells, SizFreeMem, SizSl, SizSli,
        SizGl, SizGli, SizGen);

#endif /* MEM_PROF */

    TimePop = 0.;
    TimeBirth = TimeDeath = 0L;
}

I16s Lbl2Int(s)
    I8s *s;
{
    if (s[0] == '-')
    return -1;
    return (s[2] - 'a') + (26 * (s[1] - 'a')) + (676 * (s[0] - 'a'));
}

I8s *Int2Lbl(i)
    I32s i;
{
    static I8s s[4];

    if (i < 0) {
    strcpy(s, "---");
    return s;
    }
    s[0] = 'a' + (I16s) i / 676;
    i %= 676;
    s[1] = 'a' + (I16s) i / 26;
    i %= 26;
    s[2] = 'a' + (I16s) i;
    s[3] = 0;
    return s;
}

I32u WhoIs(ce, a)
    Pcells  Fp ce;
    I32s a;
{
    I8s md;

    if (a >= (*ce)->mm.p && a < (*ce)->mm.p + (*ce)->mm.s)
        return 0;          /* same cell */
    if (a >= (*ce)->md.p && a < (*ce)->md.p + (*ce)->md.s)
        return 1;          /* daughter cell */
    if (IsFree(a))
        return 3;          /* is free memory */
    WhichCell(a, ce, &md);
    if (md == 'm')
        return 2;          /* is other cell */
    return 4;              /* is the daughter of another cell */
}

I8s IsSameGen(size, g1, g2)/* compare two genomes */
    I32s size;
    FpInst g1, g2;
{
    I32s i, j;

    for (i = 0; i < size; i++) 
#if PLOIDY == 1
        if ((g1 + i)->inst != (g2 + i)->inst)
#else /* PLOIDY > 1 */
    for (j = 0; j < PLOIDY; j++)
        if ((g1 + i)[j]->inst != (g2 + i)[j]->inst)
#endif /* PLOIDY > 1 */
            return 0;
    return 1;
}
