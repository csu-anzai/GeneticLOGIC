
/* diverse.c  15-8-92  code for computing the diversity index */
/*** Diverse: Version 4.0  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

/* Thresholding decisions:

1) When the threshold is set at one cell, the behavior is the same
   as if thresholds were not used.
2) Size classes are counted as above thereshold only when one of their
   genotypes is above threshold.
3) The age of a class is based on when it first appeared, not when it
   crossed the threshold.
4) When a class crosses a threshold, its entire population is counted,
   not just the above threshold part of the population.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#ifdef __TURBOC__
#include <alloc.h>
#endif /* __TURBOC__ */

#ifdef __STDC__           /* ANSI prototyping */
#define P_(A) A
#define const const
#else                 /* non-ANSI prototyping */
#define P_(A) ()
#define const
#endif

typedef unsigned int   Uint;
typedef unsigned long  Ulong;
typedef long int       I32s;

struct siz {
    long   Time;
    long   NumCell;
    long   NumSize;
    float  SizeDiv;
    long   AgeSize;
    } ;

struct gen {
    long   NumGeno;
    float  GenoDiv;
    long   AgeGeno;
    } ;

struct cum {
    double  Time;
    double  NumCell;
    double  NumSize;
    double  SizeDiv;
    double  AgeSize;
    double  NumGeno;
    double  GenoDiv;
    double  AgeGeno;
    } cu, icu;

struct last_out {
    Ulong  time;   /* += itime % 1000000 */
    Ulong  ctime;  /* count of millions of cpu cycles */
    Ulong  itime;  /* time interval since last birth or death */
    char   bd;
    Uint   size;
    char   label[4];
    } lo;

struct gene_dat {
    struct tnode  *t;
    long          index;
    } ;

struct pop_dat {
    long  pop;  /* current population of this size or genotype */
    long  popt; /* current threshold population of this size or genotype */
    long  age;  /* current age of this size or genotype */
    } ;

struct tnode {
    long            size;  /* genome size */
    int             ntg;   /* number of threshold genotypes */
    struct pop_dat  sd;    /* pop_dat for this size class */
    int             gsize; /* allocated size of *g array */
    struct pop_dat  *g;    /* array of pop_dat structures */
    struct tnode    *l;    /* left sub-tree */
    struct tnode    *r;    /* right sub-tree */
    } ;

struct Totals {
    I32s    TotPopTh;
    I32s    TotPopTo;
    I32s    TotSizes;
    I32s    TotGenos;
    double  time;
    } to;

struct DivDat {
    float   SizeDiv;
    float   GenoDiv;
    I32s    NumSize;
    I32s    NumGeno;
    I32s    NumCellSTh;
    I32s    NumCellSTo;
    I32s    NumCellGTh;
    I32s    NumCellGTo;
    double  tot_size_age;
    double  tot_geno_age;
    } dd;

struct range {
    double  n;  /* minimum value */
    double  x;  /* maximum value */
    I32s    m;  /* count of values */
    int     f;  /* first time through = 1 */
    } timer, sdivr, gdivr, nsr, ngr, ncr, asar, agar;

void main P_((int  argc, char  **argv));
void doranges P_(());
void doaverages P_(());
void breakrange P_(());
struct tnode * AddTree P_((struct tnode  *p));
void divCountTree P_((struct tnode  *t));
void minmax2 P_((double  v, struct range  *r));
int Lbl2Int P_((char  s[]));
int t_read P_((char  data[], struct last_out  *lo, int  *first,
     int  *genotypes));

int     genotypes, thr;
double  AgeSize, AgeGeno;
char    infile[13], ofile[13], bifile[13], bofile[13], data[81];
FILE    *inf, *ouf, *our, *iouf;

void main (argc, argv)
int  argc;
char  **argv;
{   unsigned long  size, fsize = 0;
    I32s           mtime = 0, otime = 0, iamtime = 0, iaotime = 0, AvgFreq;
    int            binum = 1, bonum = 1;
    int            first = 1, first2 = 1, first3 = 1, format = 0, bdrecs = 0;
    char           c, buf[80];
    struct tnode   *troot = NULL;
    struct gen     ge;
    struct siz     si;
    double         tmp;
/* for debugging:
    Ulong          dtime = 0, a = 0, b = 1;
 */

    timer.n = timer.x = sdivr.n = sdivr.x = gdivr.n = gdivr.x = 0.;
    nsr.n = nsr.x = ngr.n = ngr.x = ncr.n = ncr.x = 0.;
    asar.n = asar.x = agar.n = agar.x = 0.;
    timer.f = sdivr.f = gdivr.f = nsr.f = ngr.f = ncr.f = asar.f = agar.f = 1;
    timer.m = sdivr.m = gdivr.m = nsr.m = ngr.m = ncr.m = asar.m = agar.m = 0;
    to.TotPopTh = to.TotPopTo = to.TotSizes = to.TotGenos = 0;
    to.time = 0.;

    printf ("diverse tool: to accept default values, hit Enter\n\n");

    sprintf(infile,"break.1");
    printf("input file (default: %s) = ", infile);
    gets(buf);
    if (strlen(buf))
        sprintf(infile, "%s", buf);

    inf = fopen(infile,"r");
    if(inf == NULL)
    {   printf("file %s not found\n", infile);
        return ;
    }
    fclose(inf);

    thr = 2;
    printf("threshold (default: %d) = ", thr);
    gets(buf);
    if (strlen(buf))
    {   sscanf(buf,"%d", &thr);
    }

    AvgFreq = 1000000L;
    printf("Frequency for Average Output (default: %ld) = ", AvgFreq);
    gets(buf);
    if (strlen(buf))
    {   sscanf(buf,"%ld", &AvgFreq);
    }

    bdrecs = 0;
    if (bdrecs)
        sprintf(buf,"y");
    else
        sprintf(buf,"n");
    printf("output birth death records: y or n (default: %s) = ", buf);
    gets(buf);
    if (strlen(buf))
    {   if(!strcmp(buf,"n"))
            bdrecs = 0;
        else bdrecs = 1;
    }
    if (bdrecs)
    {   sprintf(buf,"divdat.1");
        printf("output file (default: %s) = ", buf);
        gets(buf);
        if (strlen(buf))
            sscanf(buf,"%[^.]", ofile);
        else
            sprintf(ofile,"divdat");
        strcpy(bofile,ofile);
        strcat(ofile,".1");

        sprintf(buf,"binary"); format = 0;
        printf("output format: binary or ascii (default: %s) = ", buf);
        gets(buf);
        if (strlen(buf))
        {   if(!strcmp(buf,"binary"))
                format = 0;
            else format = 1;
        }

        size = 4096;
        printf("break size (default: %lu) = ", size);
        gets(buf);
        if (strlen(buf))
        {   sscanf(buf,"%lu", &size);
        }
        size *= 1024;

        our = fopen("brkrange","w");
    }

    sscanf(infile,"%[^.].%d", bifile, &binum);
    inf = fopen(infile,"r");
    if(inf == NULL)
    {   printf("\n Input File %s NOT found. \n", infile);
        exit(1);
    }
    lo.time = lo.ctime = lo.size = 0;
    cu.Time    = 0.; cu.NumCell = 0.; cu.NumSize = 0.; cu.SizeDiv = 0.;
    cu.AgeSize = 0.; cu.NumGeno = 0.; cu.GenoDiv = 0.; cu.AgeGeno = 0.;
    icu.Time    = 0.; icu.NumCell = 0.; icu.NumSize = 0.; icu.SizeDiv = 0.;
    icu.AgeSize = 0.; icu.NumGeno = 0.; icu.GenoDiv = 0.; icu.AgeGeno = 0.;
    for(;;)
    {   if(fgets(data,80,inf) == NULL)
        {   binum++;
            sprintf(infile,"%s.%d", bifile, binum);
            fclose(inf);
            inf = fopen(infile,"r");
            if(inf == NULL)
                break ;
            if(fgets(data,80,inf) == NULL)
                break ;
        }
        t_read(data, &lo, &first, &genotypes);

/* begin: for debugging purposes */
/*      if (to.time > dtime)
            a = b;
 */
/* end: for debugging purposes */

        if(first2)
        {   first2 = 0;
#ifdef __TURBOC__
            printf("\nTime = %4ld million  Coreleft = %6lu\r",
                0L, coreleft());
#else  /* __TURBOC__ */
            printf("\nTime = %4d million\r", first2);
#endif /* __TURBOC__ */
            fflush(stdout);
            if (bdrecs)
            {   if(format)
                {   ouf = fopen(ofile,"w");
                    if(genotypes)
                        fprintf(ouf,"11 %.1lf\n", to.time);
                    else
                        fprintf(ouf,"10 %.1lf\n", to.time);
                }
                else
                {   ouf = fopen(ofile,"wb");
                    c = (char) format;
                    fwrite(&c,sizeof(char),1,ouf);
                    c = (char) genotypes;
                    fwrite(&c,sizeof(char),1,ouf);
                    fwrite(&to.time,sizeof(double),1,ouf);
                }
            }
            if (AvgFreq)
            {   iouf = fopen("averages","w");
                fprintf(iouf,
                    "       Time NumCell NumSize SizeDiv    AgeSize");
                if(genotypes)
                    fprintf(iouf, " NumGeno GenoDiv    AgeGeno");
                fprintf(iouf,"\n\n");
            }
        }
        troot = AddTree(troot);
        dd.NumCellSTh = 0;
        dd.NumCellSTo = 0;
        dd.NumSize = 0;
        dd.SizeDiv = 0;
        dd.tot_size_age = 0.;
        if(genotypes)
        {   dd.NumCellGTh = 0;
            dd.NumCellGTo = 0;
            dd.NumGeno = 0;
            dd.GenoDiv = 0;
            dd.tot_geno_age = 0.;
        }

        divCountTree(troot);

#ifdef ERROR
        if(dd.NumCellSTh != to.TotPopTh)
            printf("dd.NumCellSTh = %ld  to.TotPopTh = %ld\n",
                dd.NumCellSTh, to.TotPopTh);
        if(dd.NumCellSTo != to.TotPopTo)
            printf("dd.NumCellSTo = %ld  to.TotPopTo = %ld\n",
                dd.NumCellSTo, to.TotPopTo);
        if(dd.NumSize != to.TotSizes)
            printf("dd.NumSize = %ld  to.TotSizes = %ld\n",
                dd.NumSize, to.TotSizes);
        if(genotypes)
        {   if(dd.NumGeno != to.TotGenos)
                printf("dd.NumGeno = %ld  to.TotGenos = %ld\n",
                    dd.NumGeno, to.TotGenos);
            if(dd.NumCellGTh != dd.NumCellSTh)
                printf("dd.NumCellGTh = %ld  dd.NumCellSTh = %ld\n",
                    dd.NumCellGTh, dd.NumCellSTh);
            if(dd.NumCellGTo != dd.NumCellSTo)
                printf("dd.NumCellGTo = %ld  dd.NumCellSTo = %ld\n",
                    dd.NumCellGTo, dd.NumCellSTo);
        }
#endif /* ERROR */

        if ((!genotypes && to.TotSizes) || (genotypes && to.TotGenos))
        {   AgeSize = dd.tot_size_age / (double) to.TotSizes;
            if (genotypes)
                AgeGeno = dd.tot_geno_age / (double) to.TotGenos;
            if (bdrecs)
            {   if (format)
                {   if (genotypes)
                        fsize += 1 +
                            fprintf(ouf,"%lx %ld %ld %g %lx %ld %g %lx\n",
                        lo.itime, to.TotPopTh, dd.NumSize, dd.SizeDiv,
                        (Ulong) AgeSize, dd.NumGeno, dd.GenoDiv,
                        (Ulong) AgeGeno);
                    else fsize += 1 + fprintf(ouf,"%lx %ld %ld %g %lx\n",
                        lo.itime, to.TotPopTh, dd.NumSize, dd.SizeDiv,
                        (Ulong) AgeSize);
                }
                else
                {   si.Time = lo.itime;
                    si.NumCell = to.TotPopTh;
                    si.NumSize = dd.NumSize;
                    si.SizeDiv = dd.SizeDiv;
                    si.AgeSize = (Ulong) AgeSize;
                    fwrite(&si, sizeof(struct siz), 1, ouf);
                    fsize += sizeof(struct siz);
                    if(genotypes)
                    {   ge.NumGeno = dd.NumGeno;
                        ge.GenoDiv = dd.GenoDiv;
                        ge.AgeGeno = (Ulong) AgeGeno;
                        fwrite(&ge, sizeof(struct gen), 1, ouf);
                        fsize += sizeof(struct gen);
                    }
                }
            }
        }
        tmp = (double) lo.itime;
        cu.Time    += tmp; icu.Time    += tmp;
        tmp = (double) lo.itime * to.TotPopTh;
        cu.NumCell += tmp; icu.NumCell += tmp;
        tmp = (double) lo.itime * dd.NumSize;
        cu.NumSize += tmp; icu.NumSize += tmp;
        tmp = (double) lo.itime * dd.SizeDiv;
        cu.SizeDiv += tmp; icu.SizeDiv += tmp;
        tmp = (double) lo.itime * AgeSize;
        cu.AgeSize += tmp; icu.AgeSize += tmp;
        if (genotypes)
        {   tmp = (double) lo.itime * dd.NumGeno;
            cu.NumGeno += tmp; icu.NumGeno += tmp;
            tmp = (double) lo.itime * dd.GenoDiv;
            cu.GenoDiv += tmp; icu.GenoDiv += tmp;
            tmp = (double) lo.itime * AgeGeno;
            cu.AgeGeno += tmp; icu.AgeGeno += tmp;
        }
        if (bdrecs && (fsize > size || first3))
        {   first3 = 0;
            breakrange();
        }
        if (bdrecs && fsize > size)
        {   fsize = 0;
            bonum++;
            fclose(ouf);
            sprintf(ofile,"%s.%d", bofile, bonum);
            if(format)
            {   ouf = fopen(ofile,"w");
                if(genotypes)
                    fprintf(ouf,"11 %.1lf\n", to.time);
                else
                    fprintf(ouf,"10 %.1lf\n", to.time);
            }
            else
            {   ouf = fopen(ofile,"wb");
                c = (char) format;
                fwrite(&c,sizeof(char),1,ouf);
                c = (char) genotypes;
                fwrite(&c,sizeof(char),1,ouf);
                fwrite(&to.time,sizeof(double),1,ouf);
            }
        }

        to.time += (double) lo.itime;
        mtime = (I32s) (to.time / 1000000L);
        if (mtime > otime)
        {
#ifdef __TURBOC__
            printf("Time = %4ld million  Coreleft = %6lu\r",
                mtime, coreleft());
#else  /* __TURBOC__ */
            printf("Time = %4ld million\r", mtime);
#endif /* __TURBOC__ */
            fflush(stdout);
            otime = mtime;
        }
        minmax2((double) to.time,&timer);
        minmax2((double) to.TotPopTh,&ncr);
        minmax2((double) dd.NumSize,&nsr);
        minmax2((double) dd.SizeDiv,&sdivr);
        minmax2((double) AgeSize,&asar);
        if(genotypes)
        {   minmax2((double) dd.GenoDiv,&gdivr);
            minmax2((double) dd.NumGeno,&ngr);
            minmax2((double) AgeGeno,&agar);
        }
        if (AvgFreq)
        {   iamtime = (I32s) (to.time / AvgFreq);
            if (iamtime > iaotime)
            {   doaverages();
                icu.Time    = 0.; icu.NumCell = 0.; icu.NumSize = 0.;
                icu.SizeDiv = 0.; icu.AgeSize = 0.; icu.NumGeno = 0.;
                icu.GenoDiv = 0.; icu.AgeGeno = 0.;
                iaotime = iamtime;
            }
        }
    }
    doranges();
    fclose(our);
    if (bdrecs)
    {   breakrange();
        fclose(ouf);
    }
    if (AvgFreq)
        fclose(iouf);
}

void doaverages()
{   fprintf(iouf,"%11.0lf %7.1lf %7.1lf %7.2lf %10.0lf",
        to.time, icu.NumCell / icu.Time, icu.NumSize / icu.Time,
        icu.SizeDiv / icu.Time, icu.AgeSize / icu.Time);
    if(genotypes)
    {   fprintf(iouf," %7.1lf %7.2lf %10.0lf",
            icu.NumGeno / icu.Time, icu.GenoDiv / icu.Time,
            icu.AgeGeno / icu.Time);
    }
    fprintf(iouf,"\n");
}

void breakrange()
{   if(genotypes)
        fprintf(our,"%12s %.0lf %.0lf %.0lf %g %ld %.0lf %g %ld\n",
            ofile, timer.x, ncr.x, nsr.x,
            sdivr.x, (Ulong) asar.x, ngr.x,
            gdivr.x, (Ulong) agar.x);
    else
        fprintf(our,"%12s %.0lf %.0lf %.0lf %g %ld\n",
            ofile, timer.x, ncr.x, nsr.x,
            sdivr.x, (Ulong) asar.x);
}

void doranges()
{   FILE  *ouf;

    ouf = fopen("divrange","w");
    fprintf(ouf, "           Minimum      Maximum      Average Number\n\n");
    fprintf(ouf,"Time    %10.0lf %12.0lf              %ld\n",
        timer.n, timer.x, timer.m);
    fprintf(ouf,"NumCell %10.0lf %12.0lf %12.1lf %ld\n",
        ncr.n,   ncr.x,   cu.NumCell / cu.Time, ncr.m);
    fprintf(ouf,"NumSize %10.0lf %12.0lf %12.1lf %ld\n",
        nsr.n,   nsr.x,   cu.NumSize / cu.Time, nsr.m);
    fprintf(ouf,"SizeDiv %10lf %12lf %12lf %ld\n",
        sdivr.n, sdivr.x, cu.SizeDiv / cu.Time, sdivr.m);
    fprintf(ouf,"AgeSize %10.0lf %12.0lf %12.1lf %ld\n",
        asar.n,  asar.x,  cu.AgeSize / cu.Time, asar.m);
    if(genotypes)
    {   fprintf(ouf,"NumGeno %10.0lf %12.0lf %12.1lf %ld\n",
            ngr.n,   ngr.x,   cu.NumGeno / cu.Time, ngr.m);
        fprintf(ouf,"GenoDiv %10lf %12lf %12lf %ld\n",
            gdivr.n, gdivr.x, cu.GenoDiv / cu.Time, gdivr.m);
        fprintf(ouf,"AgeGeno %10.0lf %12.0lf %12.1lf %ld\n",
            agar.n,  agar.x,  cu.AgeGeno / cu.Time, agar.m);
    }
    fclose(ouf);
}

struct tnode * AddTree(p)
struct tnode  *p;
{   int  i, j, osize;
    struct pop_dat  *pd;

    if(p == NULL) /* this is a new size class */
    {   if(lo.bd == 'd')
        {   printf("new node is a death, exiting\n");
            doranges();
            exit(0);
        }
        p = (struct tnode  *) calloc(1,sizeof(struct tnode));
        if(p == NULL)
        {   printf("calloc failure, exiting\n");
            doranges();
            exit(0);
        }
        p->size = lo.size;         /* initialize this size */
        p->sd.pop = 1;
        p->sd.age = (-lo.itime);
        p->sd.popt = 0;
        to.TotPopTo++;
        if(genotypes)               /* allocate genotype array */
        {   i = Lbl2Int(lo.label); /* & initialize first genotype */
            p->gsize = i + 1;
            p->g = (struct pop_dat  *)
                calloc(p->gsize,sizeof(struct pop_dat));
            if(p->g == NULL)
            {   printf("calloc failure, exiting\n");
                doranges();
                exit(0);
            }
            pd = p->g + i;
            pd->age = (-lo.itime);
            pd->pop = 1;
            pd->popt = 0;
            if (pd->pop == thr)
            {   to.TotGenos++;
                if (!p->ntg)
                    to.TotSizes++;
                p->ntg++;
                to.TotPopTh++;
            }
        }
        else if (p->sd.pop == thr)
        {   to.TotSizes++;
            to.TotPopTh++;
            p->ntg++;
        }
        if (p->ntg)
        {   p->sd.popt++;
            pd->popt++;
        }
        p->l = p->r = NULL;
    }
    else if(lo.size < p->size) /* if not this size */
        p->l = AddTree(p->l);
    else if(lo.size > p->size)
        p->r = AddTree(p->r);
    else                        /* this size, but not new */
    {   if(genotypes) /* reallocating gsize array */
        {   i = Lbl2Int(lo.label);
            if(i >= p->gsize)
            {   osize = p->gsize;
                p->gsize = i + 5;
                if(p->g == NULL)
                    p->g = (struct pop_dat  *)
                        calloc(p->gsize, sizeof(struct pop_dat));
                else
                    p->g = (struct pop_dat  *)
                        realloc(p->g, p->gsize * sizeof(struct pop_dat));
                if(p->g == NULL)
                {   printf("realloc failure, exiting\n");
                    doranges();
                    exit(0);
                }
                for(j = osize; j < p->gsize; j++)
                {   pd = p->g + j;
                    pd->pop = pd->age = 0;
                }
            }
        }
        if(lo.bd == 'b') /* this is a birth */
        {   if(!p->sd.pop)
                p->sd.age = (-lo.itime);
            p->sd.pop++;
            to.TotPopTo++;
            if(genotypes)
            {   pd = p->g + i;
                if(!pd->pop)
                    pd->age = (-lo.itime);
                pd->pop++;
                if (pd->pop == thr)
                {   to.TotGenos++;
                    if (!p->ntg)
                        to.TotSizes++;
                    to.TotPopTh += pd->pop;
                    pd->popt = pd->pop;
                    p->sd.popt += pd->pop;
                    p->ntg++;
                }
                else if (pd->pop > thr)
                {   to.TotPopTh++;
                    pd->popt++;
                    p->sd.popt++;
                }
            }
            else if (p->sd.pop == thr)
            {   to.TotSizes++;
                to.TotPopTh += p->sd.pop;
                p->sd.popt = p->sd.pop;
                p->ntg++;
            }
            else if (p->sd.pop > thr)
            {   to.TotPopTh++;
                p->sd.popt++;
            }
        }
        else /* this is a death */
        {   p->sd.pop--;
            to.TotPopTo--;
            if(!p->sd.pop)
                p->sd.age = 0;
            if(genotypes)
            {   pd = p->g + i;
                pd->pop--;
                if(!pd->pop)
                    pd->age = 0;
                if (pd->pop == thr - 1)
                {   to.TotGenos--;
                    p->ntg--;
                    if (!p->ntg)
                        to.TotSizes--;
                    to.TotPopTh -= thr;
                    p->sd.popt -= thr;
                    pd->popt = 0;
                }
                else if (pd->pop >= thr)
                {   to.TotPopTh--;
                    pd->popt--;
                    p->sd.popt--;
                }
            }
            else if (p->sd.pop == thr - 1)
            {   to.TotSizes--;
                to.TotPopTh -= pd->pop;
                p->sd.popt = 0;
                p->ntg--;
            }
            else if (p->sd.pop >= thr)
            {   to.TotPopTh--;
                p->sd.popt--;
            }
        }
    }
    return p;
}

void divCountTree(t)
struct tnode  *t;
{   struct pop_dat  *pd;
    int i;

    if(t != NULL)
    {   if(t->sd.pop > 0)
        {   t->sd.age += lo.itime;
            dd.NumCellSTo += t->sd.pop;
            if (t->ntg)
            {   dd.NumSize++;
                dd.tot_size_age += (double) t->sd.age;
                dd.NumCellSTh += t->sd.popt;
                dd.SizeDiv -= ((float) t->sd.popt / (float) to.TotPopTh)
                    * (log((float) t->sd.popt / (float) to.TotPopTh));
            }
            if(genotypes) for(i = 0; i < t->gsize; i++)
            {   pd = t->g + i;
                if(pd->pop > 0)
                {   pd->age += lo.itime;
                    dd.NumCellGTo += pd->pop;
                    if (pd->pop >= thr)
                    {   dd.GenoDiv -= ((float) pd->popt / (float) to.TotPopTh)
                            * (log((float) pd->popt / (float) to.TotPopTh));
                        dd.NumGeno++;
                        dd.tot_geno_age += (double) pd->age;
                        dd.NumCellGTh += pd->popt;
                    }
                }
            }
        }
        divCountTree(t->l);
        divCountTree(t->r);
    }
}

void minmax2(v, r)
double  v;
struct range  *r;
{   if(r->f)
    {   r->f = 0;
        r->n = r->x = v;
    }
    if(v < r->n) r->n = v; if(v > r->x) r->x = v;
    r->m++;
}

int Lbl2Int(s)
char  s[];
{   if(s[0] == '-') return 0;
    return 1 + (s[2]- 'a') + (26 * (s[1] - 'a')) + (676 * (s[0] - 'a'));
}

int t_read(data, lo, first, genotypes)
char  data[];
struct last_out  *lo;
int  *first;
int  *genotypes;
{   struct last_out  ti;
    int    nargs;
    char   v2[9], v3[9], v4[9];

    sscanf(data,"%s", v2);
    if(!strcmp(v2,"num_sp")) return 0;
    nargs = sscanf(data,"%lx%s%s%s", &ti.time, v2, v3, v4);
    lo->itime = ti.time;
    if(*first)
    {   *first = 0;
        if(nargs == 4) *genotypes = 1;
        else *genotypes = 0;
        lo->time += ti.time;     /* assumes lo structure initialized to zero */
        if(lo->time >= 1000000L)
        {   lo->time %= 1000000L;
            lo->ctime++;
        }
        lo->bd = v2[0];
        sscanf(v3,"%u", &lo->size);
        if(*genotypes) strcpy(lo->label,v4);
        else strcpy(lo->label,"");
    }
    else
    {   lo->time += ti.time;
        if(lo->time >= 1000000L)
        {   lo->time %= 1000000L;
            lo->ctime++;
        }
        if(*genotypes) switch(nargs)
        {   case 1: break;
            case 2:
            {   if(isdigit(v2[0]))
                {   sscanf(v2,"%u", &lo->size); break; }
                else
                {   if(strlen(v2) == 1)
                    {   lo->bd = v2[0]; break; }
                    else
                    {   strcpy(lo->label,v2); break; }
                }
            }
            case 3:
            {   if(isdigit(v2[0]))
                {   sscanf(v2,"%u", &lo->size);
                    strcpy(lo->label,v3);
                }
                else
                {   lo->bd = v2[0];
                    if(isdigit(v3[0]))
                        sscanf(v3,"%u", &lo->size);
                    else
                        strcpy(lo->label,v3);
                }
                break;
            }
            case 4:
            {   lo->bd = v2[0];
                sscanf(v3,"%u", &lo->size);
                strcpy(lo->label,v4);
                break;
            }
        }
        else switch(nargs)
        {   case 1: break;
            case 2:
            {   if(isdigit(v2[0]))
                    sscanf(v2,"%u", &lo->size);
                else
                    lo->bd = v2[0];
                break;
            }
            case 3:
            {   lo->bd = v2[0];
                sscanf(v3,"%u", &lo->size);
                break;
            }
        }
    }
    return 1;
}
