/* tierun.c  27-3-92  makes run_info file for tierra.run, with genotypes */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

extern int  genotypes, update;
extern char  directory[];
char  infile[13];
FILE  *inf, *souf, *gouf;
HWND  witi;

struct pop_dat  totals;
struct last_out  rlo;
Ulong  sctime, stime, tctime, ttime;

int Lbl2Int(char  s[])
{   if(s[0] == '-') return 0;
    return 1 + (s[2]- 'a') + (26 * (s[1] - 'a')) + (676 * (s[0] - 'a'));
}

void Int2Lbl(int  i, char  s[])
{   if(!i)
    {   strcpy(s,"---");
        return;
    }
    i--;
    s[0] = 'a' + i / 676;
    i %= 676;
    s[1] = 'a' + i / 26;
    i %= 26;
    s[2] = 'a' + i;
    s[3] = 0;
}

int gtierunc(void)
{   int  i;
    static int  first = 1;
    HWND         whgt, whins;
    TRANSACTION  *ptgt;

    if(first)
    {   first = 0;
        genotypes = 0;
        update = 100;
        sprintf(infile,"break.1");
        sprintf(directory,"c:\\tierra\\td");
        wind_setup();
    }
    while(1) {
    whgt = wigen(2,4,3,55,FRDOUBLE,"tierun");
    ptgt = vpreload(3,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 2; i++) (*(ptgt->def))[i].use_ext = TRUE;
    vdeffld(ptgt,0,STRING,0,1,"directory  =",-1,40,NULL,directory,NO);
    vdeffld(ptgt,1,STRING,1,1,"input file =",-1,12,NULL,infile,NO);
    vdeffld(ptgt,2,INTEGER,2,1,"update frequency   =",-1,5,NULL,&update,NO);
    ptgt->fld_valfn = temp_info_chk_g;
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whgt,YES,YES);
    if(DWSUCCESS == vread(ptgt,whgt,YES))
    {   vdelete(whgt,NONE); vdelete(whins,NONE);
        gtierun();
    }
    else
    {   vdelete(whgt,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int temp_info_chk_g(TRANSACTION  *tp)
{   char  path[80];
    int   success;

    if(tp->cur_fld == 0)
    {   getcwd(path,79);
        success = chdir(directory);
        chdir(path);
        if(success)
        {   wierror(2,45,"directory not found");
            return FALSE;
        }
        return TRUE;
    }
    if(tp->cur_fld == 1)
    {   sprintf(path,"%s\\%s", directory, infile);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"file not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    return TRUE;
}

void gtierun(void)
{   int  first = 1, upcount = update;
    struct tnode  *uroot = NULL, *troot = NULL;
    Ulong c = 0;
    char  data[81];
    char   bifile[13];
    int    binum = 1;

    sctime = stime = tctime = ttime = 0;
    rlo.time = rlo.ctime = rlo.size = 0;
    witi = wigen(7,30,2,19,FRDOUBLE,"time");
    sscanf(infile,"%[^.]", bifile);
    sprintf(data,"%s\\%s", directory, infile);
    inf = fopen(data,"r");
    if(inf == NULL)
    {   wierror(2,45,"input file not found");
        return ;
    }
    sprintf(data,"%s\\run_info", directory);
    souf = fopen(data,"w");
    sprintf(data,"%s\\run_info.g", directory);
    gouf = fopen(data,"w");
    for(;;)
    {   if(fgets(data,80,inf) == NULL)
        {   fclose(inf);
            binum++;
            sprintf(data,"%s/%s.%d", directory, bifile, binum);
            inf = fopen(data,"r");
            if(inf == NULL)
                break ;
            if(fgets(data,80,inf) == NULL)
                break ;
        }
        if(!t_read(data, &rlo, &first, &genotypes))
        {   StartFragment(data,&rlo,&uroot,&troot,c);
            sctime = rlo.ctime; stime = rlo.time;
            if(!c)
            {   tctime = sctime; ttime = stime; }
            while(rlo.ctime > upcount)
                upcount += update;
            continue;
        }
        if(!(c%100LU))
            vatputf(witi,0,1,"%6lu,%06lu", rlo.ctime, rlo.time);
        c++;
        if(update && rlo.ctime >= upcount)
        {   upcount += update;
            uroot = DoUpdate(uroot,update);
            sctime = rlo.ctime; stime = rlo.time;
        }
        if(update) uroot = AddTree(uroot,&rlo);
        troot = AddTree(troot,&rlo);
    }
    vdelete(witi,NONE);
    fclose(inf);
    if(update) uroot = DoUpdate(uroot,update);
    FreeTTree(uroot);
    sctime = tctime; stime = ttime;
    troot = DoUpdate(troot,0);
    FreeTTree(troot);
    fclose(souf); fclose(gouf);
    if(!genotypes)
    {   sprintf(data,"del %s\\run_info.g", directory);
        system(data);
    }
}

struct tnode * AddTree(struct tnode  *p, struct last_out  *lo)
{   int  i, j, osize;
    struct pop_dat  *pd;

    if(p == NULL)
    {   if(lo->bd == 'd')
        {   wierror(2,45,"new node is a death, exiting");
            vexit(0);
        }
        p = (struct tnode  *) calloc(1,sizeof(struct tnode));
        if(p == NULL)
        {   wierror(2,45,"calloc failure, exiting");
            vexit(0);
        }
        else
            vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        p->size = lo->size;
        p->sd.max = p->sd.end = 1;
        p->sd.min = p->sd.beg = 0;
        if(genotypes)
        {   i = Lbl2Int(lo->label);
            p->gsize = i + 1;
            p->g = (struct pop_dat  *)
                calloc(p->gsize,sizeof(struct pop_dat));
            if(p->g == NULL)
            {   wierror(2,45,"calloc failure, exiting");
                vexit(0);
            }
            else
                vatputf(witi,1,1,"coreleft = %6lu", coreleft());
            (p->g + i)->end = (p->g + i)->max = 1;
        }
        p->l = p->r = NULL;
    }
    else if(lo->size < p->size)
        p->l = AddTree(p->l,lo);
    else if(lo->size > p->size)
        p->r = AddTree(p->r,lo);
    else
    {   if(genotypes)
        {   i = Lbl2Int(lo->label);
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
                {   wierror(2,45,"realloc failure, exiting");
                    vexit(0);
                }
                else
                    vatputf(witi,1,1,"coreleft = %6lu", coreleft());
                for(j = osize; j < p->gsize; j++)
                {   pd = p->g + j;
                    pd->max = pd->min = pd->beg = pd->end = 0;
                }
            }
        }
        if(lo->bd == 'b')
        {   p->sd.end++;
            if(p->sd.end > p->sd.max)
                p->sd.max = p->sd.end;
            if(genotypes)
            {   pd = p->g + i;
                pd->end++;
                if(pd->end > pd->max)
                    pd->max = pd->end;
            }
        }
        else
        {   p->sd.end--;
            if(p->sd.end < p->sd.min)
                p->sd.min = p->sd.end;
            if(genotypes)
            {   pd = p->g + i;
                pd->end--;
                if(pd->end < pd->min)
                    pd->min = pd->end;
            }
        }
    }
    return p;
}

struct snode * Tree2Tree(struct tnode  *t, struct snode  *s, int  index)
{   int  i;

    if(t != NULL)          /* sort tnode tree into snode tree */
    {   s = Tree2Tree(t->l,s,index);
        if(index) for(i = 0; i < t->gsize; i++)
        {  if((t->g + i)->max > 0)
               s = AddMaxTree(t,i,s);
        }
        else s = AddMaxTree(t,-1,s);
        s = Tree2Tree(t->r,s,index);
    }
    return s;
}

struct snode * AddMaxTree(struct tnode  *t, int  index, struct snode  *s)
{   int  i;
    long  osiz;

    if(s == NULL)
    {   s = (struct snode  *) calloc(1,sizeof(struct snode));
        if(s == NULL)
        {   wierror(2,45,"calloc failure, exiting");
            vexit(0);
        }
        else
            vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        if(index == -1) s->max = t->sd.max;
        else s->max = (t->g + index)->max;
        s->num = 1;
        s->gsiz = 1;
        s->g = (struct gene_dat  *) calloc(s->gsiz,sizeof(struct gene_dat));
        if(s->g == NULL)
        {   wierror(2,45,"calloc failure, exiting");
            vexit(0);
        }
        else
            vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        s->g->t = t;
        s->g->index = index;
        s->l = s->r = NULL;
    }
    else if((index == -1 && (t->sd.max < s->max)) ||
        (index != -1 && ((t->g + index)->max < s->max)))
        s->l = AddMaxTree(t,index,s->l);
    else if((index == -1 && (t->sd.max > s->max)) ||
        (index != -1 && ((t->g + index)->max > s->max)))
        s->r = AddMaxTree(t,index,s->r);
    else
    {   s->num++;
        if(s->num > s->gsiz)
        {   osiz = s->gsiz;
            s->gsiz *= 1.1;
            if(s->gsiz == osiz) s->gsiz++;
            s->g = (struct gene_dat  *)
                realloc(s->g, s->gsiz * sizeof(struct gene_dat));
            if(s->g == NULL)
            {   wierror(2,45,"realloc failure, exiting");
                vexit(0);
            }
            else
                vatputf(witi,1,1,"coreleft = %6lu", coreleft());
            for(i = osiz; i < s->gsiz; i++)
            {   (s->g + i)->t = NULL;
                (s->g + i)->index = -1;
            }
        }
        i = s->num - 1;
        (s->g + i)->t = t;
        (s->g + i)->index = index;
    }
    return s;
}

void OutSearchTree(struct snode  *s, FILE  *ouf, int  *out_num, int  update)
{   int   i, in;
    char  lbl[4];
    struct pop_dat  *p;

    if(update && *out_num <= 0) return;
    if(s != NULL)
    {   OutSearchTree(s->r, ouf, out_num, update);
        if(s->max) for(i = 0; i < s->num; i++)
        {   if(update && *out_num <= 0) return;
            in = (s->g + i)->index;
            if(in == -1)
            {   strcpy(lbl,"---");
                p = &((s->g + i)->t->sd);
            }
            else
            {   Int2Lbl(in,lbl);
                p = (s->g + i)->t->g + in;
            }
            fprintf(ouf,"%04ld%s	%ld	%ld	%ld	%ld\n",
                (s->g + i)->t->size, lbl, p->min, p->max, p->end, p->beg);
            if(update) (*out_num)--;
        }
        OutSearchTree(s->l, ouf, out_num, update);
    }
}

void CountTree(struct snode  *s, int  *num)
{   if(s != NULL)
    {   if(s->max) *num += s->num;
        CountTree(s->l,num);
        CountTree(s->r,num);
    }
}

void OutUpdate(struct snode  *s, FILE  *ouf, int  update)
{   int  num_sp = 0, out_num = 55;

    CountTree(s, &num_sp);
    if(update && num_sp > 55) num_sp = 55;
    out_num = num_sp;
    fprintf(ouf,"num_sp = %d  sctime = %ld  stime = %ld  update = %d\n",
        num_sp, sctime, stime, update);
    fprintf(ouf,"classes	min	max	end	beg\n");
    OutSearchTree(s, ouf, &out_num, update);
}

void FreeSTree(struct snode  *p)
{   if(p != NULL)
    {   FreeSTree(p->l);
        FreeSTree(p->r);
        free(p->g);
        free(p);
    }
}

void FreeTTree(struct tnode  *p)
{   if(p != NULL)
    {   FreeTTree(p->l);
        FreeTTree(p->r);
        free(p->g);
        free(p);
    }
}

struct tnode * oldPruneTree(struct tnode  *p, struct tnode  *root)
{   if(p != NULL)
    {   if(p->sd.end > 0)
            root = AddPrunedTree(root,p);
        oldPruneTree(p->l,root);
        oldPruneTree(p->r,root);
    }
    return root;
}

void PruneTree(struct tnode  *p)
{   int  i;
    struct pop_dat  *pd;

    if(p != NULL)
    {   if(p->sd.end <= 0)
        {   p->sd.max = p->sd.min = p->sd.beg = p->sd.end = 0;
            if(genotypes)
            {   free(p->g);
                p->g = NULL;
                p->gsize = 0;
            }
        }
        else
            p->sd.max = p->sd.beg = p->sd.end; p->sd.min = 0;
        if(genotypes) for(i = 0; i < p->gsize; i++)
        {   pd = p->g + i;
            if(pd->end <= 0)
                pd->max = pd->min = pd->beg = pd->end = 0;
            else
                pd->max = pd->beg = pd->end; pd->min = 0;
        }
        PruneTree(p->l);
        PruneTree(p->r);
    }
}

struct tnode * AddPrunedTree(struct tnode  *r, struct tnode *p)
{   int  i;
    struct pop_dat  *d;

    if(r == NULL)
    {   r = (struct tnode  *) calloc(1,sizeof(struct tnode));
        if(r == NULL)
        {   wierror(2,45,"calloc failure, exiting");
            vexit(0);
        }
        else
            vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        *r = *p;
        r->sd.beg = r->sd.max = r->sd.end; r->sd.min = 0;
        if(genotypes) for(i = 0; i < r->gsize; i++)
        {   d = r->g + i;
            if(d->end <= 0)
                d->beg = d->max = d->min = d->end = 0;
            else
            {   d->beg = d->max = d->end; d->min = 0;
            }
        }
        r->l = r->r = NULL;
    }
    else if(p->size < r->size)
        r->l = AddPrunedTree(r->l,p);
    else if(p->size > r->size)
        r->r = AddPrunedTree(r->r,p);
    else if(p->size == r->size)
        wierror(5,45,"size repeated in pruned tree");
    return r;
}

struct tnode * DoUpdate(struct tnode  *p, int  update)
{   struct snode  *sroot = NULL;

    sroot = Tree2Tree(p,sroot,0);
    OutUpdate(sroot, souf, update);
    FreeSTree(sroot);
    vatputf(witi,1,1,"coreleft = %6lu", coreleft());
    sroot = NULL;
    sroot = Tree2Tree(p,sroot,1);
    OutUpdate(sroot, gouf, update);
    FreeSTree(sroot);
    vatputf(witi,1,1,"coreleft = %6lu", coreleft());
    if(!update) return p;
    PruneTree(p);
    return p;
}

struct tnode * AddFragTree(struct tnode *p, long size, char lbl[], long end)
{   int  i, j;
    long  osize;

    if(p == NULL)
    {   p = (struct tnode  *) calloc(1,sizeof(struct tnode));
        if(p == NULL)
        {   wierror(2,45,"calloc failure, exiting");
            vexit(0);
        }
        else
            vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        p->size = size;
        p->sd.beg = p->sd.end = p->sd.max = end;
        if(genotypes)
        {   i = Lbl2Int(lbl);
            p->gsize = i + 1;
            p->g = (struct pop_dat  *) calloc(p->gsize,sizeof(struct pop_dat));
            if(p->g == NULL)
            {   wierror(2,45,"calloc failure, exiting");
                vexit(0);
            }
            else
                vatputf(witi,1,1,"coreleft = %6lu", coreleft());
            (p->g + i)->beg = (p->g + i)->end = (p->g + i)->max = end;
        }
        p->l = p->r = NULL;
    }
    else if(size < p->size)
        p->l = AddFragTree(p->l,size,lbl,end);
    else if(size > p->size)
        p->r = AddFragTree(p->r,size,lbl,end);
    else
    {   if(genotypes)
        {   i = Lbl2Int(lbl);
            if(i >= p->gsize)
            {   osize = p->gsize;
                p->gsize = i + 5;
                p->g = (struct pop_dat  *)
                    realloc(p->g, p->gsize * sizeof(struct pop_dat));
                if(p->g == NULL)
                {   wierror(2,45,"realloc failure, exiting");
                    vexit(0);
                }
                else
                    vatputf(witi,1,1,"coreleft = %6lu", coreleft());
                for(j = osize; j < p->gsize; j++)
                    (p->g + j)->max = (p->g + j)->min =
                    (p->g + j)->beg = (p->g + j)->end = 0;
            }
            (p->g + i)->max = (p->g + i)->beg = (p->g + i)->end = end;
        }
        p->sd.max += end; p->sd.end += end; p->sd.beg += end;
    }
    return p;
}

void ZeroTree(struct tnode  *p)
{   int  i;

    if(p != NULL)
    {   ZeroTree(p->l);
        ZeroTree(p->r);
        p->sd.end = 0;
        if(genotypes)
            for(i = 0; i < p->gsize; i++)
                (p->g + i)->end = 0;
    }
}

void StartFragment(char  data[], struct last_out  *lo,
    struct tnode  **uroot, struct tnode  **troot, int  c)
{   int  i, lnum_sp;
    long  size, end;
    char  ldata[81], lbl[4];

    if(update && c) *uroot = DoUpdate(*uroot, update);
    if(c)
    {   FreeTTree(*uroot);
        vatputf(witi,1,1,"coreleft = %6lu", coreleft());
        *uroot = NULL;
        ZeroTree(*troot);
    }
    sscanf(data,"%*s%*s%d%*s%*s%lu%*s%*s%lu", &lnum_sp, &lo->ctime, &lo->time);
    for(i = 0; i < lnum_sp; i++)
    {   fgets(ldata,80,inf);
        sscanf(ldata,"%ld%s%ld", &size, lbl, &end);
        if(!i)
	{   if(!strcmp(lbl,"---"))
                genotypes = 0;
            else
                genotypes = 1;
        }
        *uroot = AddFragTree(*uroot,size,lbl,end);
        *troot = AddFragTree(*troot,size,lbl,end);
    }
}
