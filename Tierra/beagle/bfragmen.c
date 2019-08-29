/* fragment.c  27-3-92  makes fragment of tierra.run */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

extern int   genotypes;
extern char  directory[];

void OutFragTree(struct tnode  *t, FILE  *ouf);
void CountTTree(struct tnode  *t, int  *num);

int fragment(void)
{   int  i, start, stop;
    char  infile[13], oufile[13];
    static int  first = 1;
    HWND         whgt, whins;
    TRANSACTION  *ptgt;

    if(first)
    {   first = 0; start = 0; stop = 1;
        sprintf(infile,"break.1"); sprintf(oufile,"fragment.run");
        sprintf(directory,"c:\\tierra\\td");
        wind_setup();
    }
    while(1) {
    whgt = wigen(2,4,5,56,FRDOUBLE,"fragment");
    ptgt = vpreload(5,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 4; i++) (*(ptgt->def))[i].use_ext = TRUE;
    vdeffld(ptgt,0,STRING,0,1, "directory   =",-1,40,NULL,directory,NO);
    vdeffld(ptgt,1,STRING,1,1, "input file  =",-1,12,NULL,infile,NO);
    vdeffld(ptgt,2,STRING,2,1, "output file =",-1,12,NULL,oufile,NO);
    vdeffld(ptgt,3,INTEGER,3,1,"start time  =",-1,5,NULL,&start,NO);
    vdeffld(ptgt,4,INTEGER,4,1,"stop time   =",-1,5,NULL,&stop,NO);
    ptgt->fld_valfn = temp_info_chk_f;
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whgt,YES,YES);
    if(DWSUCCESS == vread(ptgt,whgt,YES))
    {   vdelete(whgt,NONE); vdelete(whins,NONE);
        fragmen(infile,oufile,directory,start,stop);
    }
    else
    {   vdelete(whgt,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int temp_info_chk_f(TRANSACTION  *tp)
{   FILE  *inf;
    char  path[80], cpath[80];
    int  success;

    if(tp->cur_fld == 0)
    {   getcwd(cpath,79);
        success = chdir((*(tp->def))[0].dataptr);
        chdir(cpath);
        if(success)
        {   wierror(2,45,"directory not found");
            return FALSE;
        }
        return TRUE;
    }
    if(tp->cur_fld == 1)
    {   sprintf(path,"%s/%s", (*(tp->def))[0].dataptr,
            (*(tp->def))[1].dataptr);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"file not found");
            return FALSE;
        }
        fclose(inf);
    }
    return TRUE;
}

void fragmen(char  *infile, char  *oufile, char  *directory,
    int  start, int  stop)
{   int  first = 1, not_yet = 1, species = 0;
    struct tnode  *troot = NULL;
    struct last_out  lo;
    long  time;
    Ulong c = 0;
    char  data[81], *obufr;
    HWND  witi;
    FILE  *inf, *ouf;
    char   bifile[13];
    int    binum = 1;

    sscanf(infile,"%[^.]", bifile);
    sprintf(data,"%s/%s", directory, infile);
    inf = fopen(data,"r");
    if(inf == NULL)
    {   wierror(2,45,"input file not found");
        return ;
    }
    sprintf(data,"%s/%s", directory, oufile);
    ouf = fopen(data,"w");
    lo.ctime = lo.time = 0;
    genotypes = 1;
    obufr = (char  *) calloc(8192, sizeof(char));
    if(obufr == NULL)
    {   wierror(2,45,"calloc failure, exiting");
        vexit(0);
    }
    setvbuf(ouf,obufr,_IOFBF,8192);
    witi = wigen(7,33,1,15,FRDOUBLE,"time");
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
        t_read(data, &lo, &first, &genotypes);
        if(!(c%100LU))
            vatputf(witi,0,1,"%5lu,%06lu", lo.ctime, lo.time);
        c++;
        if(lo.ctime < start) troot = AddTree(troot,&lo);
        if(lo.ctime >= start && lo.ctime <= stop)
        {   if(not_yet && start > 0)
            {   not_yet = 0;
                CountTTree(troot,&species);
                fprintf(ouf,"num_sp = %d  ctime = %lu  time = %lu\n",
                    species, lo.ctime, lo.time );
                OutFragTree(troot,ouf);
                sscanf(data,"%lx", &time);
                fprintf(ouf,"%lx %c %d", time, lo.bd, lo.size);
                if(genotypes) fprintf(ouf," %s", lo.label);
                fprintf(ouf,"\n");
                FreeTTree(troot);
            }
            else
            fprintf(ouf,"%s",data);
        }
        if(lo.ctime > stop) break;
    }
    vdelete(witi,NONE);
    fclose(inf);
    fclose(ouf);
    free(obufr);
}

void CountTTree(struct tnode  *t, int  *num)
{   int  i;

    if(t != NULL)
    {   if(genotypes)
        {   for(i = 0; i < t->gsize; i++)
                if((t->g + i)->end > 0)
                    (*num)++;
        }
        else
            if(t->sd.end > 0)
                (*num)++;
        CountTTree(t->l,num);
        CountTTree(t->r,num);
    }
}

void OutFragTree(struct tnode  *t, FILE  *ouf)
{   int  i;
    char  lbl[4];

    if(t != NULL)
    {   if(genotypes) for(i = 0; i < t->gsize; i++)
        {   if((t->g + i)->end <= 0)
                continue ;
            Int2Lbl(i,lbl);
            fprintf(ouf,"%04ld%s %ld\n", t->size, lbl, (t->g + i)->end);
        }
        else if(t->sd.end > 0)
        {   strcpy(lbl,"---");
            fprintf(ouf,"%04ld%s %ld\n", t->size, lbl, t->sd.end);
        }
        OutFragTree(t->r, ouf);
        OutFragTree(t->l, ouf);
    }
}

/*
struct rspecies * fix_array(struct rspecies  *sp, int *num_sp, int *array_siz,
    struct last_out  *lo)
{    int  i, in = 1, found;

    found = 0;
    for(i = 1; i <= *num_sp; i++)
    {   if(genotypes)
        {   if(sp[i].size == lo->size && !strcmp(sp[i].label,lo->label))
            { in = i; found = 1; break; }
        }
        else
        {   if(sp[i].size == lo->size)
            { in = i; found = 1; break; }
        }
    }
    if(!found)
    {   (*num_sp)++; in = *num_sp;
        if(*num_sp >= *array_siz)
        {   (*array_siz) += 50;
            sp = (struct rspecies *)
                realloc(sp, *array_siz * sizeof(struct rspecies));
            if(sp == NULL) vexit(0);
            for(i = *array_siz - 50; i < *array_siz; i++)
            {   sp[i].size = sp[i].num = sp[i].begin = sp[i].max =
                    sp[i].min = 0;
                sprintf((sp + i)->label,"---");
            }
        }
        sp[in].size = lo->size;
        if(genotypes) sprintf(sp[in].label,lo->label);
    }
    count(sp, lo->bd);
    count(sp + in, lo->bd);
    return sp;
}

void count(struct rspecies  *s, char  bd)
{   if(bd == 'b') s->num++;
    if(bd == 'd') s->num--;
    if(s->num < s->min) s->min = s->num;
    if(s->num > s->max) s->max = s->num;
}
*/
