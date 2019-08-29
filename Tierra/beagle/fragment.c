/* fragment.c  27-3-92  makes fragment of tierra.run */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "dir.h"
#include "alloc.h"
#include "ctype.h"

typedef unsigned int   Uint;
typedef unsigned long  Ulong;

struct last_out {
    Ulong  time;
    Ulong  ctime;
    char   bd;
    Uint   size;
    char   label[4];
    } ;

struct pop_dat {
    long  max; /* maximum population of this genotype */
    long  min; /* minimum population of this genotype */
    long  beg; /* beginning population of this genotype */
    long  end; /* current population of this genotype */
    } ;

struct gene_dat {
    struct tnode  *t;
    long          index;
    } ;

struct snode {
    long             max;  /* max pop value upon which tree is sorted */
    long             num;  /* number of genotypes of this max value */
    long             gsiz; /* allocated size of *gd array */
    struct gene_dat  *g;
    struct snode     *l;  /* left sub-tree */
    struct snode     *r;  /* right sub-tree */
    } ;

struct tnode {
    long            size;  /* genome size */
    struct pop_dat  sd; /* pop_dat for this size class */
    int             gsize; /* allocated size of *g array */
    struct pop_dat  *g; /* array of pop_dat structures */
    struct tnode    *l;  /* left sub-tree */
    struct tnode    *r;  /* right sub-tree */
    } ;

int   genotypes;
char  directory[80];

int  main(void);
int  temp_info_chk_f(char  *infile, char  *directory);
int  t_read(char  data[], struct last_out  *lo, int  *first, int  *genotypes);
void OutFragTree(struct tnode  *t, FILE  *ouf);
void CountTTree(struct tnode  *t, int  *num);
void FreeTTree(struct tnode  *p);
struct tnode * AddTree(struct tnode  *p, struct last_out  *lo);
void fragmen(char  *infile, char  *oufile, char  *directory,
    int  start, int  stop);

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

int main(void)
{   char  buf[80];
    int   start = 0, stop = 1;
    char  infile[13], oufile[13];

    sprintf(infile,"break.1"); sprintf(oufile,"fragment.run");
    sprintf(directory,"c:\\tierra\\td");

    printf("directory (default: %s) = ", directory);
    gets(buf);
    if (strlen(buf))
        sprintf(directory, "%s", buf);

    printf("input file (default: %s) = ", infile);
    gets(buf);
    if (strlen(buf))
        sprintf(infile, "%s", buf);

    printf("output file (default: %s) = ", oufile);
    gets(buf);
    if (strlen(buf))
        sprintf(oufile, "%s", buf);

    printf("start time (default: %d) = ", start);
    gets(buf);
    if (strlen(buf))
        sscanf(buf, "%d", &start);

    printf("stop time (default: %d) = ", stop);
    gets(buf);
    if (strlen(buf))
        sscanf(buf, "%d", &stop);

    if (temp_info_chk_f(infile, directory))
        fragmen(infile,oufile,directory,start,stop);
    return 1;
}

int temp_info_chk_f(char  *infile, char  *directory)
{   FILE  *inf;
    char  path[80], cpath[80];
    int  success;

    getcwd(cpath,79);
    success = chdir(directory);
    chdir(cpath);
    if(success)
    {   printf("directory %s not found\n", directory);
        return 0;
    }

    sprintf(path,"%s/%s", directory, infile);
    inf = fopen(path,"r");
    if(inf == NULL)
    {   printf("file %s not found\n", path);
        return 0;
    }
    fclose(inf);

    return 1;
}

void fragmen(char  *infile, char  *oufile, char  *directory,
    int  start, int  stop)
{   int  first = 1, not_yet = 1, species = 0;
    struct tnode  *troot = NULL;
    struct last_out  lo;
    long  time;
    Ulong c = 0;
    char  data[81], *obufr;
    FILE  *inf, *ouf;
    char   bifile[13];
    int    binum = 1;

    sscanf(infile,"%[^.]", bifile);
    sprintf(data,"%s/%s", directory, infile);
    inf = fopen(data,"r");
    if(inf == NULL)
    {   printf("input file %s not found\n", data);
        return ;
    }
    sprintf(data,"%s/%s", directory, oufile);
    ouf = fopen(data,"w");
    lo.ctime = lo.time = 0;
    genotypes = 1;
    obufr = (char  *) calloc(8192, sizeof(char));
    if(obufr == NULL)
    {   printf("calloc failure, exiting\n");
        exit(0);
    }
    setvbuf(ouf,obufr,_IOFBF,8192);
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
        if(!(c%1000LU))
            printf("Time = %5lu,%06lu  Coreleft = %6lu\r",
                lo.ctime, lo.time, coreleft());
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

int t_read(char  data[], struct last_out  *lo, int  *first, int  *genotypes)
{   struct last_out  ti;
    int    nargs;
    char   v2[9], v3[9], v4[9];

    sscanf(data,"%s", v2);
    if(!strcmp(v2,"num_sp")) return 0;
    nargs = sscanf(data,"%lx%s%s%s", &ti.time, v2, v3, v4);
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

void FreeTTree(struct tnode  *p)
{   if(p != NULL)
    {   FreeTTree(p->l);
        FreeTTree(p->r);
        free(p->g);
        free(p);
    }
}

struct tnode * AddTree(struct tnode  *p, struct last_out  *lo)
{   int  i, j, osize;
    struct pop_dat  *pd;

    if(p == NULL)
    {   if(lo->bd == 'd')
        {   printf("new node is a death, exiting\n");
            exit(0);
        }
        p = (struct tnode  *) calloc(1,sizeof(struct tnode));
        if(p == NULL)
        {   printf("calloc failure, exiting\n");
            exit(0);
        }
/*
        else
            printf("coreleft = %6lu\n", coreleft());
*/
        p->size = lo->size;
        p->sd.max = p->sd.end = 1;
        p->sd.min = p->sd.beg = 0;
        if(genotypes)
        {   i = Lbl2Int(lo->label);
            p->gsize = i + 1;
            p->g = (struct pop_dat  *)
                calloc(p->gsize,sizeof(struct pop_dat));
            if(p->g == NULL)
            {   printf("calloc failure, exiting\n");
                exit(0);
            }
/*
            else
                printf("coreleft = %6lu\n", coreleft());
*/
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
                {   printf("realloc failure, exiting\n");
                    exit(0);
                }
/*
                else
                    printf("coreleft = %6lu\n", coreleft());
*/
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
