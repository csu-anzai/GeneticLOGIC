/* genework.c  27-3-92  */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include  "beagle.h"
#include  "externb.h"

extern FILE  *info;
extern int  asize, psize, gsize, num_match, min_temp_siz;
extern struct geneprobe  *p;
extern Instruction  *pgene, *pprobe;
extern HWND  wima, wili;
struct insdel  *id;

int  find_next_use_template(Instruction  *pgene, int  start, int  tail_size,
     int  *site, char  track)
{   int  i;

    for(i = 0; i < tail_size - min_temp_siz; i++)
        if(is_use_template(pgene + start + i, track))
        {   *site = start + i;
            return 1;
        }
    return 0;
}

int  is_use_template(Instruction  *locus, char  track)
{   int  i, j, match = 0;

#if PLOIDY == 1
    for (j = 0; j < Tnum; j++)
    {   if (locus->inst == templ[j].op)
        {   match = 1;
            break ;
        }
    }
    if (!match)
        return 0;
    for(i = 1; i <= min_temp_siz; i++)
        if((locus + i)->inst != Nop0 && (locus + i)->inst != Nop1)
            return 0;
    return 1;
#else /* PLOIDY > 1 */
    for (j = 0; j < Tnum; j++)
    {   if (locus[track]->inst == templ[j].op)
        {   match = 1;
            break ;
        }
    }
    if (!match)
        return 0;
    for(i = 1; i <= min_temp_siz; i++)
        if((locus + i)[track]->inst != Nop0
            && (locus + i)[track]->inst != Nop1)
            return 0;
    return 1;
#endif /* PLOIDY == 1 */
}

int  get_template_size(Instruction  *locus, int  tail_size, char  track)
{   int  i;

    for(i = 0; i < tail_size; i++)
#if PLOIDY == 1
        if((locus + i)->inst != Nop0 && (locus + i)->inst != Nop1)
#else /* PLOIDY > 1 */
        if((locus + i)[track]->inst != Nop0
            && (locus + i)[track]->inst != Nop1)
#endif /* PLOIDY == 1 */
            return i;
    return tail_size;
}

char * inst_name(int  inst, char  *name)
{   sprintf(name,aid[inst].mn);
    return name;
}

char inst_letter(int  inst)
{   if(inst < 10) return (char) inst + 48;
    return (char) inst + 87;
}

void unlap(char  track)
{   int  c, i, j, m, diff, fit;
    struct insdel  f = {0,0,0,0,0};

    for(m = 0; m < num_match; m++)
    {   p[m].start = 0; p[m].stop = psize; }
    for(m = 0; m < num_match - 1; m++)
    {   diff = p[m + 1].position - p[m].position;
        if(p[m].position + psize > p[m + 1].position)
        {   f.idpos = psize; f.fit = 0;
            for(c = diff; c < psize; c++)
            {   fit = 0;
                for(i = diff, j = 0; i < psize; i++, j++)
                {   if(i < c)
                    {   if((pprobe+i)->inst == (pgene+p[m].position+i)->inst)
                            fit++;
                    }
                    else
                    {   if((pprobe+j)->inst == (pgene+p[m+1].position+j)->inst)
                            fit++;
                    }
                }
                if(fit > f.fit)
                {   f.idpos = c;
                    f.fit = fit;
                }
            }
            p[m].stop = f.idpos;
            p[m + 1].start = f.idpos - diff;
        }
    }
}

void lineup(char  track)
{   int  c, i, j, m, r, pos, start, stop, diff;

    for(m = 0; m < num_match; m++)
    {   r = 1;
        diff = p[m].diff;
        if(p[m].position < 0) pos = 0;
        else pos = p[m].position;
        pos += p[m].start;
        r += 3 * (pos / 76);
        c = 1 + pos % 76;
        lcurset(wima,r,c);
        if(p[m].position < 0) start = -p[m].position;
        else start = 0;
        if(p[m].start > start) start = p[m].start;
        if(p[m].position + diff > gsize - psize)
            stop = gsize - p[m].position;
        else stop = psize + diff;
        if(p[m].stop < stop) stop = p[m].stop;
        for(i = j = start; i < stop; i++, j++)
        {   if(!(c % 77)) { c = 1; r += 3; lcurset(wima,r,c); }
            if(i > p[m].idpos && i <= p[m].idpos + diff)
            {   vpc('+'); vatpa(r,c,NORML); }
            else
            {
#if PLOIDY == 1
                vpc(inst_letter((int)(pprobe + j)->inst));
                if((pprobe + j)->inst ==
                    (pgene + i + p[m].position)->inst)
#else /* PLOIDY > 1 */
                vpc(inst_letter((int)(pprobe + j)[track]->inst));
                if((pprobe + j)[track]->inst ==
                    (pgene + i + p[m].position)[track]->inst)
#endif /* PLOIDY */
                {   vatpa(r - 1,c,REVNORML);
                    vatpa(r,c,REVNORML);
                }
                else vatpa(r,c,NORML);
            }
            if(i == p[m].idpos) { j -= diff; }
            c++;
        }
    }
}

void comp_ins_del(void)
{   int  i, j, id_count = 0, array_size = 10;

    id = (struct insdel  *) calloc(array_size, sizeof(struct insdel));
    for(i = 0; i < asize - 1; i++)
    {   for(j = i + 1; j < asize; j++)
        {   if(abs(p[i].position - p[j].position) < 2 * psize / 3)
            {   id_count++;
                if(id_count > array_size)
                {   array_size += 10;
                    id = (struct insdel  *)
                        realloc(id, array_size * sizeof(struct insdel));
                }
                id[id_count - 1] = ins_del(i,j);
            }
        }
    }
    id_count = clear_conflicts(id_count);
    judgement(id_count);
    free(id);
}

int clear_conflicts(int  num)
{   int  i, j, lnum = num;

    qsort(id, num, sizeof(struct insdel), idcmp);
    for(i = num - 1; i > 0; i--)
    {   for(j = i - 1; j >= 0; j--)
        {   if(id[i].fit && id[j].fit && (id[i].first == id[j].first ||
                id[i].first == id[j].second || id[i].second == id[j].first ||
                id[i].second == id[j].second))
            {   id[i].first = id[i].second = id[i].diff = id[i].idpos =
                    id[i].fit = 0;
                lnum--; break;
            }
        }
    }
    qsort(id, num, sizeof(struct insdel), idcmp);
    return lnum;
}

int idcmp(const void  *i1, const void  *i2)
{   struct insdel  *id1 = i1, *id2 = i2;

    return id2->fit - id1->fit;
}

void judgement(int  num)
{   int  i, lasize = asize, bfit, wfit, tfit;

    for(i = 0; i < num; i++)
    {   bfit = p[id[i].first].fit;
        wfit = p[id[i].second].fit;
        if(wfit > bfit)
        {   tfit = wfit;
            wfit = bfit;  /* wfit = worse fit */
            bfit = tfit;  /* bfit = better fit */
        }
        if(id[i].fit > bfit && id[i].fit > wfit * 1.2 &&
            id[i].fit > psize * .4)
        {   p[id[i].second].fit = 0;
            p[id[i].second].position = 0;
            p[id[i].first].fit = id[i].fit;
            p[id[i].first].diff = id[i].diff;
            p[id[i].first].idpos = id[i].idpos;
            lasize--;
        }
    }
    qsort(p, asize, sizeof(struct geneprobe), prcmp);
    asize = lasize;
}

struct insdel ins_del(int  i1, int  i2)
{   int  p1, p2;
    struct insdel  ifit, dfit, rfit;

    p1 = p[i1].position; p2 = p[i2].position;
    if(p1 < p2)
    {   ifit = insert(p1,p2);
        dfit = delete(p2,p1);
    }
    else
    {   ifit = insert(p2,p1);
        dfit = delete(p1,p2);
    }
    if(ifit.fit > dfit.fit) rfit = ifit;
    else rfit = dfit;
    if(rfit.first == p[i1].position) { rfit.first = i1; rfit.second = i2; }
    else { rfit.first = i2; rfit.second = i1; }
    return rfit;
}

struct insdel insert(int  p1, int  p2)
{   int  diff, i, j, fit, start, stop;
    struct insdel  f = {0,0,0,0,0};

    diff = p2 - p1;
    if(p1 < 0) start = -p1;
    else start = 0;
    if(p2 > gsize - psize) stop = gsize - p2;
    else stop = psize;
    for(j = start + 1; j < stop - 1; j++)
    {   fit = 0;
        for(i = start; i < stop; i++)
        {   if(i < j)
            {   if((pprobe + i)->inst == (pgene + p1 + i)->inst)
                    fit++;
            }
            else
            {   if((pprobe + i)->inst == (pgene + p2 + i)->inst)
                    fit++;
            }
        }
        if(fit > f.fit)
        {   f.idpos = j;
            f.fit = fit;
        }
    }
    f.first = p1;
    f.second = p2;
    f.diff = diff;
    return f;
}

struct insdel delete(int  p1, int  p2)
{   int  diff, i, j, fit, start, stop;
    struct insdel  f = {0,0,0,0,0};

    diff = p1 - p2;
    if(p1 < 0) start = -p1;
    else start = 0;
    stop = psize - diff;
    if(p1 + stop > gsize)
        stop = gsize - p1;
    for(j = start + 1; j < stop - 1; j++)
    {   fit = 0;
        for(i = start; i < stop; i++)
        {   if(i < j)
            {   if((pprobe + i)->inst == (pgene + p1 + i)->inst)
                    fit++;
            }
            else
            {   if((pprobe + i + diff)->inst == (pgene + p1 + i)->inst)
                    fit++;
            }
        }
        if(fit > f.fit)
        {   f.idpos = j - 1;
            f.fit = fit;
        }
    }
    f.first = p1;
    f.second = p2;
    f.diff = -diff;
    return f;
}

int probefit(int  position, Instruction  *pprobe,
        int  psize, Instruction  *pgene, int  gsize, char  track)
{   int  i, fit = 0, start, stop;

    if(psize > gsize)
    {   printf("probefit error: probe larger than genome\n");
        vexit(0);
    }
    if(position < 0) { start = -position; stop = psize; }
    if(position >= 0 && position < gsize - psize + 1)
    {   start = 0; stop = psize; }
    if(position > gsize - psize) { start = 0; stop = gsize - position; }
    for(i = start; i < stop; i++)
#if PLOIDY == 1
        if((pprobe + i)->inst == (pgene + position + i)->inst)
#else /* PLOIDY > 1 */
        if((pprobe + i)[track]->inst == (pgene + position + i)[track]->inst)
#endif /* PLOIDY == 1 */
            fit++;
    return fit;
}

void probeslide(Instruction  *pprobe, int  psize,
    Instruction  *pgene, int  gsize, char  track)
{   int  fit, pos, i;

    pos = 1 - psize;
    for(i = 0; i < gsize + psize - 1; i++)
    {   fit = probefit(pos,pprobe,psize,pgene,gsize,track);
        if(fit > p[0].fit)
        {   p[0].position = pos;
            p[0].fit = fit;
            qsort(p, asize, sizeof(struct geneprobe), rprcmp);
        }
        pos++;
    }
}

void instlist(void)
{   int  i;
    char  name[10], tmp[10];

    wili = wigen(1,1,4,78,FRDOUBLE,"instruction list");
    vcurrent(wili);
    lcurset(wili,0,3);
    for(i = 0; i < 8; i++)
    {   vpc(inst_letter(i));
        vpc(':');
        sprintf(name,"%-6.6s", inst_name(i,tmp));
        vps(name);
        vpc(' ');
    }
    lcurset(wili,1,3);
    for(i = 8; i < 16; i++)
    {   vpc(inst_letter(i));
        vpc(':');
        sprintf(name,"%-6.6s", inst_name(i,tmp));
        vps(name);
        vpc(' ');
    }
    lcurset(wili,2,3);
    for(i = 16; i < 24; i++)
    {   vpc(inst_letter(i));
        vpc(':');
        sprintf(name,"%-6.6s", inst_name(i,tmp));
        vps(name);
        vpc(' ');
    }
    lcurset(wili,3,3);
    for(i = 24; i < 32; i++)
    {   vpc(inst_letter(i));
        vpc(':');
        sprintf(name,"%-6.6s", inst_name(i,tmp));
        vps(name);
        vpc(' ');
    }
}

FpInst get_gentype(char  genepath[], char  genefile[], char  genotype[])
{   struct g_list *g;
    FILE    *fp;
    head_t  head;
    indx_t  *indx;
    char    path[90];
    I16s    n;
    FpInst  genome;

    g = (struct g_list  *) calloc(1, sizeof(struct g_list));
    sprintf(path, "%s/%s", genepath, genefile);
    fp = fopen(path, "rb");
    head = read_head(fp);
    indx = read_indx(fp, &head);
    n = find_gen(indx, genotype, head.n);
    g = get_gen(fp, &head, &indx[n], n);
    genome = g->genome;
    fclose(fp);
    farfree(indx);
    if (g)
    {   if (g->gbits)
            free(g->gbits);
        free(g);
    }
    return genome;
}
