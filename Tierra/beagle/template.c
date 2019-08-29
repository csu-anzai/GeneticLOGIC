/* template.c  27-3-92  template locator for the Beagle Explorer */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

extern FILE  *info;
HWND         whte;
TRANSACTION  *ptte;

int   min_temp_siz;
char  genepath[41], genefile[13], genotype[4];
struct use_template  *t;

int template(void)
{   int  i;
    static int  first = 1;
    char  t;
    HWND  whins;

    if(first)
    {   first = 0;
        sprintf(genepath,"c:\\tierra\\gb");
        sprintf(genefile,"0080.gen");
        sprintf(genotype,"aaa");
        min_temp_siz = 3;
    }
    while(1) {
    whte = wigen(2,4,4,58,FRDOUBLE,"templates");
    ptte = vpreload(4,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 3; i++) (*(ptte->def))[i].use_ext = TRUE;
    vdeffld(ptte,0,STRING,0,1, "genebank path =",-1,40,NULL,genepath,NO);
    vdeffld(ptte,1,STRING,1,1, "genefile name =",-1,12,NULL,genefile,NO);
    vdeffld(ptte,2,STRING,2,1, "genotype      =",-1, 3,NULL,genotype,NO);
    vdeffld(ptte,3,INTEGER,3,1,"min temp size =",-1, 3,NULL,&min_temp_siz,NO);
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whte,YES,YES);
    ptte->fld_valfn = temp_info_chk;
    if(DWSUCCESS == vread(ptte,whte,YES))
    {   vdelete(whte,NONE); vdelete(whins,NONE);
        for (t = 0; t < PLOIDY; t++)
            templat(t);
    }
    else
    {   vdelete(whte,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int temp_info_chk(TRANSACTION  *tp)
{   int     success;
    char    path[80];
    head_t  head;
    indx_t  *indx;

    if(tp->cur_fld == 0)
    {   getcwd(path,79);
        success = chdir(genepath);
        chdir(path);
        if(success)
        {   wierror(2,45,"directory not found");
            return FALSE;
        }
        sprintf(path,"%s/opcode.map", genepath);
        info = fopen(path,"r");
        if(info == NULL)
        {   wierror(2,45,"opcode.map not found");
            return FALSE;
        }
        fclose(info);
        return TRUE;
    }
    if(tp->cur_fld == 1)
    {   sprintf(path,"%s/%s", genepath, genefile);
        info = fopen(path,"r");
        if(info == NULL)
        {   wierror(2,45,"file not found");
            return FALSE;
        }
        fclose(info);
        return TRUE;
    }
    if(tp->cur_fld == 2)
    {   sprintf(path,"%s/%s", genepath, genefile);
        if (!(info = fopen(path, "rb")))
        {   wierror(2,45,"file not found");
            return FALSE;
        }
        head = read_head(info);
        indx = read_indx(info, &head);
        fclose(info);
        if (find_gen(indx, genotype, head.n) == head.n)
        {   wierror(2,45,"genotype not contained in file");
            farfree(indx);
            return FALSE;
        }
        farfree(indx);
    }
    return TRUE;
}

void templat(int  track)
{   int  c = 0, i, gsize, start = 0, site = 0, tail_size, array_size = 10;
    int  otnum = 6;
    FpInst  genome;
    FILE  *info;
    char  data[85], error[110];

    templ = (ArgInstDef  *) calloc(otnum, sizeof(ArgInstDef));
    Tnum = 0;
    sprintf(data,"%s/opcode.map", genepath);
    GetAMap(data);
/*
    info = fopen(data,"r");
    if(info == NULL)
        wierror(2,45,"opcode.map not found");
    fgets(data, 84, info);
    while (strlen(data) > 3)
    {   if (!GetAMap(data))
        {   sprintf(error, "bad IMapFile line: %s", data);
            wierror(2,10,error);
        }
        if (fgets(data, 84, info) == NULL)
            break ;
    }
    fclose(info);
    qsort(aid,INSTNUM,sizeof(ArgInstDef),aid_compare);
*/
    for (i = 0; i < INSTNUM; i++)
    {   if (   !strcmp(aid[i].mn, "jmp")  || !strcmp(aid[i].mn, "jmpb")
            || !strcmp(aid[i].mn, "call") || !strcmp(aid[i].mn, "adr")
            || !strcmp(aid[i].mn, "adrb") || !strcmp(aid[i].mn, "adrf"))
        {   Tnum++;
            if (Tnum > otnum)
            {   templ = (ArgInstDef  *) realloc(templ,
                    Tnum * sizeof(ArgInstDef));
                otnum = Tnum;
            } 
            templ[Tnum - 1] = aid[i];
        }
        if (!strcmp(aid[i].mn, "nop_0"))
            Nop0 = i;
        if (!strcmp(aid[i].mn, "nop_1"))
            Nop1 = i;
    }

    t = (struct use_template  *) calloc(10, sizeof(struct use_template));
    sscanf(genefile, "%d", &gsize);
    tail_size = gsize;
    genome = get_gentype(genepath, genefile, genotype);
    while(find_next_use_template(genome, start, tail_size, &site, track))
    {   if(c >= array_size)
        {   array_size += 10;
            t = (struct use_template  *)
                realloc(t, array_size * sizeof(struct use_template));
        }
        t[c].locus = site;
#if PLOIDY == 1
        inst_name((int)(genome + site)->inst, t[c].inst);
#else /* PLOIDY > 1 */
        inst_name((int)(genome + site)[track]->inst, t[c].inst);
#endif /* PLOIDY == 1 */
        t[c].size =
            get_template_size(genome + site + 1, gsize - site - 1, track);
        for(i = 1; i <= t[c].size; i++)
#if PLOIDY == 1
            t[c].template[i - 1] = inst_letter((genome+site+i)->inst);
#else /* PLOIDY > 1 */
            t[c].template[i - 1] = inst_letter((genome+site+i)[track]->inst);
#endif /* PLOIDY == 1 */
        t[c].template[t[c].size] = 0;
        start = site + t[c].size;
        tail_size = gsize - start;
        c++;
    }
    logical(c);
    free(t);
    free(genome);
}

void logical(int  c)
{   HWND      winl, whins;
    int       i, p = c - 1;

    winl = vcreat(c,40,NORML,YES);
    if(c > 21) { vwind(winl,21,40,0,0); p = 20; }
    else vwind(winl,c,40,0,0);
    vlocate(winl,2,8);
    vframe(winl,NORML,FRSINGLE);
    for(i = 0; i < c; i++)
        vatputf(winl, i, 2, "locus: %6d, %-6.6s %-15s",
            t[i].locus, t[i].inst, t[i].template); 
    visible(winl,YES,YES);
    whins = wiinv(23,31, "Esc to continue");
    scroll(p,winl);
    vdelete(whins,NONE);
    vdelete(winl,NONE); 
}
