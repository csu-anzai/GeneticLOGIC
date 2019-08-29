/* probe.c  27-3-92  genome probe for the Beagle Explorer */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

HWND         whpr, wili, wima;
TRANSACTION  *ptpr;

char  genomefile[13], genomepath[51], genome[4];
char  probefile[13], probepath[51], proben[4];
struct geneprobe  *p;
int  asize, gsize, psize, num_match;
Instruction  *pgene, *pprobe;

int probe(void)
{   int  i;
    static int  first = 1;
    char  t;
    HWND  whins;

    if(first)
    {   first = 0;
        sprintf(genomepath,"c:\\tierra\\gb");
        sprintf(genomefile,"0080.gen");
        sprintf(genome,"aaa");
        sprintf(probepath,"c:\\tierra\\gb");
        sprintf(probefile,"0045.gen");
    	sprintf(proben,"aaa");
    }
    while(1) {
    whpr = vcreat(6,66,REVNORML,YES);
    vwind(whpr,6,66,0,0);
    vlocate(whpr,2,4);
    vframe(whpr,REVNORML,FRDOUBLE);
    vtitle(whpr,REVNORML,"probe");
    ptpr = vpreload(6,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 5; i++) (*(ptpr->def))[i].use_ext = TRUE;
    vdeffld(ptpr,0,STRING,0,1,"genome path =",-1,50,NULL,genomepath,NO);
    vdeffld(ptpr,1,STRING,1,1,"genome file =",-1,12,NULL,genomefile,NO);
    vdeffld(ptpr,2,STRING,2,1,"genome      =",-1,3,NULL,genome,NO);
    vdeffld(ptpr,3,STRING,3,1,"probe path =", -1,50,NULL,probepath,NO);
    vdeffld(ptpr,4,STRING,4,1,"probe file =", -1,12,NULL,probefile,NO);
    vdeffld(ptpr,5,STRING,5,1,"probe      =", -1,3,NULL,proben,NO);
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whpr,YES,YES);
    ptpr->fld_valfn = prob_info_chk;
    if(DWSUCCESS == vread(ptpr,whpr,YES))
    {   vdelete(whpr,NONE); vdelete(whins,NONE);
        for (t = 0; t < PLOIDY; t++)
            prob(t);
    }
    else
    {   vdelete(whpr,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int prob_info_chk(TRANSACTION  *tp)
{   int  success;
    char  path[80];
    head_t  head;
    indx_t  *indx;
    FILE  *inf;

    if(tp->cur_fld == 0)
    {   getcwd(path,79);
        success = chdir(genomepath);
        chdir(path);
        if(success)
        {   wierror(2,45,"genomepath not found");
            return FALSE;
        }
        sprintf(path,"%s/opcode.map", genomepath);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"opcode.map not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 1)
    {   sprintf(path,"%s/%s", genomepath, genomefile);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"genomefile not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 2)
    {   sprintf(path,"%s/%s", genomepath, genomefile);
	if (!(inf = fopen(path, "rb")))
        {   wierror(2,45,"file not found");
            return FALSE;
	}
	head = read_head(inf);
	indx = read_indx(inf, &head);
        fclose(inf);
        if (find_gen(indx, genome, head.n) == head.n)
        {   wierror(2,35,"genotype not contained in genomefile");
            farfree(indx);
            return FALSE;
        }
        farfree(indx);
        return TRUE;
    }
    if(tp->cur_fld == 3)
    {   getcwd(path,79);
        success = chdir(probepath);
        chdir(path);
        if(success)
        {   wierror(2,45,"probepath not found");
            return FALSE;
        }
        sprintf(path,"%s/opcode.map", probepath);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"opcode.map not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 4)
    {   sprintf(path,"%s/%s", probepath, probefile);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   wierror(2,45,"probefile not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 5)
    {   sprintf(path,"%s/%s", probepath, probefile);
	if (!(inf = fopen(path, "rb")))
        {   wierror(2,45,"file not found");
            return FALSE;
	}
	head = read_head(inf);
	indx = read_indx(inf, &head);
		fclose(inf);
        if (find_gen(indx, proben, head.n) == head.n)
        {   wierror(2,35,"genotype not contained in probefile");
            farfree(indx);
            return FALSE;
        }
        farfree(indx);
    }
    return TRUE;
}

void prob(char  track)
{   char  tname[13];
    int  tsize, i;
    Instruction  *tgene;
    FILE  *info;
    char  data[85], error[110];
    ArgInstDef  *paid;

    paid = (ArgInstDef  *) calloc(INSTNUM, sizeof(ArgInstDef));
    sprintf(data,"%s/opcode.map", probepath);
    GetAMap(data);
    for (i = 0; i < INSTNUM; i++)
        paid[i] = aid[i];
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
    for (i = 0; i < INSTNUM; i++)
        paid[i] = aid[i];
    qsort(paid,INSTNUM,sizeof(ArgInstDef),aid_compare);
*/

    sprintf(data,"%s/opcode.map", genomepath);
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
        if (paid[i].op != aid[i].op || strcmp(paid[i].mn,aid[i].mn))
        {   wierror(2,45,"incompatible opcode.maps");
            break;
        }
    free(paid);

    sscanf(genomefile, "%d", &gsize);
    pgene = get_gentype(genomepath, genomefile, genome);
    if(pgene == NULL)
    {   wierror(2,45,"genome file not found");
        return ;
    }
    sscanf(probefile, "%d", &psize);
    pprobe = get_gentype(probepath, probefile, proben);
    if(pprobe == NULL)
    {   wierror(2,45,"probe file not found");
        return ;
    }
    if(psize > gsize)
    {   tgene = pgene;
        pgene = pprobe;
        pprobe = tgene;
        sprintf(tname,"%s", genome);
        sprintf(genome,"%s", proben);
        sprintf(proben,"%s", tname);
        tsize = gsize;
        gsize = psize;
        psize = tsize;
    }
    asize = 1 + ((2 * (gsize + psize)) / psize);
    p = (struct geneprobe  *) calloc(asize, sizeof(struct geneprobe));
    probeslide(pprobe,psize,pgene,gsize,track);
    qsort(p, asize, sizeof(struct geneprobe), prcmp);
    comp_ins_del();
    plogical(asize);
    instlist();
    pmatch(track);
    free(pgene);
    free(pprobe);
    free(p);
}

void pmatch(char  track)
{   HWND  whins;
    int   i, r = -3, wr, page;
    char  title[78];

    wr = gsize / 76;
    if(gsize % 76) wr++; wr *= 3;  page = wr;
    wima = vcreat(wr - 1,78,REVNORML,YES);
    if(wr > 17) { vwind(wima,17,78,0,0); page = 16; }
    else vwind(wima,wr - 1,78,0,0);
    vlocate(wima,7,1);
    vframe(wima,REVNORML,FRSINGLE);
    sprintf(title,
        "genome-to-probe match:  genome (top) - %04d%s    probe - %04d%s",
        gsize, genome, psize, proben);
    vtitle(wima,REVNORML,title);
    visible(wima,YES,YES);
    for(i = 0; i < gsize; i++)
    {   if(!(i % 76)) { r += 3; lcurset(wima,r,1); }
#if PLOIDY == 1
        vrpc(NORML,inst_letter((int)(pgene + i)->inst));
#else /* PLOIDY > 1 */
        vrpc(NORML,inst_letter((int)(pgene + i)[track]->inst));
#endif /* PLOIDY == 1 */
    }
    qsort(p, num_match, sizeof(struct geneprobe), orcmp);
    unlap(track);
    if(wr > 17)
         whins = wint(24,18,
             "Instructions: \x1e\x1f to scroll; Esc to continue");
    else
         whins = wint(6 + wr,25,"Instructions: Esc to continue");
    lineup(track);
    scroll(page,wima);
    vdelete(wima,NONE);
    vdelete(wili,NONE);
    vdelete(whins,NONE);
}

void plogical(int  size)
{   HWND  winl, winu, whins;
    int   i, page = size - 1;
    TRANSACTION  *ptnu;

    num_match = gsize / 80;
    if(gsize % 80 > 40) num_match++;
    if(gsize <= 40) num_match = 1;
    if(size > 400) size = 400;
    winl = vcreat(size,55,NORML,YES);
    if(size > 21) { vwind(winl,21,55,0,0); page = 20; }
    else vwind(winl,size,55,0,0);
    vlocate(winl,2,8);
    vframe(winl,NORML,FRSINGLE);
    for(i = 0; i < size; i++)
        vatputf(winl,i,3,"position: %4d   fit: %3d   diff: %2d   idpos: %3d",
            p[i].position, p[i].fit, p[i].diff, p[i].idpos);
    if(size > 21)
         whins = wiinv(23,25,"\x1e\x1f to scroll; Esc to continue");
    else
         whins = wiinv(23,32,"Esc to continue");
    visible(winl,YES,YES);
    scroll(page,winl);
    vdelete(whins,NONE);
    winu = wigen(2,46,1,30,FRDOUBLE,"number of matches to use");
    ptnu = vpreload(5,0,NULLF,NULLF,NULLF,NULLF);
    (*(ptnu->def))[0].use_ext = TRUE;
    vdeffld(ptnu,0,INTEGER,0,1,"number of matches =",-1,4,NULL,&num_match,NO);
    whins = wiinv(23,28,"Ctrl-Enter to continue");
    visible(winu,YES,YES);
    vread(ptnu,winu,YES);
    vdelete(winu,NONE);
    vdelete(winl,NONE); 
    vdelete(whins,NONE);
}

int orcmp(const void  *g1, const void  *g2)
{   struct geneprobe  *ge1 = g1, *ge2 = g2;

    return ge1->position - ge2->position;
}

int prcmp(const void  *g1, const void  *g2)
{   struct geneprobe  *ge1 = g1, *ge2 = g2;

    return ge2->fit - ge1->fit;
}

int rprcmp(const void  *g1, const void  *g2)
{   struct geneprobe  *ge1 = g1, *ge2 = g2;

    return ge1->fit - ge2->fit;
}
