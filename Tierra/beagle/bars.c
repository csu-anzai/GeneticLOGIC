/* bars.c  27-3-92  bar display for the Beagle Explorer */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

struct sbar  sb;
struct barst  bs = { 24., 16., 14., 4., 10., 10., 40., 8., 5., 79. } ;
struct species  sp[55];

int    num_sp, pop, inst, gent, genotypes;
int    update, view, species, barcolor, maxcolor, udisplay;
float  max_max, bxln;
double instructions;
char   xaxis[5], ifile[13];
HWND         whba, whinb;
TRANSACTION  *ptba;

extern char  directory[], info_file[];
extern FILE  *info;
extern struct last_out  lo;

int bars(void)
{   int  i, rows;
    static int    first = 1;
    HWND  whins;

    if(first)
    {   first = 0;
/*      strcpy(ifile,"tierra.run"); */
        strcpy(ifile,"break.1");
        bs.nbr = 24.;
        genotypes = 1;
        udisplay = 1;
        view = 0;
    }
    sprintf(xaxis,"inst");
    if(y_res == 480) rows = 7; else rows = 6;
    while(1) {
    whba = wigen(3,1,rows,54,FRDOUBLE,"bars");
    ptba = vpreload(rows,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i < rows; i++) (*(ptba->def))[i].use_ext = TRUE;
    (*(ptba->def))[3].refresh = ON;
    vdeffld(ptba,0,STRING,0,1, "directory  =",-1,39,NULL,directory,NO);
    vdeffld(ptba,1,STRING,1,1, "input file =",-1,12,NULL,ifile,NO);
    vdeffld(ptba,2,STRING,2,1, "x-axis (pop,inst)  =",-1,4,NULL,xaxis,NO);
    vdeffld(ptba,3,BOOLEAN,3,1,"separate genotypes =",-1,1,NULL,&genotypes,NO);
    vdeffld(ptba,4,BOOLEAN,4,1,"update display     =",-1,1,NULL,&udisplay,NO);
    vdeffld(ptba,5,BOOLEAN,5,1,"view run_info      =",-1,1,NULL,&view,NO);
    vdeffld(ptba,6,DOUBLE,6,1, "num bars (24,55)   =",-1,2,NULL, &bs.nbr,NO);
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whba,YES,YES);
    ptba->fld_valfn = run_info_chk2;
    ptba->fld_valid = run_info2;
    if(DWSUCCESS == vread(ptba,whba,YES))
    {   vdelete(whinb,whba); vdelete(whba,NONE); vdelete(whins,NONE);
        bebar();
    }
    else
    {   vdelete(whinb,whba); vdelete(whba,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int run_info_chk2(TRANSACTION  *tp)
{   char  path[80], dpath[80];
    int  success;
    FILE  *inf;

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
    {   sprintf(dpath,"%s/%s", directory, ifile);
        inf = fopen(dpath,"r");
        if(inf == NULL)
        {   wierror(2,45,"input file not found");
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 2)
    {   if(!strcmp(xaxis,"pop") || !strcmp(xaxis,"inst"))
            return TRUE;
        wierror(2,45,"pop or inst please");
        return FALSE;
    }
    if(tp->cur_fld == 6)
    {   if(bs.nbr <= 55)
            return TRUE;
        wierror(2,45,"value must be <= 55");
        return FALSE;
    }
    return TRUE;
}

void run_info2(TRANSACTION  *tp)
{   int     i, row_siz_phy, tnum_sp;
    static int  first = 1;
    char  data[81];

    if(!strcmp(xaxis,"pop")) { pop = 1; inst = 0; }
    if(!strcmp(xaxis,"inst")) { pop = 0; inst = 1; }
    if(tp->cur_fld > 3 || !view) return ;
    if(genotypes)
    {   sprintf(data,"%s/run_info.g",directory);
        info = fopen(data,"r");
        if(info == NULL)
        {   sprintf(data,"%s/run_info",directory);
            info = fopen(data,"r");
            if(info == NULL)
            {   wierror(2,45,"run_info file not found");
                return ;
            }
            else genotypes = 0;
        }
    }
    else
    {   sprintf(data,"%s/run_info",directory);
        info = fopen(data,"r");
        if(info == NULL)
        {   wierror(2,45,"run_info file not found");
            return ;
        }
    }
    barst_setup();
    species_array_setup(&tnum_sp);
    species = tnum_sp;
    fclose(info);
    qsort(sp, num_sp, sizeof(struct species), spcmp3);
    if(num_sp > 23) row_siz_phy = 24; else row_siz_phy = num_sp + 1;
    if(first) first = 0;
    else vdelete(whinb,whba);
    whinb = wigenlp(1,56,num_sp + 1,24,row_siz_phy,24,FRSINGLE,"run info");
    visible(whinb,YES,NO);
    vatputf(whinb,0,1,"species = %i", species);
    for(i = 0; i < num_sp; i++)
    {   vatputf(whinb,i + 1,1,"%5d", sp[i].size);
        if(genotypes) vatputf(whinb,i + 1,6,"%s", sp[i].label);
        if(pop) vatputf(whinb,i + 1,10,"%u", sp[i].max);
        if(inst) vatputf(whinb,i + 1,10,"%ld", sp[i].min);
    }
    visible(whba,YES,YES);
}

void species_array_setup(int  *tnum_sp) /* update the species array */
{   char    data[100];
    int     i, species;

    for(i = 0; i < bs.nbr; i++)
    {   sp[i].size = sp[i].num = sp[i].max = sp[i].xpm = 0;
        sp[i].min = (long) 0;
        sprintf(sp[i].label,"---");
    }
    if(udisplay) fgets(data,99,info);
    else get_total(data);
    sscanf(data,"%*s%*s%d%*s%*s%lu%*s%*s%lu%*s%*s%d",
        &species, &lo.ctime, &lo.time, &update);
    *tnum_sp = species;
    fgets(data,99,info);
    if(species > bs.nbr) num_sp = bs.nbr; else num_sp = species;
    if(species > 55) species = 55;
    for(i = 0; i < species; i++)
    {   fgets(data,99,info);
        sscanf(data,"%d%s%*d%d%*d%d",
            &sp[i].size, sp[i].label, &sp[i].max, &sp[i].num);
        sp[i].min = (long) sp[i].size * (long) sp[i].max;
    }
    max_max = 0.;
    for(i = 0; i < num_sp; i++)
    {   if(pop) if(sp[i].max > max_max) max_max = (float) sp[i].max;
        if(inst) if(sp[i].min > max_max) max_max = (float) sp[i].min;
    }
}

void get_total(char  *data) /* read to total update at end of run_info file */
{   char  word[80];

    do
    {   fgets(data,99,info);
        sscanf(data,"%s%*s%*d%*s%*s%*lu%*s%*s%*lu%*s%*s%d", word, &update);
    }   while(strcmp(word,"num_sp") || update);
}

void bars_update(void) /* this sets up all bars, after a species list update */
{   int  i, xp;

    for(i = 0; i < num_sp; i++)
    {   if(sp[i].num <= 0) continue;
        if(pop) xp = (int) bs.bas + bxln * sp[i].num;
        if(inst) xp = (int) bs.bas + bxln * sp[i].num * sp[i].size;
        sb.top    = i * (bs.bwd + bs.sbb) + bs.thd;
        sb.bottom = sb.top + bs.bwd - 1;
        setfillstyle(SOLID_FILL,barcolor);
        sb.left   = sp[i].xpo;
        sb.right  = xp - 1;
        bar(sb.left, sb.top, sb.right, sb.bottom);
        sp[i].xpo = xp;
        if(xp > sp[i].xpm)
        {   sp[i].xpm = xp;
            setfillstyle(SOLID_FILL,maxcolor);
            bar(xp, sb.top, xp, sb.bottom);
            setfillstyle(SOLID_FILL,barcolor);
        }
    }
}

void barst_setup(void) /* this sets up the bars structure */
{   if(y_res == 480 && bs.nbr <= 24)
    {   bs.csz = 16.; bs.bwd = 14.; bs.sbb = 4.; bs.lbd = 10.;
        bs.tbd = 10.; bs.thd = 40.; bs.inb = 5.;
        if(genotypes) bs.nge = 3.; else bs.nge = 0.;
    }
    if(y_res == 480 && bs.nbr > 24)
    {   bs.csz = 8.; bs.bwd = 7.;  bs.sbb = 1.; bs.lbd = 10.;
        bs.tbd = 5.; bs.thd = 30.; bs.inb = 5.;
        if(genotypes) bs.nge = 3.; else bs.nge = 0.;
    }
    if(y_res == 200)
    {   bs.csz = 8.; bs.bwd = 7.;  bs.sbb = 1.; bs.lbd = 10.;
        bs.tbd = 5.; bs.thd = 15.; bs.inb = 5.;
        if(genotypes) bs.nge = 3.; else bs.nge = 0.;
    }
}

int spcmp2(const void  *s1, const void  *s2)
{   struct species  *sp1 = s1, *sp2 = s2;

    if(sp2->size > sp1->size) return 1;
    if(sp2->size < sp1->size) return -1;
    if(sp2->size == sp1->size)
    {   if(!strcmp(sp2->label,"---")) return -1;
        if(!strcmp(sp1->label,"---")) return 1;
        return -strcmp(sp2->label,sp1->label);
    }
    return 0;
}

int spcmp3(const void  *s1, const void  *s2)
{   struct species  *sp1 = s1, *sp2 = s2;

    if(pop) return sp2->max - sp1->max;
    if(inst)
    {   if(sp2->min > sp1->min) return 1;
        if(sp2->min < sp1->min) return -1;
        if(sp2->min == sp1->min) return 0;
    }
    return 0;
}

void do_bupdate(void) /* this updates the species list and scales */
{   char  data[81];
    int  i, tnum_sp;

    species_array_setup(&tnum_sp);
    qsort(sp, num_sp, sizeof(struct species), spcmp2);
    itoa(sp[0].size,data,10);
    bs.nsz = strlen(data);
    bs.nch = bs.nsz + bs.nge;
    bs.bas = bs.lbd + (bs.nch * bs.csz) + bs.inb;
    for(i = 0; i < bs.nbr; i++)
        sp[i].xpo = (int) bs.bas;
    if(y_res == 480) settextstyle(0,0,2);
    if(y_res == 200) settextstyle(0,0,1);
    cleardevice();
    sprintf(data,"T =");
    outtextxy(bs.lbd, bs.tbd, data);
    time_update();
    if(pop) sprintf(data,"N_max = %i", (int) max_max);
    if(inst) sprintf(data,"I_max = %li", (long) max_max);
    outtextxy(350, bs.tbd, data);
    if(y_res == 480 && bs.nbr > 24) settextstyle(0,0,1);
    for(i = 0; i < num_sp; i++) /* this makes the list of size labels */
    {   if(genotypes) sprintf(data,"%*i%s",
            (int) bs.nsz, sp[i].size, sp[i].label);
        else sprintf(data,"%*i", (int) bs.nsz, sp[i].size);
        outtextxy(bs.lbd, i * (bs.bwd + bs.sbb) + bs.thd, data);
    }
    bxln = (x_res - 4. - bs.bas) / max_max;
}

void time_update(void) /* this updates the time display */
{   int  csz, bwd;
    char  data[81];

    if(y_res == 480)
    {   settextstyle(0,0,2);
        csz = 16; bwd = 14;
    }
    else { csz = 8; bwd = 7; }
    setfillstyle(SOLID_FILL,0);
    sb.left = (4 * csz) + bs.lbd;
    sb.top = bs.tbd;
    sb.right = (4 * csz) + sb.left;
    sb.bottom = sb.top + bwd - 1;
    bar(sb.left, sb.top, sb.right, sb.bottom);
    sprintf(data,"%i", (int) lo.ctime);
    outtextxy(sb.left, sb.top, data);
}

void bebar(void)
{   Ulong  otime = 0;
    Uint   c;
    FILE   *inf;
    int    i, in, xp, found, firstr = 1, first = 1, upcount, tnum_sp;
    int    binum = 1;
    char   data[81];
    char   bifile[13];

    if(genotypes)
    {   sprintf(data,"%s/run_info.g",directory);
        info = fopen(data,"r");
        if(info == NULL)
        {   sprintf(data,"%s/run_info",directory);
            info = fopen(data,"r");
            if(info == NULL)
            {   wierror(2,45,"run_info file not found");
                return ;
            }
            else genotypes = 0;
        }
    }
    else
    {   sprintf(data,"%s/run_info",directory);
        info = fopen(data,"r");
        if(info == NULL)
        {   wierror(2,45,"run_info file not found");
            return ;
        }
    }
    sscanf(ifile,"%[^.]", bifile);
    sprintf(data,"%s/%s", directory, ifile);
    inf = fopen(data,"r");
    if(inf == NULL)
    {   wierror(2,45,"run file not found");
        return ;
    }
    barst_setup();
    setgraphmode(graphics_mode);
    maxcolor = barcolor = getmaxcolor();
    if(barcolor > 1) barcolor--;
    if(y_res == 480) settextstyle(0,0,2);
    if(y_res == 200) settextstyle(0,0,1);
    sprintf(data,"T = 0");
    outtextxy(bs.lbd, bs.tbd, data);
    do_bupdate();
    upcount = update;
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
        if(!t_read(data, &lo, &first, &gent))
        {   sscanf(data,"%*s%*s%d%*s%*s%lu%*s%*s%lu",
                &tnum_sp, &lo.ctime, &lo.time);
            for(i = 0; i < tnum_sp; i++) fgets(data,80,inf);
            while(udisplay && lo.ctime >= upcount) upcount += update;
            if(firstr)
                firstr = 0;
            else if(udisplay)
            {   do_bupdate();
                bars_update();
            }
            continue;
        }
        firstr = 0;
        if(lo.ctime > otime)         /* this updates the time */
        {   time_update();
            otime = lo.ctime;
        }
        if(udisplay && lo.ctime >= upcount)
        {   upcount += update;
            do_bupdate();
            bars_update();
        }
        found = 0;
        for(i = 0; i < num_sp; i++)
            if(genotypes)
            {   if(sp[i].size == lo.size && !strcmp(sp[i].label,lo.label))
                {   in = i; found = 1; break; }
            }
            else
            if(sp[i].size == lo.size) { in = i; found = 1; break; }
        if(found)
        {   if(lo.bd == 'b') sp[in].num++;
            if(lo.bd == 'd') sp[in].num--;
            if(pop) xp = (int) bs.bas + bxln * sp[in].num;
            if(inst) xp = (int) bs.bas + bxln * sp[in].num * sp[in].size;
            sb.top    = in * (bs.bwd + bs.sbb) + bs.thd;
            sb.bottom = sb.top + bs.bwd - 1;
            if(xp > sp[in].xpo) /* the bar gets larger */
            {   setfillstyle(SOLID_FILL,barcolor);
                sb.left   = sp[in].xpo;
                sb.right  = xp - 1;
                bar(sb.left, sb.top, sb.right, sb.bottom);
                sp[in].xpo = xp;
                if(xp > sp[in].xpm)
                {   sp[in].xpm = xp;
                    setfillstyle(SOLID_FILL,maxcolor);
                    bar(xp, sb.top, xp, sb.bottom);
                    setfillstyle(SOLID_FILL,barcolor);
                }
            }
            if(xp < sp[in].xpo) /* the bar gets smaller */
            {   setfillstyle(SOLID_FILL,0);
                sb.left   = xp;
                sb.right  = sp[in].xpo - 1;
                bar(sb.left, sb.top, sb.right, sb.bottom);
                sp[in].xpo = xp;
            }
        }
        if(gfkbhit())
        {   c = getkey();
            if(c == 'p' || c == 'P') getkey();  /* P or p key */
            if(c == ESC) break ;                /* Esc key */
            if(c == 'g' || c == 'G')            /* G or g key */
                DumpScreen2Gif("image.gif",VGA,VGAHI,VGAHI,VGAHI);
        }
    }
    fclose(inf);
    fclose(info);
    if(scr)
    {   vbeep();
        while(1)
        {   if(gfkbhit())
            {   c = getkey();
                if(c == ESC) break ;                /* Esc key */
                if(c == 'g' || c == 'G')            /* G or g key */
                    DumpScreen2Gif("image.gif",VGA,VGAHI,VGAHI,VGAHI);
            }
        }
        restorecrtmode();
    }
}
