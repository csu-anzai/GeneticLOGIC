/* trace.c  27-3-92  trace display for the Beagle Explorer */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

extern struct barst    bs;
extern struct sbar     sb;
extern struct species  sp[];
extern struct last_out  lo;
extern char  directory[], info_file[], ifile[];
extern int  pop, inst, num_sp, genotypes, gent, udisplay;
extern float  xln, yln, xo, yo;

char  axes[5], lab_x[4], lab_y[4];
int   siz_x, siz_y;
char  gen_x[9], gen_y[9];
float  shrink;
FILE  *info;
HWND         whbe, whint;
TRANSACTION  *ptbe;
struct tracest  tr;

int trace(void)
{   int  i;
    static int  first = 1;
    char  sshrink[5];
    HWND  whins;

    if(first)
    {   first = pop = 0; genotypes = inst = 1;
        xnvf = -.05; xxvf = 1.05; ynvf = -.05; yxvf = 1.1; shrink = 1.0;
        siz_x = 80; siz_y = 45; sprintf(lab_x,"aaa"); sprintf(lab_y,"aaa");
        sprintf(gen_x,"80aaa"); sprintf(gen_y,"45aaa");
/*      sprintf(ifile,"tierra.run"); */
        sprintf(ifile,"break.1");
    }
    sprintf(sshrink,"%g", shrink);
    udisplay = 0;
    sprintf(axes,"pop");
    while(1) {
    whbe = wigen(3,1,7,55,FRDOUBLE,"trace");
    ptbe = vpreload(7,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 6; i++) (*(ptbe->def))[i].use_ext = TRUE;
    (*(ptbe->def))[2].refresh = (*(ptbe->def))[3].refresh =
    (*(ptbe->def))[6].refresh = ON;
    vdeffld(ptbe,0,STRING,0,1, "directory  =",-1,40,NULL,directory,NO);
    vdeffld(ptbe,1,STRING,1,1, "input file =",-1,12,NULL,ifile,NO);
    vdeffld(ptbe,2,STRING,2,1, "x size     =",-1,8,NULL,gen_x,NO);
    vdeffld(ptbe,3,STRING,3,1, "y size     =",-1,8,NULL,gen_y,NO);
    vdeffld(ptbe,4,STRING,4,1, "axis (pop,inst)    =",-1,4,NULL,axes,NO);
    vdeffld(ptbe,5,STRING,5,1, "shrink factor      =",-1,4,NULL,sshrink,NO);
    vdeffld(ptbe,6,BOOLEAN,6,1,"separate genotypes =",-1,1,NULL,&genotypes,NO);
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whbe,YES,YES);
    ptbe->fld_valfn = run_info_chk;
    ptbe->fld_valid = run_info;
    ptbe->fld_setfn = FieldSet;
    if(DWSUCCESS == vread(ptbe,whbe,YES))
    {   vdelete(whint,whbe); vdelete(whbe,NONE); vdelete(whins,NONE);
        if(genotypes)
        {   sscanf(gen_x,"%d%s", &siz_x, lab_x);
            sscanf(gen_y,"%d%s", &siz_y, lab_y);
        }
        else
        {   sscanf(gen_x,"%d", &siz_x);
            sscanf(gen_y,"%d", &siz_y);
        }
        sscanf(sshrink,"%f", &shrink);
        xln = yln = shrink;
        xo = (1. - xln) / 2.;
        yo = 1. - yln;
        trac();
    }
    else
    {   vdelete(whint,whbe); vdelete(whbe,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

char    *sim_input = NULL;
unsigned  key;

int FieldSet(TRANSACTION  *tp)
{   MENUHDR  *disp;
    MENUITEM  *sel;

  if(tp->cur_fld == 4)
  {
    key = tp->thiskey;
    disp = MNUCreateHdr(POPUP);
    disp->toprow = 7;
    disp->topcol = 20;
    disp->uattr  = 8;
    disp->dattr  = 16;
    disp->lattr  = 13;
    MNUAddItem("pop", "display by population size   ",'P',0,NULL,disp,NULLF);
    MNUAddItem("inst","display by instruction number",'I',0,NULL,disp,NULLF);
      sel = MNUDoSelect(disp,NULLF);
      if(sel)
      {   tp->inp_get = KeySim;
          sim_input = sel->item0;
      }
  }
  else tp->inp_get = (unsigned (*)()) vgetkey;
  return FALSE;
}

unsigned KeySim(unsigned (*fp)(), TRANSACTION *tp)
{   bool  ff = 0;

    if(ff) { ff = FALSE; return (unsigned) 11; } /* 11 == FIELD_ERASE */
    if(*sim_input) return (unsigned) *sim_input++;
    else
    {   sim_input = NULL;
        ff = TRUE;
        tp->inp_get = (unsigned (*)()) vgetkey;
        return key;
    }
}

int run_info_chk(TRANSACTION  *tp)
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
    if(tp->cur_fld == 4)
    {   if(!strcmp(axes,"pop") || !strcmp(axes,"inst")) return TRUE;
        wierror(2,45,"pop or inst please");
        return FALSE;
    }
    return TRUE;
}

void run_info(TRANSACTION  *tp)
{   char    data[100];
    int     i, tnum_sp;
    static int  first = 1;

    if(!strcmp(axes,"pop")) { pop = 1; inst = 0; }
    if(!strcmp(axes,"inst")) { pop = 0; inst = 1; }
    if((tp->cur_fld > 1 && tp->cur_fld < 4) && tp->cur_fld != 6) return ;
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
    fclose(info);
    if(num_sp > 22) num_sp = 22;
    if(first) first = 0;
    else vdelete(whint,whbe);
    whint = wigen(1,56,num_sp + 1,23,FRSINGLE,"run info");
    visible(whint,YES,NO);
    vatputf(whint,0,1,"species = %d",tnum_sp);
    for(i = 0; i < num_sp; i++)
    {   vatputf(whint,i + 1,1,"%5d", sp[i].size);
        if(genotypes) vatputf(whint,i + 1,6,"%s", sp[i].label);
        if(pop) vatputf(whint,i + 1,10,"%u", sp[i].max);
        if(inst) vatputf(whint,i + 1,10,"%ld", sp[i].min);
    }
    visible(whbe,YES,YES);
    if(sp[0].size < 57 && sp[1].size > 56)
    {   siz_x = sp[1].size; sprintf(lab_x,sp[1].label);
        siz_y = sp[0].size; sprintf(lab_y,sp[0].label);
    }
    else
    {   siz_x = sp[0].size; sprintf(lab_x,sp[0].label);
        siz_y = sp[1].size; sprintf(lab_y,sp[1].label);
    }
    if(genotypes)
    {   sprintf(gen_x,"%d%s", siz_x, lab_x);
        sprintf(gen_y,"%d%s", siz_y, lab_y);
    }
    else
    {   sprintf(gen_x,"%d", siz_x);
        sprintf(gen_y,"%d", siz_y);
    }
    vfldrfsh(ptbe,whbe);
}

void tracest_setup(void)
{   if(y_res == 480)
    {   tr.csz = 16; tr.lbd = 10; tr.tbd = 10; tr.bti = 4; tr.tim = 4;
    }
    if(y_res == 200)
    {   tr.csz = 8; tr.lbd = 10; tr.tbd = 5; tr.bti = 4; tr.tim = 4;
    }
}

void trac(void)
{   struct point   p = { 0, 0 } ;
    Uint   c;
    FILE   *inf;
    int    i, num_x, num_y, first = 1, firstr = 1, linecolor, lettercolor;
    int    tnum_sp, xnum_sp, binum = 1;
	char   data[81], bifile[13];
    Ulong  otime = 0;

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
    tracest_setup();
    species_array_setup(&xnum_sp);
    fclose(info);
    for(i = 0; i < num_sp; i++)
    {   if(genotypes)
        {   if(sp[i].size == siz_x && !strcmp(sp[i].label,lab_x))
            { if(pop) xlpl = sp[i].max; if(inst) xlpl = sp[i].min; }
            if(sp[i].size == siz_y && !strcmp(sp[i].label,lab_y))
            { if(pop) ylpl = sp[i].max; if(inst) ylpl = sp[i].min; }
        }
        else
        {   if(sp[i].size == siz_x)
            { if(pop) xlpl = sp[i].max; if(inst) xlpl = sp[i].min; }
            if(sp[i].size == siz_y)
            { if(pop) ylpl = sp[i].max; if(inst) ylpl = sp[i].min; }
        }
    }
    num_x = num_y = 0;
    sscanf(ifile,"%[^.]", bifile);
    sprintf(info_file,"%s/%s", directory, ifile);
    inf = fopen(info_file,"r");
    if(inf == NULL)
    {   wierror(2,45,"run file not found");
        return ;
    }
    if(scr)
    {   setgraphmode(graphics_mode);
        lettercolor = getmaxcolor();
        if(lettercolor > 2)
            linecolor = lettercolor - 1;
        else if(lettercolor > 1)
            linecolor = lettercolor - 1;
        else linecolor = lettercolor;
        if(y_res == 480) settextstyle(0,0,2);
        if(y_res == 200) settextstyle(0,0,1);
        sprintf(data,"T = 0");
        outtextxy(tr.lbd, tr.tbd, data);
        if(pop)
        {   if(genotypes) sprintf(data,"Xp(%d%s)=%d",siz_x,lab_x,(int) xlpl);
            else sprintf(data,"Xp(%i)=%i", siz_x, (int) xlpl);
            outtextxy(tr.lbd + 165, tr.tbd, data);
            if(genotypes) sprintf(data,"Yp(%d%s)=%d",siz_y,lab_y,(int) ylpl);
            else sprintf(data,"Yp(%i)=%i", siz_y, (int) ylpl);
            outtextxy(tr.lbd + 410, tr.tbd, data);
        }
        if(inst)
        {   if(genotypes) sprintf(data,"Xi(%d%s)=%ld",siz_x,lab_x,(long) xlpl);
            else sprintf(data,"Xi(%i)=%ld", siz_x, (long) xlpl);
            outtextxy(tr.lbd + 165, tr.tbd, data);
            if(genotypes) sprintf(data,"Yi(%d%s)=%ld",siz_y,lab_y,(long) ylpl);
            else sprintf(data,"Yi(%i)=%ld", siz_y, (long) ylpl);
            outtextxy(tr.lbd + 410, tr.tbd, data);
        }
    }
    setcolor(linecolor);
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
        if(!t_read(data, &lo, &firstr, &gent))
        {   sscanf(data,"%*s%*s%d%*s%*s%lu%*s%*s%lu",
                &tnum_sp, &lo.ctime, &lo.time);
            for(i = 0; i < tnum_sp; i++) fgets(data,80,inf);
            continue;
        }
        if(lo.ctime > otime)
        {   setfillstyle(SOLID_FILL,0);
            setcolor(lettercolor);
            sb.left = (tr.bti * tr.csz) + tr.lbd;
            sb.top = tr.tbd;
            sb.right = (tr.tim * tr.csz) + sb.left;
            sb.bottom = sb.top + tr.csz;
            bar(sb.left, sb.top, sb.right, sb.bottom);
            sprintf(data,"%i", (int) lo.ctime);
            outtextxy(sb.left, sb.top, data);
            setcolor(linecolor);
            sline(&p);
            otime = lo.ctime;
        }
        if(lo.size == siz_x && (!genotypes || !strcmp(lo.label,lab_x)))
        {   if(lo.bd == 'b') num_x++;
            if(lo.bd == 'd') num_x--;
        }
        if(lo.size == siz_y && (!genotypes || !strcmp(lo.label,lab_y)))
        {   if(lo.bd == 'b') num_y++;
            if(lo.bd == 'd') num_y--;
        }
        if((lo.size == siz_x && (!genotypes || !strcmp(lo.label,lab_x))) ||
        (lo.size == siz_y && (!genotypes || !strcmp(lo.label,lab_y))))
        {   p.x = (float) num_x; p.y = (float) num_y;
            if(inst) { p.x *= siz_x; p.y *= siz_y; }
            p.x = (p.x - xnpl) / xlpl; p.y = (p.y - ynpl) / ylpl;
            if(first) { sline(&p); first = 0; }
            else cline(&p);
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
