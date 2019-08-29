/* bdivers.c  22-3-92  diversity trace display for the Beagle Explorer */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

extern char   directory[], info_file[];
extern float  xln, yln, xo, yo, xlpl, xxpl, xnpl, ylpl, yxpl, ynpl, shrink;
extern FILE   *info;
extern struct tracest  tr;
extern unsigned  key;
extern int     genotypes;
extern double  ttime;

char    x_var[9], y_var[9], datafile[13], rangefile[13], *SimInput = NULL;
HWND         whdi;
TRANSACTION  *ptdi;

struct siz {
    long   Time;
    long   NumCell;
    long   NumSize;
    float  SizeDiv;
    long   AgeSize;
    } si;

struct gen {
    long   NumGeno;
    float  GenoDiv;
    long   AgeGeno;
    } ge;

int diverse(void)
{   int  i;
    static int  first = 1;
    char  sshrink[5];
    HWND  whins;

    if(first)
    {   first = 0;
        xnvf = -.05; xxvf = 1.05; ynvf = -.05; yxvf = 1.1; shrink = 1.0;
        sprintf(x_var,"Time"); sprintf(y_var,"GenoDiv");
        sprintf(datafile,"divdat.1"); sprintf(rangefile,"divrange");
    }
    sprintf(sshrink,"%g", shrink);
    while(1) {
    whdi = wigen(3,1,6,55,FRDOUBLE,"diversity");
    ptdi = vpreload(6,0,NULLF,NULLF,NULLF,NULLF);
    for(i = 0; i <= 5; i++) (*(ptdi->def))[i].use_ext = TRUE;
    (*(ptdi->def))[1].refresh = (*(ptdi->def))[2].refresh =
    (*(ptdi->def))[5].refresh = ON;
    vdeffld(ptdi,0,STRING,0,1, "directory  =",-1,40,NULL,directory,NO);
    vdeffld(ptdi,1,STRING,1,1, "x var      =",-1,8,NULL,x_var,NO);
    vdeffld(ptdi,2,STRING,2,1, "y var      =",-1,8,NULL,y_var,NO);
    vdeffld(ptdi,3,STRING,3,1, "data file  =",-1,12,NULL,datafile,NO);
    vdeffld(ptdi,4,STRING,4,1, "range file =",-1,12,NULL,rangefile,NO);
    vdeffld(ptdi,5,STRING,5,1, "shrink factor =",-1,4,NULL,sshrink,NO);
    whins = wiinv(23,13,
        "\x1e\x1f, or Enter to select; Esc to exit; Ctrl-Enter to run");
    visible(whdi,YES,YES);
    ptdi->fld_valfn = RunInfoChk;
    ptdi->fld_setfn = dFieldSet;
    if(DWSUCCESS == vread(ptdi,whdi,YES))
    {   vdelete(whdi,NONE); vdelete(whins,NONE);
        sscanf(sshrink,"%f", &shrink);
        xln = yln = shrink;
        xln *= 1.5;
        xo = (1. - xln) / 2.;
        yo = 1. - yln;
        divers();
    }
    else
    {   vdelete(whdi,NONE); vdelete(whins,NONE);
        return 1;
    }
    }
    vdelete(whins,NONE);
    return 1;
}

int dFieldSet(TRANSACTION  *tp)
{   MENUHDR  *disp;
    MENUITEM  *sel;

  if(tp->cur_fld == 1 || tp->cur_fld == 2)
  {
    key = tp->thiskey;
    disp = MNUCreateHdr(POPUP);
    disp->toprow = 7;
    disp->topcol = 20;
    disp->uattr  = 8;
    disp->dattr  = 16;
    disp->lattr  = 13;
    MNUAddItem("Time", "elapsed time in instructions   ",'T',0,NULL,disp,NULLF);
    MNUAddItem("NumCell","number of cells",'C',0,NULL,disp,NULLF);
    MNUAddItem("NumSize","number of sizes",'I',0,NULL,disp,NULLF);
    MNUAddItem("SizeDiv","size diversity",'S',0,NULL,disp,NULLF);
    MNUAddItem("AgeSize","average age of sizes",'Z',0,NULL,disp,NULLF);
    MNUAddItem("NumGeno","number of genotypes",'E',0,NULL,disp,NULLF);
    MNUAddItem("GenoDiv","genotype diversity",'G',0,NULL,disp,NULLF);
    MNUAddItem("AgeGeno","average age of genotypes",'N',0,NULL,disp,NULLF);
      sel = MNUDoSelect(disp,NULLF);
      if(sel)
      {   tp->inp_get = dKeySim;
          SimInput = sel->item0;
      }
  }
  else tp->inp_get = (unsigned (*)()) vgetkey;
  return FALSE;
}

unsigned dKeySim(unsigned (*fp)(), TRANSACTION *tp)
{   bool  ff = 0;

    if(ff) { ff = FALSE; return (unsigned) 11; } /* 11 == FIELD_ERASE */
    if(*SimInput) return (unsigned) *SimInput++;
    else
    {   SimInput = NULL;
        ff = TRUE;
        tp->inp_get = (unsigned (*)()) vgetkey;
        return key;
    }
}

int RunInfoChk(TRANSACTION  *tp)
{   char  path[80], errmess[56];
    int   success;
    FILE  *inf;

    if(tp->cur_fld == 0)
    {   getcwd(path,79);
        success = chdir(directory);
        chdir(path);
        if(success)
        {   sprintf(errmess,"directory not found: %s", directory);
            wierror(11,10,errmess);
            return FALSE;
        }
        return TRUE;
    }
    if(tp->cur_fld == 3)
    {   sprintf(path,"%s\\%s", directory, datafile);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   sprintf(errmess,"file or path not found: %s", path);
            wierror(11,10,errmess);
            return FALSE;
        }
        fclose(inf);
        return TRUE;
    }
    if(tp->cur_fld == 4)
    {   sprintf(path,"%s\\%s", directory, rangefile);
        inf = fopen(path,"r");
        if(inf == NULL)
        {   sprintf(errmess,"file or path not found: %s", path);
            wierror(11,10,errmess);
            return FALSE;
        }
        fclose(inf);
    }
    return TRUE;
}

void TracestSetup(void)
{   if(y_res == 480)
    {   tr.csz = 16; tr.lbd = 10; tr.tbd = 10; tr.bti = 4; tr.tim = 4;
    }
    if(y_res == 200)
    {   tr.csz = 8; tr.lbd = 10; tr.tbd = 5; tr.bti = 4; tr.tim = 4;
    }
}

void divers(void)
{   struct point   p = { 0, 0 } ;
    Uint    c;
    FILE    *inf;
    int     first = 1, linecolor, lettercolor;
    int     x, y, binum = 1;
    char    errmess[56], bifile[13];
    Ulong   otime = 0, ctime;
    char    format, data[100];

    ttime = 0;
    tracest_setup();
    if(scr)
    {   setgraphmode(graphics_mode);
        lettercolor = getmaxcolor();
        if(lettercolor > 2)
        {   linecolor = lettercolor - 1;
        }
        else if(lettercolor > 1)
        {   linecolor = lettercolor - 1;
        }
        else linecolor = lettercolor;
        if(y_res == 480) settextstyle(0,0,2);
        if(y_res == 200) settextstyle(0,0,1);
        sprintf(data,"T = 0");
/*
        outtextxy(tr.lbd, tr.tbd, data);
        if(pop)
        {   if(genotypes) sprintf(data,"Xp(%d%s)=%d",siz_x,lab_x,(int) xlpl);
            else sprintf(data,"Xp(%i)=%i", siz_x, (int) xlpl);
            outtextxy(tr.lbd + 165, tr.tbd, data);
            if(genotypes) sprintf(data,"Yp(%d%s)=%d",siz_y,lab_y,(int) ylpl);
            else sprintf(data,"Yp(%i)=%i", siz_y, (int) ylpl);
            outtextxy(tr.lbd + 410, tr.tbd, data);
        }
*/
    }
    setcolor(linecolor);
    x = setxy(x_var);
    y = setxy(y_var);
    setranges();

    sscanf(datafile,"%[^.].%d", bifile, &binum);
    sprintf(info_file,"%s/%s", directory, datafile);
    inf = fopen(info_file,"rb");
    if(inf == NULL)
    {   sprintf(errmess,"data file not found: %s", datafile);
        wierror(2,35,errmess);
        return ;
    }
    fread(&format,sizeof(char),1,inf);
    fread(&genotypes,sizeof(char),1,inf);
    if(format)
    {   fclose(inf);
        inf = fopen(info_file,"r");
        fgets(data,80,inf);
        sscanf(data,"%*d %lf", &ttime);
    }
    else
        fread(&ttime,sizeof(double),1,inf);
    if(!genotypes && x > 5)
    {   restorecrtmode();
        sprintf(errmess,"can not use %s, %s does not include genotypes",
            x_var, datafile);
        wierror(2,15,errmess);
        return;
    }
    if(!genotypes && y > 5)
    {   restorecrtmode();
        sprintf(errmess,"can not use %s, %s does not include genotypes",
            y_var, datafile);
        wierror(2,15,errmess);
        return;
    }
    si.Time = 0.;
    for(;;)
    {   if(format)  /* ascii format */
        {   if(fgets(data,80,inf) == NULL)
            {   binum++;
                sprintf(info_file,"%s/%s.%d", directory, bifile, binum);
                fclose(inf);
                inf = fopen(info_file,"r");
                if(inf == NULL)
                    break ;
                if(fgets(data,80,inf) == NULL)
                    break ;
                sscanf(data,"%*d %lf", &ttime);
                if(fgets(data,80,inf) == NULL)
                    break ;
            }
            if(genotypes)
                sscanf(data,"%lx%ld%ld%f%lx%ld%f%lx",
                    &si.Time, &si.NumCell, &si.NumSize, &si.SizeDiv,
                    &si.AgeSize, &ge.NumGeno, &ge.GenoDiv, &ge.AgeGeno);
            else
                sscanf(data,"%lx%ld%ld%f%lx", &si.Time, &si.NumCell,
                    &si.NumSize, &si.SizeDiv, &si.AgeSize);
        }
        else  /* binary format */
        {   if(!fread(&si,sizeof(struct siz),1,inf))
            {   binum++;
                sprintf(info_file,"%s/%s.%d", directory, bifile, binum);
                fclose(inf);
                inf = fopen(info_file,"rb");
                if(inf == NULL)
                    break ;
                fread(data,sizeof(char),2,inf);
                fread(&ttime,sizeof(double),1,inf);
                if(!fread(&si,sizeof(struct siz),1,inf))
                    break ;
            }
            if(genotypes)
            fread(&ge,sizeof(struct gen),1,inf);
        }
        ttime += (double) si.Time;
        ctime = (Ulong) si.Time / 1000000uL;
        if(ctime > otime)
        {   setfillstyle(SOLID_FILL,0);
            setcolor(lettercolor);
            sprintf(data,"%i", (int) ctime);
            setcolor(linecolor);
            sline(&p);
            otime = ctime;
        }
        p.x = (float) putout(x); p.y = (float) putout(y);
        p.x = (p.x - xnpl) / xlpl; p.y = (p.y - ynpl) / ylpl;
        if(first) { sline(&p); first = 0; }
        else cline(&p);
        if(gfkbhit())
        {   c = getkey();
            if(c == 'p' || c == 'P') getkey();  /* P or p key */
            if(c == ESC) break ;                /* Esc key */
            if(c == 'g' || c == 'G')            /* G or g key */
                DumpScreen2Gif("image.gif",VGA,VGAHI,VGAHI,VGAHI);
        }
    }
/*  fclose(inf); */
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

void setranges(void)
{   FILE  *inf;
    char  data[81], tmp[8];
    
    sprintf(data,"%s/%s", directory, rangefile);
    inf = fopen(data,"r");
    while(1)
    {   if(fgets(data,80,inf) == NULL) break ;
        sscanf(data,"%s", tmp);
        if(!strcmp(tmp,x_var))
            sscanf(data,"%*s%f%f", &xnpl, &xxpl);
        if(!strcmp(tmp,y_var))
            sscanf(data,"%*s%f%f", &ynpl, &yxpl);
    }
    fclose(inf);
    xlpl = xxpl - xnpl; ylpl = yxpl - ynpl;
}

int setxy(s)
char  s[];
{   if(!strcmp(s,"Time"))
        return 1;
    if(!strcmp(s,"NumCell"))
        return 2;
    if(!strcmp(s,"NumSize"))
        return 3;
    if(!strcmp(s,"SizeDiv"))
        return 4;
    if(!strcmp(s,"AgeSize"))
        return 5;
    if(!strcmp(s,"NumGeno"))
        return 6;
    if(!strcmp(s,"GenoDiv"))
        return 7;
    if(!strcmp(s,"AgeGeno"))
        return 8;
    return 0;
}

double putout(s)
int  s;
{   switch(s)
    {
    case 1: return ttime;
    case 2: return (double) si.NumCell;
    case 3: return (double) si.NumSize;
    case 4: return (double) si.SizeDiv;
    case 5: return (double) si.AgeSize;
    case 6: return (double) ge.NumGeno;
    case 7: return (double) ge.GenoDiv;
    case 8: return (double) ge.AgeGeno;
    }
    return 0;
}
