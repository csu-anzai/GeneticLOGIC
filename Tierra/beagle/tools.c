/* tools.c  22-3-92  tools for graphis and windows */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/

#include "beagle.h"
#include "externb.h"

int    graphics_adapter = 0, graphics_mode = 0;

void grinit(char  path[])
{   char  dir[120], *epath;
    HWND  whgr, whins;
    TRANSACTION  *ptgr;
    int  i, drive, ndrive, ga, gm, max;
    size_t  dln;

    detectgraph(&graphics_adapter,&graphics_mode);
    ga = graphics_adapter; gm = graphics_mode;
    initgraph(&ga,&gm,path);
    if(ga > -1) goto found;
    if(ga != -3)
        initerror(ga);
    /* look in current directory */
    ga = graphics_adapter; gm = graphics_mode;
    initgraph(&ga,&gm,"");
    if(ga > -1) goto found;
    if(ga != -3)
        initerror(ga);
    /* look in x:\tc directories for all drives */
    drive  = getdisk(); /* drive = current drive */
    ndrive = setdisk(drive); /* ndrive = number of drives */
    for(i = 2; i < ndrive; i++)
    {   sprintf(path,"%c:/tc", 'a' + i);
        ga = graphics_adapter; gm = graphics_mode;
        initgraph(&ga,&gm,path);
        if(ga > -1) goto found;
        if(ga != -3)
            initerror(ga);
    }
    /* look in x:\tc\bgi directories for all drives */
    for(i = 2; i < ndrive; i++)
    {   sprintf(path,"%c:/tc/bgi", 'a' + i);
        ga = graphics_adapter; gm = graphics_mode;
        initgraph(&ga,&gm,path);
        if(ga > -1) goto found;
        if(ga != -3)
            initerror(ga);
    }
    /* look in x:\bc\bgi directories for all drives */
    for(i = 2; i < ndrive; i++)
    {   sprintf(path,"%c:/bc/bgi", 'a' + i);
        ga = graphics_adapter; gm = graphics_mode;
        initgraph(&ga,&gm,path);
        if(ga > -1) goto found;
        if(ga != -3)
            initerror(ga);
    }
    /* search path */
    epath = getenv("PATH");
    max = strlen(epath);
    while((dln = strcspn(epath,";")) && max > 0)
    {   strncpy(path,epath,dln); path[dln] = 0;
        ga = graphics_adapter; gm = graphics_mode;
        initgraph(&ga,&gm,path);
        if(ga > -1) goto found;
        if(ga != -3)
            initerror(ga);
        epath += dln + 1;
        max -= dln + 1;
    }
    epath = getenv("PATH");
    max = strlen(epath);
    while((dln = strcspn(epath,";")) && max > 0)
    {   strncpy(dir,epath,dln); dir[dln] = 0;
        sprintf(path,"%s/bgi", dir);
        ga = graphics_adapter; gm = graphics_mode;
        initgraph(&ga,&gm,path);
        if(ga > -1) goto found;
        if(ga != -3)
            initerror(ga);
        epath += dln + 1;
        max -= dln + 1;
    }
    /* ask user for path to bgi files */
    whgr = wigen(3,4,3,50,FRSINGLE,"graphics drivers path");
    vatputf(whgr,0,1,"graphics drivers were not found in the path:");
    vatputf(whgr,2,1,"please enter the correct path");
    ptgr = vpreload(4,0,NULLF,NULLF,NULLF,NULLF);
    (*(ptgr->def))[0].use_ext = TRUE;
    vdeffld(ptgr,0,STRING,1,1,"",-1,45,NULL,path,NO);
    whins = wiinv(23,28, "Ctrl-Enter to continue");
    visible(whgr,YES,YES);
    ptgr->fld_valfn = gr_chk;
    vread(ptgr,whgr,YES);
    vdelete(whins,NONE);
    vdelete(whgr,NONE);
    ga = graphics_adapter; gm = graphics_mode;
    initgraph(&ga,&gm,path);
    if(ga < 0)
        initerror(ga);
    found:
    resolutions();
    setcolor(1);
    restorecrtmode();
}

void initerror(int  val)
{   HWND  whgr;

    whgr = wigen(3,4,1,50,FRSINGLE,"initgraph error");
    if(val == -2)
        vatputf(whgr,0,1,"cannot detect a graphics card");
    if(val == -3)
        vatputf(whgr,0,1,"cannot find driver file");
    if(val == -4)
        vatputf(whgr,0,1,"invalid driver");
    if(val == -5)
        vatputf(whgr,0,1,"insufficient memory to load driver");
    vexit(0);
}

void grwisetup(char  path[])
{   char  curdir[80];
    HWND  whgr;
    TRANSACTION  *ptgr;
    int  success;

    getcwd(curdir,79);
    success = chdir(path);
    chdir(curdir);
    if(success)
    {   whgr = wigen(3,4,3,50,FRSINGLE,"graphics drivers path");
        vatputf(whgr,0,1,"graphics drivers were not found in the path:");
        vatputf(whgr,2,1,"please enter the correct path");
        ptgr = vpreload(4,0,NULLF,NULLF,NULLF,NULLF);
        (*(ptgr->def))[0].use_ext = TRUE;
	vdeffld(ptgr,0,STRING,1,1,"",-1,45,NULL,path,NO);
        visible(whgr,YES,YES);
        ptgr->fld_valfn = gr_chk;
        vread(ptgr,whgr,YES);
        vdelete(whgr,NONE);
    }
    detectgraph(&graphics_adapter,&graphics_mode);
    initgraph(&graphics_adapter,&graphics_mode,path);
    resolutions();
    setcolor(1);
    restorecrtmode();
}

int gr_chk(TRANSACTION  *tp)
{   char  curdir[80];
    int   success;

    getcwd(curdir,79);
    success = chdir((*(tp->def))[tp->cur_fld].dataptr);
    chdir(curdir);
    if(success) return FALSE;
    return TRUE;
}

void resolutionn(void)
{    x_res = getmaxx() + 1;
     y_res = getmaxy() + 1;
}

void resolutions(void)
{   switch(graphics_adapter)
    {   case CGA: switch(graphics_mode)
        {   case CGAC0: x_res = 320; y_res = 200; return;
            case CGAC1: x_res = 320; y_res = 200; return;
            case CGAC2: x_res = 320; y_res = 200; return;
            case CGAC3: x_res = 320; y_res = 200; return;
            case CGAHI: x_res = 640; y_res = 200; return;
        }
        case MCGA: switch(graphics_mode)
        {   case MCGAC0:  x_res = 320; y_res = 200; return;
            case MCGAC1:  x_res = 320; y_res = 200; return;
            case MCGAC2:  x_res = 320; y_res = 200; return;
            case MCGAC3:  x_res = 320; y_res = 200; return;
            case MCGAMED: x_res = 640; y_res = 200; return;
            case MCGAHI:  x_res = 640; y_res = 480; return;
        }
        case EGA: switch(graphics_mode)
        {   case EGALO: x_res = 640; y_res = 200; return;
            case EGAHI: x_res = 640; y_res = 350; return;
        }
        case EGA64: switch(graphics_mode)
        {   case EGA64LO: x_res = 640; y_res = 200; return;
            case EGA64HI: x_res = 640; y_res = 350; return;
        }
        case EGAMONO: switch(graphics_mode)
        {   case EGAMONOHI: x_res = 640; y_res = 350; return;
        }
        case IBM8514: switch(graphics_mode)
        {   case IBM8514HI: x_res =  640; y_res = 480; return;
            case IBM8514LO: x_res = 1024; y_res = 768; return;
        }
        case HERCMONO: switch(graphics_mode)
        {   case HERCMONOHI: x_res = 720; y_res = 348; return;
        }
        case ATT400: switch(graphics_mode)
        {   case ATT400C0:  x_res = 320; y_res = 200; return;
            case ATT400C1:  x_res = 320; y_res = 200; return;
            case ATT400C2:  x_res = 320; y_res = 200; return;
            case ATT400C3:  x_res = 320; y_res = 200; return;
            case ATT400MED: x_res = 640; y_res = 200; return;
            case ATT400HI:  x_res = 640; y_res = 400; return;
        }
        case VGA: switch(graphics_mode)
        {   case VGALO:  x_res = 640; y_res = 200; return;
            case VGAMED: x_res = 640; y_res = 350; return;
            case VGAHI:  x_res = 640; y_res = 480; return;
        }
        case PC3270: switch(graphics_mode)
        {   case PC3270HI: x_res = 720; y_res = 350; return;
        }
    }
    printf("\n\nUnable to proceed.\n");
    printf("Requires CGA, MCGA, EGA, EGA64, EGAMONO, IBM8514, HERCMONO\n"
        "ATT400, VGA or PC3270 adapter; with appropriate monitor.\n");
    exit(0);
}

/* graphics_adapter types:

    DETECT       0
    CGA          1
    MCGA         2
    EGA          3
    EGA64        4
    EGAMONO      5
    IBM8514      6
    HERCMONO     7
    ATT400       8
    VGA          9
    PC3270      10

   graphics_mode types:

   graphics    graphics    value   columns   palette     pages
   adapter     modes               x rows

    CGA         CGAC0         0    320x200    C0           1
                CGAC1         1    320x200    C1           1
                CGAC2         2    320x200    C2           1
                CGAC3         3    320x200    C3           1
                CGAHI         4    640x200    2 color      1

    MCGA        MCGAC0        0    320x200    C0           1
                MCGAC1        1    320x200    C1           1
                MCGAC2        2    320x200    C2           1
                MCGAC3        3    320x200    C3           1
                MCGAMED       4    640x200    2 color      1
                MCGAHI        5    640x480    2 color      1

    EGA         EGALO         0    640x200    16 color     4
                EGAHI         1    640x350    16 color     2

    EGA64       EGA64LO       0    640x200    16 color     1
                EGA64HI       1    640x350    4 color      1

    EGAMONO     EGAMONOHI     3    640x350    2 color      1*
                EGAMONOHI     3    640x350    2 color      2**

    IBM8514     IBM8514HI     0    640x480    256 color
                IBM8514LO     0   1024x768    256 color

    HERCMONO    HERCMONOHI    0    720x348    2 color      2

    ATT400      ATT400C0      0    320x200    C0           1
                ATT400C1      1    320x200    C1           1
                ATT400C2      2    320x200    C2           1
                ATT400C3      3    320x200    C3           1
                ATT400MED     4    640x200    2 color      1
                ATT400HI      5    640x400    2 color      1

    VGA         VGALO         0    640x200    16 color     2
                VGAMED        1    640x350    16 color     2
                VGAHI         2    640x480    16 color     1

    PC3270      PC3270HI      0    720x350    2 color      1
    
     *  64K on EGAMONO card
    ** 256K on EGAMONO card
*/

void g_keyboard(void)
{   union u_type { int a; char b[3]; } keystroke;
    int    get_keystroke(void);           /* declare a local subroutine */

    do keystroke.a = getkeystroke();
    while (keystroke.b[0] != 27);        /* return if <Esc> is pressed */
}

int getkeystroke(void)
{   union REGS regs;

    regs.h.ah = 0;
    return int86(0x16,&regs,&regs);
}

void minmax(struct point  *p, struct xyrange  *r)
{   if(r->f)
    {   r->f = 0;
        r->xn = r->xx = p->x;
        r->yn = r->yx = p->y;
    }
    if(p->x < r->xn) r->xn = p->x; if(p->x > r->xx) r->xx = p->x;
    if(p->y < r->yn) r->yn = p->y; if(p->y > r->yx) r->yx = p->y;
    r->n++;
}

struct point coords(struct point  *p)
{   float    xl, yl, xmin, xmax, ymin, ymax, r = .75;
    struct point  t;

    xmin = xnvf; xmax = xxvf; ymin = ynvf; ymax = yxvf;
    xl = xmax - xmin; yl = ymax - ymin;
    if(yl / xl >= r)
    {   xmin -= (yl / r - xl) / 2.;
        xmax += (yl / r - xl) / 2.;
    }
    else
    {   ymin -= (r * xl - yl) / 2.;
        ymax += (r * xl - yl) / 2.;
    }
    xl = xmax - xmin; yl = ymax - ymin;
    t.x = 639. * (p->x - xmin) / xl; t.y = 479. - (479. * (p->y - ymin) / yl);
    t.x = t.x * x_res / 640.; t.y = t.y * y_res / 480.;
    return t;
}

struct point rcoords(struct point  *p)
{   float    xl, yl, xmin, xmax, ymin, ymax, r = .75;
    struct point  t;

    xmin = xnvf; xmax = xxvf; ymin = ynvf; ymax = yxvf;
    xl = xmax - xmin; yl = ymax - ymin;
    if(yl / xl >= r)
    {   xmin -= (yl / r - xl) / 2.;
        xmax += (yl / r - xl) / 2.;
    }
    else
    {   ymin -= (r * xl - yl) / 2.;
        ymax += (r * xl - yl) / 2.;
    }
    xl = xmax - xmin; yl = ymax - ymin;
    t.x = xmin + (p->x * (640. / 639.) * (xl / x_res));
    t.y = ymin - ((p->y - (y_res * 479. / 480)) * (480. / 479.)*(yl/y_res));
    return t;
}

struct point * fxy(struct point  *p)
{   struct point  t;

    t.x = (p->x - xnpl) / xlpl;
    t.y = (p->y - ynpl) / ylpl;
    return &t;
}

struct point rrotr(struct point  *xy)
{   struct point    p;

    if(rt) { p.x = xy->y; p.y = -xy->x; }
    else p = *xy;
    p.x = (p.x - xo) / xln; p.y = (p.y - yo) / yln;
    return p;
}

struct point rotr(struct point  *xy)
{   struct point    p, t;

    t.x = xy->x * xln + xo; t.y = xy->y * yln + yo;
    if(rt) { p.x = t.y; p.y = -t.x; }
    else p = t;
    return p;
}

void sline(struct point  *p)
{   struct point  t;

    t = rotr(p); minmax(&t,&xywin);
    if(cad) fprintf(ouf, "li %.5f,%.5f;\n", t.x, t.y);
    if(hp) fprintf(ouf, "pu %.5f,%.5f pd;\n", t.x, t.y);
    if(scr)
    {   t = coords(&t);
        moveto((int) t.x,(int) t.y);
    }
}

void cline(struct point  *p)
{   struct point  t;

    t = rotr(p); minmax(&t,&xywin);
    if(cad) fprintf(ouf, "%.5f,%.5f;\n", t.x, t.y);
    if(hp) fprintf(ouf, "pd %.5f,%.5f;\n", t.x, t.y);
    if(scr)
    {   t = coords(&t);
        lineto((int) t.x,(int) t.y);
    }
}

void eline(struct point  *p)
{   struct point  t;

    t = rotr(p); minmax(&t,&xywin);
    if(cad) fprintf(ouf, "%.5f,%.5f;\npu;\n", t.x, t.y);
    if(hp) fprintf(ouf, "pd %.5f,%.5f pu;\n", t.x, t.y);
    if(scr)
    {   t = coords(&t);
        lineto((int) t.x,(int) t.y);
    }
}

void putpoint(struct point  *p, int color)
{   struct point  t;

    t = rotr(p); minmax(&t,&xywin);
    if(cad) fprintf(ouf, "%.5f,%.5f;\npu;\n", t.x, t.y);
    if(hp) fprintf(ouf, "pd %.5f,%.5f pu;\n", t.x, t.y);
    if(scr)
    {   t = coords(&t);
        putpixel((int) t.x, (int) t.y, color);
    }
}

void charsiz(float xz, float yz) /* sets character size and aspect ratio */
{   if(cad) fprintf(ouf,"tz,%.5f;\nta,%.5f;\n", yz,  1.26 * xz / yz);
    if(hp) fprintf(ouf,"sr %.5f,%.5f ;\n", xz, yz);
}

void labchr(struct point  *p, float xz, float yz, float an, char lc)
/* places a character centered at the coordinate p.  xz and yz are
the character size.  an is the angle of the character */
{   char    tmp[2];
    struct point  t;

    if(rt) an -= 90.;
    t = rotr(p); minmax(&t,&xywin);
    if(cad)
    {   if(an != 0 && !sclach) fprintf(ouf,"tr,%.5f;\n", an);
        if(rt) { t.x -= xz / 2.; t.y += yz / 2.; }
        else { t.x -= xz / 2.; t.y -= yz / 2.; }
        fprintf(ouf,"tp,%.5f,%.5f,%c;\n", t.x, t.y, lc);
        if(an != 0 && !sclach) fprintf(ouf,"tr,0;\n");
    }
    if(hp)
    {   if(an != 0 && !sclach) fprintf(ouf, "dr0,-1;\n");
        fprintf(ouf,"pu%.5f,%.5f;cp-.333,-.25;lb%c;pu;\n",t.x,t.y,lc);
        if(an != 0 && !sclach) fprintf(ouf,"dr;\n");
    }
    if(scr)
    {   sprintf(tmp,"%c",lc); t = coords(&t);
        outtextxy((int) t.x,(int) t.y,tmp);
    }
}

void labstr(struct point  *p, float xz, float yz, float an, char lab[])
/* places a string, starting at the coordinate xp, yp */
{   struct point  t;

    if(rt) an -= 90;
    t = rotr(p); minmax(&t,&xywin);
    if(cad)
    {   if(an != 0) fprintf(ouf,"tr,%.5f;\n", an);
        fprintf(ouf,"tp,%.5f,%.5f,%s;\n", t.x, t.y, lab);
        if(an != 0) fprintf(ouf,"tr,0;\n");
    }
    if(hp)
    {   if(an == 90) fprintf(ouf, "dr0,1;\n");
        if(an == -90) fprintf(ouf, "dr0,-1;\n");
        fprintf(ouf,"pu%.5f,%.5f;lb%s;pu;\n", t.x, t.y, lab);
        if(an != 0) fprintf(ouf,"dr;\n");
    }
    if(scr)
    {   t = coords(&t);
        outtextxy((int) t.x,(int) t.y,lab);
    }
    t.x += strlen(lab) * xz; t.y += yz;
    t = rotr(&t); minmax(&t,&xywin);
}

void curon(struct point  *p, int siz)
{   float    cl;
    struct point    t, u;

    u = rotr(p); minmax(&u,&xywin);
    if(scr)
    {   u = coords(&u);
        moveto((int) u.x,(int) u.y + siz);
        lineto((int) u.x,(int) u.y - siz);
        moveto((int) u.x - 2 * siz,(int) u.y);
        lineto((int) u.x + 2 * siz,(int) u.y);
        moveto((int) u.x,(int) u.y);
    }
    else
    {   cl = (yxvf - ynvf) * (float) siz / 200.;
        eline(p); t = u;
        t.y = u.y + cl; sline(&t); t.y = u.y - cl; eline(&t);
        t.y = u.y;
        t.x = u.x - cl; sline(&t); t.x = u.x + cl; eline(&t);
        sline(&u);
    }
}

void curof(struct point  *p, int siz)
{   struct point  t;

    t = rotr(p);
    if(scr)
    {   t = coords(&t); setcolor(0);
        moveto((int) t.x,(int) t.y + siz);
        lineto((int) t.x,(int) t.y - siz);
        moveto((int) t.x - 2 * siz,(int) t.y);
        lineto((int) t.x + 2 * siz,(int) t.y);
        moveto((int) t.x,(int) t.y); setcolor(1);
    }
}

struct rect ptr(struct polar  *rth)
{	struct rect xy;

	xy.dx = rth->r * cos(rth->th);
	xy.dy = rth->r * sin(rth->th);
	return xy;
}

struct polar rtp(struct rect  *xy)
{	struct polar	rth;

	rth.r = hypot(xy->dx,xy->dy);
	rth.th = atanc(xy->dy,xy->dx);
	return rth;
}

float redangle(float th)
{	if(th >  M_PI) th -= 2 * M_PI;
	if(th < -M_PI) th += 2 * M_PI;
	if(th >= -M_PI && th <= M_PI) return th;
	return redangle(th);
}

void ptro(float th, float r)
{	xpr = r * cos(th);
	ypr = r * sin(th);
}

void rtpo(float x, float y)
{	rpr = hypot(x,y);
	thpr = atanc(y,x);
}

float atanc(float y, float x)
{	if(x != 0.) return atan2(y,x);
	if(y >= 0) return M_PI_2;
	else return -M_PI_2;
}

void wind_setup(void)
{   pclrattr(REVHELP);
    PRM_NRM0 = REVHELP; PRM_SEL0 = REVEMPHNORML;
    FLD_NRM0 = REVHELP; FLD_SEL0 = HELP;
}

HWND wigen(int rp, int cp, int rs, int cs, char frame[7], char title[80])
{   HWND whtmp;
    whtmp = vcreat(rs,cs,REVHELP,NO);
    vwind(whtmp,rs,cs,0,0); /* write with vatputs(whtmp,x,y,"string") */
    vlocate(whtmp,rp,cp);
    vframe(whtmp,REVEMPHNORML,frame);
    visible(whtmp,YES,NO);
    vtitle(whtmp,REVEMPHNORML,title);
    return whtmp;
}

HWND wigenlp(int  row_pos, int  col_pos, int  row_siz_log, int  col_siz_log,
     int  row_siz_phy, int  col_siz_phy, char  frame[7], char  title[80])
{   HWND whtmp;
    whtmp = vcreat(row_siz_log, col_siz_log, REVHELP, NO);
    vwind(whtmp, row_siz_phy, col_siz_phy, 0, 0);
    vlocate(whtmp, row_pos, col_pos);
    vframe(whtmp, REVEMPHNORML, frame);
    visible(whtmp, YES, NO);
    vtitle(whtmp, REVEMPHNORML, title);
    return whtmp;
}       /* write with vatputs(whtmp,x,y,"string") */

void wipak(int rp, int cp)
{   whpak = vcreat(1,15,REVNORML,NO);
    vwind(whpak,1,15,0,0);
    vlocate(whpak,rp,cp);
    vframe(whpak,REVNORML,FRSINGLE);
    visible(whpak,YES,NO);
    vratputs(whpak,0,1,REVHELP,"press any key");
    getkey(); vdelete(whpak,NONE);
}

void wiin(int x, int y)
{   whinst = vcreat(1,42,REVNORML,NO);
    vwind(whinst,1,42,0,0);
    vlocate(whinst,x,y);
    vframe(whinst,REVNORML,FRSINGLE);
    vtitle(whinst,REVNORML,"instructions");
    visible(whinst,YES,NO);
    vratputs(whinst,0,1,REVHELP,
        "\x1e\x1f, or ENTER; Ctrl ENTER to leave window");
}

HWND wiinv(int x, int y, char  mes[80])
{   HWND whinst;
    int  meslen = strlen(mes) + 2;

    whinst = vcreat(1,meslen,REVNORML,NO);
    vwind(whinst,1,meslen,0,0);
    vlocate(whinst,x,y);
    vframe(whinst,REVNORML,FRSINGLE);
    vtitle(whinst,REVNORML,"instructions");
    visible(whinst,YES,NO);
    vratputf(whinst,0,1,REVHELP,"%s", mes);
    return whinst;
}

HWND wint(int x, int y, char  mes[80])
{   HWND whinst;
    int  meslen = strlen(mes) + 2;

    whinst = vcreat(1,meslen,REVNORML,NO);
    vwind(whinst,1,meslen,0,0);
    vlocate(whinst,x,y);
    visible(whinst,YES,NO);
    vratputf(whinst,0,1,REVHELP,"%s", mes);
    return whinst;
}

void wipw(int x, int y)
{   whpw = vcreat(1,13,REVNORML,NO);
    vwind(whpw,1,13,0,0);
    vlocate(whpw,x,y);
    vframe(whpw,REVNORML,FRSINGLE);
    visible(whpw,YES,NO);
    vratputs(whpw,0,1,REVHELP,"please wait");
}

void wiwait(int rp, int cp)
{   whwait = vcreat(1,13,REVNORML,NO);
    vwind(whwait,1,13,0,0);
    vlocate(whwait,rp,cp);
    vframe(whwait,REVNORML,FRSINGLE);
    visible(whwait,YES,NO);
    vratputs(whwait,0,1,REVHELP,"please wait");
}

void wiwaitm(int rp, int cp, char  mes[80])
{   int    siz = 11;
    if(strlen(mes)+2 > siz) siz = strlen(mes)+2;
    whwaitm = vcreat(1,siz,REVNORML,NO);
    vwind(whwaitm,1,siz,0,0);
    vlocate(whwaitm,rp,cp);
    vframe(whwaitm,REVNORML,FRSINGLE);
    visible(whwaitm,YES,NO);
    vtitle(whwaitm,REVNORML,"please wait");
    vratputf(whwaitm,0,1,REVNORML,"%s", mes);
}

void witest(int rp, int cp, int rs, int cs)
{   whtest = vcreat(rs,cs,REVNORML,NO);
    vwind(whtest,rs,cs,0,0); /* write with vatputs(whtest,x,y,"string") */
    vlocate(whtest,rp,cp);
    vframe(whtest,REVNORML,FRSINGLE);
    visible(whtest,YES,NO);
    vtitle(whtest,REVNORML,"test");
}

void wierror(int rp, int cp, char  mes[80])
{   int    siz = 7;
    if(strlen(mes) + 2 > siz) siz = strlen(mes) + 2;
    wherror = vcreat(1,siz,REVNORML,NO);
    vwind(wherror,1,siz,0,0);
    vlocate(wherror,rp,cp);
    vframe(wherror,REVNORML,FRSINGLE);
    visible(wherror,YES,YES);
    vtitle(wherror,REVNORML,"error");
    vratputf(wherror,0,1,REVNORML,"%s", mes);
    wipak(rp + 3, cp);
    vdelete(wherror,NONE);
}

void nrerror(char  error_text[80])
{   int    p, l;
    HWND    wher;
    void exit();

    l = strlen(error_text);
    if(l < 27) l = 27; l += 2; p = (int) (80 - l) / 2.;
    wher = wigen(9,p,3,l,FRDOUBLE,"fatal error");
    vatputs(wher,0,1,"Run-time error...");
    vatputf(wher,1,1,"%s",error_text);
    vatputs(wher,2,1,"...now exiting to system...");
    wipak(14,31);
    vexit(1);
}

void scroll(int  page, HWND  wi)
{   unsigned  key = 0;

    while(key != ESC)
    {   switch (key = getkey())
        {   case CURUP: vmovedn(wi,1); break;
            case CURDN: vmoveup(wi,1); break;
            case CURLF: vmovelf(wi,1); break;
            case CURRT: vmovert(wi,1); break;
            case PGUP: vmovedn(wi,page); break;
            case PGDN: vmoveup(wi,page); break;
            default: break;
        }
    }
}
