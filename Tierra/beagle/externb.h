/* externb.h  22-3-92  external definitions of global variables */

#ifndef EXTERNB_H
#define EXTERNB_H

#include <dw.h>
#include <dwdata.h>
#include <stdio.h>

extern I32s   GFormat, Nop0, Nop1, NopS;
extern FILE   *ouf;
extern char   xv[], yv[];
extern int    scr, graphics_mode;
extern float  x_res, y_res;
extern float  xpr, ypr, rpr, thpr, xnpl, xxpl, xlpl, ynpl, yxpl, ylpl;
extern float  xnvf, xxvf, ynvf, yxvf;
extern char   sxn[], sxx[], syn[], syx[], sstz[];
extern int    hp, scr, cad, rt, sclach, Tnum;
extern float  xln, yln, xo, yo;
extern struct xyrange    xywin;
extern HWND   whinst, whvf, whpw, whpak, whtest, wherror, whwait, whwaitm;
extern ArgInstDef  aid[INSTNUM], *templ;

#endif /* EXTERNB_H */
