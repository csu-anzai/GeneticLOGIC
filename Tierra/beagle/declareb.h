/* declareb.h  3-5-92  global variable declarations for the Beagle Explorer */
/*** Beagle Explorer:  Copyright (c) 1992  Tom Ray ***/

#ifndef DECLARE_H
#define DECLARE_H

TRANSACTION    *ptvf;
HWND        whinst, whvf, whpw, whpak, whtest, wherror, whwait, whwaitm;

I32s  GFormat = -1, Nop0 = 0, Nop1 = 1, NopS = 1;
struct xyrange    xywin;
int    cad = 1, hp = 0, scr = 0, rt = 0, sclach = 0, Tnum;
float  xo = 0, yo = 0, xln = 1, yln = 1, xnvf, xxvf, ynvf, yxvf;
float  xnpl = 0.0, xxpl = 1., ynpl = 0.0, yxpl = 1., xlpl=1., ylpl=1.;
float  x_res, y_res, xpr, ypr, rpr, thpr;
FILE   *ouf;

ArgInstDef  *templ, aid[INSTNUM];

#endif /* DECLARE_H */
