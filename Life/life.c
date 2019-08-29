/*
 *   What fun!  A new LIFE program by Tomas Rokicki.  Commented rather
 *   erratically; hell, this is all just one big hack.
 */
int modulo ;
int wmodulo ;
int vsize ;
#define RASTSIZE (wmodulo * (long)(vsize+1))
#include "structures.h"
#include "blit.h"
short *a, *b, *c, *d, *e, *t1=NULL, *t2=NULL, *t3=NULL, *t4=NULL ;
short noplanes ;
short torus ;
short orify ;
struct GfxBase *GfxBase = NULL ;     /* the GfxBase */
struct IntuitionBase *IntuitionBase = NULL ; /* the IntuitionBase */
struct Screen *myscreen = NULL ;
struct Window *mywindow = NULL ;
struct TextAttr myfont = {(STRPTR)"topaz.font", TOPAZ_EIGHTY, 0, 0 };
struct NewScreen mynewscreen = {0, 0, 0, 0, 0, 1, 2, 0,
   CUSTOMSCREEN, &myfont, (UBYTE *)"AmigaLIFE -- Radical Eye Software"} ;
/*
 *   Let's use a borderless window too, so we can get vanilla
 *   keys.  This gives us a nicer way to exit and adjust the
 *   speed of the program.  But we still play with the screen
 *   bitmaps.
 */
static struct NewWindow mynewwindow = { 0, 0, 0, 0, 0, 1,
   VANILLAKEY, SIMPLE_REFRESH | ACTIVATE | NOCAREREFRESH | BORDERLESS | BACKDROP,
   NULL, NULL, NULL, NULL, NULL, -1, -1, -1, -1, CUSTOMSCREEN } ;
/*
 *   This routine gets a raster for temporary storage.
 */
short *myalloc() {
   void *AllocMem() ;
   void *p ;

   if ((p=AllocMem(2L*RASTSIZE, MEMF_CHIP | MEMF_CLEAR))==NULL) {
      printf("Could not allocate raster data\n") ;
      cleanup() ;
   }
   return(p) ;
}
/*
 *   Here we set things up.
 */
initialize() {
   long color ;

   if ((IntuitionBase = (struct IntuitionBase *)OpenLibrary(
      "intuition.library",0L))==NULL ||
       (GfxBase = (struct GfxBase *)OpenLibrary("graphics.library",0L))
      ==NULL) {
      printf("Couldn't open libraries.\n") ;
      cleanup() ;
   }
   if ((myscreen = OpenScreen(&mynewscreen))==NULL) {
      printf("Couldn't open screen.\n") ;
      cleanup() ;
   }
   mynewwindow.Screen = myscreen ;
   if ((mywindow = OpenWindow(&mynewwindow))==NULL) {
      printf("Couldn't open window.\n") ;
      cleanup() ;
   }
   a = ((short *)(myscreen->BitMap.Planes[0])) + 10 * wmodulo ;
   b = ((short *)(myscreen->BitMap.Planes[1])) + 10 * wmodulo ;
   c = ((short *)(myscreen->BitMap.Planes[2])) + 10 * wmodulo ;
   d = ((short *)(myscreen->BitMap.Planes[3])) + 10 * wmodulo ;
   e = ((short *)(myscreen->BitMap.Planes[4])) + 10 * wmodulo ;
   t1 = myalloc() ;
   t2 = myalloc() ;
   t3 = myalloc() ;
   t4 = myalloc() ;
   if (orify) {
      color = GetRGB4(myscreen->ViewPort.ColorMap, 0L) ;
      if (color & 0xf00)
         color -= 256 ;
      if (color & 0xf0)
         color -= 16 ;
      if (color & 0xf)
         color -= 1 ;
      SetRGB4(&(myscreen->ViewPort), 1L << (noplanes - 1), (color >> 8) & 15,
                (color >> 4) & 15, color & 15) ;
   }
}
/*
 *   Exit routine.
 */
cleanup() {
   if (mywindow != NULL)
      CloseWindow(mywindow) ;
   if (myscreen != NULL)
      CloseScreen(myscreen) ;
   if (IntuitionBase)
      CloseLibrary(IntuitionBase) ;
   IntuitionBase = NULL ;
   if (GfxBase)
      CloseLibrary(GfxBase) ;
   GfxBase = NULL ;
   if (t1)
      FreeMem(t1, 2L*RASTSIZE) ;
   if (t2)
      FreeMem(t2, 2L*RASTSIZE) ;
   if (t3)
      FreeMem(t3, 2L*RASTSIZE) ;
   if (t4)
      FreeMem(t4, 2L*RASTSIZE) ;
   exit(0) ;
}
#define PARITY (0x96)
#define CARRY (0xe8)
#define SPEC1 (0x6a)
#define SPEC2 (0xbe)
#define SPEC3 (0x40)
#define COPY (0xf0)
#define ORIFY (0xfc)
#define ORAC (0xfa)
/*
 *   This routine does the necessary four blits to wrap the LIFE image.
 *   Please make sure that you own the blitter when you call it!
 */
struct blitparam blitparam ;
fixit() {
   register struct blitparam *p = &blitparam ;

   p->bltsize = BLTSIZE(1, vsize) ;
   p->asource = a ;
   p->csource = a + wmodulo - 1 ;
   p->dsource = a + wmodulo - 1 ;
   p->amod = 2 * (wmodulo - 1) ;
   p->bmod = 0 ;
   p->cmod = 2 * (wmodulo - 1) ;
   p->dmod = 2 * (wmodulo - 1) ;
   p->fwm = 0x4000 ;
   p->lwm = 0xffff ;
   p->con0 = SHIFTSHIFT(14) + USEA + USEC + USED + ORAC ;
   p->con1 = 0 ;
   blit(p) ;
   p->asource = a + 2 * wmodulo - 1 ;
   p->csource = a ;
   p->dsource = a ;
   p->fwm = 0x0002 ;
   p->con0 = SHIFTSHIFT(2) + USEA + USEC + USED + ORAC ;
   blit(p) ;
   p->bltsize = BLTSIZE(wmodulo, 1) ;
   p->asource = a + wmodulo ;
   p->dsource = a + wmodulo * (long)(vsize - 1) ;
   p->amod = 0 ;
   p->cmod = 0 ;
   p->dmod = 0 ;
   p->fwm = 0xffff ;
   p->lwm = 0xffff ;
   p->con0 = USEA + USED + COPY ;
   blit(p) ;
   p->asource = a + wmodulo * (long)(vsize - 2) ;
   p->dsource = a ;
   blit(p) ;
}
/*
 *   Does one LIFE generation.  Fancy algorithm uses only 9 blits.  If
 *   anyone can improve this, please let me know.
 */
dogeneration() {
   register struct blitparam *p = &blitparam ;
/*
 *   Initialize the parameters we are not going to change.
 */
   p->fwm = 0xffff ;
   p->lwm = 0xffff ;
   p->amod = 0 ;
   p->bmod = 0 ;
   p->cmod = 0 ;
   p->dmod = 0 ;
   p->asource = a - wmodulo ;
   p->bsource = a - wmodulo + 1 ;
   p->csource = a - wmodulo ;
   p->dsource = t1 ;
   p->bltsize = BLTSIZE(wmodulo, vsize+1) ;
   p->con0 = SHIFTSHIFT(1) + USEA + USEB + USEC + USED + PARITY ;
   p->con1 = SHIFTSHIFT(15) ;
   OwnBlitter() ;
   blit(p) ;
   p->dsource = t2 ;
   p->con0 = SHIFTSHIFT(1) + USEA + USEB + USEC + USED + CARRY ;
   blit(p) ;
   p->csource = t1 + wmodulo ;
   p->dsource = t3 ;
   p->con0 = SHIFTSHIFT(1) + USEA + USEB + USEC + USED + CARRY ;
   blit(p) ;
   p->bltsize = BLTSIZE(wmodulo, vsize) ;
   p->asource = t2 ;
   p->bsource = t2 + 2 * wmodulo ;
   p->csource = t3 + wmodulo ;
   p->dsource = t4 ;
   p->con1 = 0 ;
   p->con0 = USEA + USEB + USEC + USED + CARRY ;
   blit(p) ;
   p->dsource = t3 ;
   p->con0 = USEA + USEB + USEC + USED + PARITY ;
   blit(p) ;
   p->bltsize = BLTSIZE(wmodulo, vsize+1) ;
   p->asource = a - wmodulo ;
   p->bsource = a - wmodulo + 1 ;
   p->csource = t1 + wmodulo ;
   p->dsource = t2 ;
   p->con0 = SHIFTSHIFT(1) + USEA + USEB + USEC + USED + PARITY ;
   p->con1 = SHIFTSHIFT(15) ;
   blit(p) ;
   p->bltsize = BLTSIZE(wmodulo, vsize) ;
   p->asource = t1 ;
   p->bsource = t2 + wmodulo ;
   p->csource = t3 ;
   p->dsource = t3 ;
   p->con0 = USEA + USEB + USEC + USED + SPEC1 ;
   p->con1 = 0 ;
   blit(p) ;
   p->csource = a ;
   p->dsource = t1 ;
   p->con0 = USEA + USEB + USEC + USED + SPEC2 ;
   blit(p) ;
/*
 *   Before we do the final write, we copy bits down one generation.
 */
   if (orify) {
      a = ((short *)(myscreen->BitMap.Planes[0])) + 10 * wmodulo ;
      p->asource = 
         ((short *)(myscreen->BitMap.Planes[noplanes-1])) + 10 * wmodulo ;
      p->bsource = 
         ((short *)(myscreen->BitMap.Planes[noplanes-2])) + 10 * wmodulo ;
      p->dsource = p->asource ;
      p->con0 = USEA + USEB + USED + ORIFY ;
      blit(p) ;
   }
   p->con0 = USEA + USED + COPY ;
   switch (noplanes - orify) {
case 5:
   p->asource = d ;
   p->dsource = e ;
   blit(p) ;
case 4:
   p->asource = c ;
   p->dsource = d ;
   blit(p) ;
case 3:
   p->asource = b ;
   p->dsource = c ;
   blit(p) ;
case 2:
   p->asource = a ;
   p->dsource = b ;
   blit(p) ;
default: ;
}
   p->bltsize = BLTSIZE(wmodulo, vsize-2) ;
   p->asource = t1 + wmodulo ;
   p->bsource = t3 + wmodulo ;
   p->csource = t4 + wmodulo ;
   p->dsource = a + wmodulo ;
   p->fwm = 0x7fff ;
   p->lwm = 0xfffe ;
   p->con0 = USEA + USEB + USEC + USED + SPEC3 ;
   blit(p) ;
/*
 *   Wrap, if necessary
 */
   if (torus)
      fixit() ;
   DisownBlitter() ;
}
/*
 *   Random number generator; probably not a very good one.
 */
int rnd(i)
int i ;
{
   static long seed = 323214521 ;
   long rval ;

   seed = seed * 123213 + 121 ;
   rval = (seed >> 5) & 65535 ;
   return ((i * rval) >> 16) ;
}
/*
 *   Main routine.  If called with no arguments, makes 1 bit plane screen.
 *   Otherwise, first argument is used as the number of bit planes.
 */
char buffer[200] ;
main (argc, argv)
int argc ;
char *argv[] ;
{
   register int x, y ;
   int hires = 320 ;
   int randoms = 0 ;
   int ir ;
   int dvsize = 0 ;
   struct IntuiMessage *message ;
   int code ;
   long delayval = 0 ;

   noplanes = 1 ;
   while (argc > 1 && argv[1][0]=='-') {
      argc-- ;
      argv++ ;
      if (argv[0][1]=='h' || argv[0][1]=='H') {
         if (sscanf(argv[0]+2, "%d", &hires)!=1)
            hires = 640 ;
      } else if (argv[0][1]=='r' || argv[0][1]=='R') {
         if (sscanf(argv[0]+2, "%d", &randoms)!=1)
            randoms = 1 ;
      } else if (argv[0][1]=='p' || argv[0][1]=='P') {
         noplanes = argv[0][2] - '0' ;
         if (noplanes < 1 || noplanes > 6)
            noplanes = 1 ;
      } else if (argv[0][1]=='o' || argv[0][1]=='O') {
         orify = 1 ;
      } else if (argv[0][1]=='t' || argv[0][1]=='T') {
         torus = 1 ;
      } else if (argv[0][1]=='v' || argv[0][1]=='V') {
         if (sscanf(argv[0]+2, "%d", &dvsize)!=1)
            dvsize = 0 ;
      } else if (argv[0][1]=='s' || argv[0][1]=='S') {
         delayval = -1 ;
      }
   }
   if (noplanes == 1)
      orify = 0 ;
   if (argc > 1) {
      if (argv[1][0]=='?' && argv[1][1]==0) {
         printf(
   "Usage:  life [-h[n]] [-r[n]] [-p[n]] [-o] [-t] [-v[n]] [-s] [infile]\n") ;
         cleanup() ;
      }
      if (freopen(argv[1], "r", stdin)==NULL) {
         printf("Couldn't open %s\n", argv[1]) ;
         cleanup() ;
      }
   }
   if (hires > 400) {
      vsize = 390 ;
      mynewscreen.ViewModes |= HIRES ;
   } else {
      vsize = 190 ;
   }
   modulo = hires & ~15 ;
   ir = randoms ;
   if (dvsize >= 10 && dvsize <= 600)
      vsize = dvsize ;
   wmodulo = modulo / 16 ;
   mynewscreen.Depth = noplanes ;
   mynewscreen.Width = modulo ;
   mynewscreen.Height = vsize + 10 ;
   mynewwindow.Width = modulo ;
   mynewwindow.Height = vsize + 10 ;
   if (vsize > 300)
      mynewscreen.ViewModes |= LACE ;
   initialize() ;
   readin(a) ;
   if (torus) {
      OwnBlitter() ;
      fixit() ;
      DisownBlitter() ;
   }
   while (1) {
      if (message = (struct IntuiMessage *)GetMsg(mywindow->UserPort)) {
         code = message->Code ;
         ReplyMsg(message) ;
         switch(code) {
case 27 : case 3 : case 'q' : case 'Q' : case 'x' : case 'X' :
            cleanup() ;
case '0' : case 'g' : case 'G' :
            delayval = 0 ;
            break ;
case '1' : case '2' : case '3' : case '4' :
case '5' : case '6' : case '7' : case '8' : case '9' :
            delayval = 1L << (code - '0') ;
            break ;
case ' ' :
            delayval = -1 ;
            goto doone ;
case 'S' : case 's' :
            delayval = -1 ;
default :
            break ;
         }
      }
      if (delayval) {
         if (delayval == -1)
            continue ;
         Delay(delayval) ;
      }
doone:
      if (randoms) {
         if (ir-- == 0) {
            x = rnd(modulo-2) + 1 ;
            y = rnd(vsize-2) + 1 ;
            a[y * (long)wmodulo + (x >> 4)] |= 1 << (15 - (15 & x)) ;
            ir = randoms ;
         }
      }
      dogeneration() ;
   }
   cleanup() ;
}
