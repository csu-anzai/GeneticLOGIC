/*
 *   Information gleaned from the Hardware Reference Manual.
 */
#define BLTADD (0xdff040L)
#include "blit.h"
/*
 *   This structure contains everything we need to know.
 *   Do not do a structure copy into this!  Instead, assign
 *   each field.  The last field assigned must be bltsize; that
 *   starts up the blitter.  Also note that all of these are
 *   write only, and you can't read them.
 */
struct bltstruct {
   short con0 ;
   short con1 ;
   short afwm ;
   short alwm ;
   short *csource, *bsource, *asource, *dsource ;
   short bltsize ;
   short dmy1, dmy2, dmy3 ;
   short cmod, bmod, amod, dmod ;
   short dmy4, dmy5, dmy6, dmy7 ;
   short cdat, bdat, adat ;
} ;
/*
 *   This is the main blit routine; it takes a structure and
 *   copies everything to the blitter registers, and starts
 *   the blit.
 */
blit(p)
register struct blitparam *p ;
{
   register struct bltstruct *d ;

   d = BLTADD ;
/*
 *   Wait for the blitter to finish whatever it needs to do.
 */
   WaitBlit() ;
/*
 *   Assign the registers.  This code runs very very quickly since
 *   everything is in registers.
 */
   d->con0 = p->con0 ;
   d->con1 = p->con1 ;
   d->afwm = p->fwm ;
   d->alwm = p->lwm ;
   d->asource = p->asource ;
   d->bsource = p->bsource ;
   d->csource = p->csource ;
   d->dsource = p->dsource ;
   d->amod = p->amod ;
   d->bmod = p->bmod ;
   d->cmod = p->cmod ;
   d->dmod = p->dmod ;
   d->adat = p->adat ;
   d->bdat = p->bdat ;
   d->cdat = p->cdat ;
/*
 *   This last assignment starts the blitter.
 */
   d->bltsize = p->bltsize ;
}
