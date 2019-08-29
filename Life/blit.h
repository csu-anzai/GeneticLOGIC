/*
 *   This is the structure passed to the new
 *   blit routine that holds all of the
 *   parameters.
 */
struct blitparam {
   short con0, con1, fwm, lwm ;
   short *asource, *bsource, *csource, *dsource ;
   short bltsize ;
   short amod, bmod, cmod, dmod ;
   short cdat, bdat, adat ;
} ;
/*
 *   Defines to make things a tad easier.
 */
#define USEA (0x0800)
#define USEB (0x0400)
#define USEC (0x0200)
#define USED (0x0100)
#define EFE (0x0010)
#define IFE (0x0008)
#define FCI (0x0004)
#define DESC (0x0002)
#define BLTSIZE(x,y) (((y)<<6)+x)
#define SHIFTSHIFT(x) ((x)<<12)
