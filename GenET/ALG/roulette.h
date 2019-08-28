/* roulette.h */

/* createRoulette(valsP,len) */
/*   if valsP==NULL then allocate local storage for len values */
/*   else use the supplied array */
extern void *createRoulette(double *valsP, int len);

/* setRoulette(rouletteType *rP, double *val) */
/*   set up rP roulette using nonnegative values val */
extern void setRoulette(void *rP, double *val);

/* spinRoulette(rP) spins the roulette rP and returns selected index */
extern int spinRoulette(void *rP);

/* copyRoulette(to,from) copies vals from from to to roulette */
extern void copyRoulette(void *to, void *from);
  
extern void dspRoulette(void *r);
