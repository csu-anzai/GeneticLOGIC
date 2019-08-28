/* rand.h */

#include <stdlib.h>

#ifndef RAND_MAX
#define RAND_MAX 32767
#endif

/* binRand(void) returns randomly 0 or 1                                     */
#define binRand() ((rand()< RAND_MAX/2) ? 0 : 1)
 
/* probRand() returns a double value on <0,1>                                */
#define probRand() ((double)rand()/RAND_MAX)
 
/* intRand(up) returns an integer on <0..up>                                 */
#define intRand(up) (((int) (rand()/273))%((int) (up)+1))
 
/* doubleRand(u) returns double on [0..u]                                    */
#define doubleRand(u) (probRand()*(u))

/* makeSeed() makes random starting point for random generator               */
/*   to use, have a call makeSeed() anywhere before using the generator      */
extern void makeSeed(void);

