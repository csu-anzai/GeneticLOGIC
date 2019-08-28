/* rand.c contains random number procedures                                  */

#include <time.h>
#include "rand.h"

/* makeSeed() makes random starting point for random generator               */
/*   to use, have a call makeSeed() anywhere before using the generator      */
void makeSeed(void)
{ unsigned int stime;
  long ltime;
  ltime=time(NULL);     
  stime=(unsigned int) ltime/2;
  srand(stime);    
}
