
/* random.h: ANSI random number generator routines from P.J. Plauger's book
   stripped from: stdlib.h standard header */

/* $Id: random.h,v 1.1 1993/02/04 11:33:40 joke Exp $ */

#ifndef _RANDOM
#define _RANDOM

#define RAND_MAX	32767

#ifdef __STDC__
int rand (void);
void srand (unsigned int);
#else
int rand ( /* void */ );
void srand ( /* unsigned int */ );
#endif

extern unsigned long _Randseed;
#endif /* _RANDOM */
