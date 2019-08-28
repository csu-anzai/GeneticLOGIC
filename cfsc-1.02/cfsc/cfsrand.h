    /* URandN and URandF each return a uniform random sample from 0 to Max. */

float URand01(), NRand1( float Mean, float SD );
#define URandN(Max)  (unsigned int)((Max+1)*URand01())  /* Expects an unsigned int: add 1 because result is truncated (not rounded) */
#define URandF(Max)  (Max*URand01())    /* Expects a float */

