//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __TVALUES_H )
#define __TVALUES_H

#include <limits.h>
#include <errno.h>

#define BITSPERBYTE 8
#define BITS(type)  (BITSPERBYTE * (int)sizeof(type))
#define CHARBITS    BITS(char)
#define SHORTBITS   BITS(short)
#define INTBITS     BITS(int)
#define LONGBITS    BITS(long)
#define PTRBITS     BITS(char*)
#define DOUBLEBITS  BITS(double)
#define FLOATBITS   BITS(float)

#if defined(_MSC_VER) || defined(__BORLANDC__)
#include <values.h>
#define HUGE	    HUGE_VAL
#endif

#ifndef MINDOUBLE
#define MINDOUBLE   4.94065645841246544e-324
#endif
#ifndef MAXDOUBLE
#define MAXDOUBLE   1.79769313486231470e+308
#endif

#ifndef MINFLOAT
#define MINFLOAT    ((float)1.40129846432481707e-45)
#endif
#ifndef MAXFLOAT
#define MAXFLOAT    ((float)3.40282346638528860e+38)
#endif

#ifndef MINSHORT
#define MINSHORT    ((short)(1 << (SHORTBITS - 1)))
#endif
#ifndef MAXSHORT
#define MAXSHORT    ((short)~MINSHORT)
#endif

#ifndef MININT
#define MININT      (1 << (INTBITS - 1))
#endif
#ifndef MAXINT
#define MAXINT      (~MININT)
#endif

#ifndef MINLONG
#define MINLONG     (1L << (LONGBITS - 1))
#endif
#ifndef MAXLONG
#define MAXLONG     (~MINLONG)
#endif

#endif

