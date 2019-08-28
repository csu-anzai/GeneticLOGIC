//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include <math.h>
#include "errhndl.h"
#include "graycode.h"

GrayCode::GrayCode(unsigned bits)
{
	unsigned long i, gc;
        unsigned numbits;


        if ((numbits = bits) == 0)
            numbits = DEFAULT_NUMBITS;

        if (numbits >= LONGBITS)
            matsize = MAXINT;
        else
            matsize = 1 << numbits;

        if ( (pBG = new unsigned[matsize]) == NULL )
             (*lib_error_handler)("GrayCode", "memory allocation error");
        if ( (pGB = new unsigned[matsize]) == NULL )
             (*lib_error_handler)("GrayCode", "memory allocation error");

        for (i=0; i < matsize; i++) {
             gc = pBG[i] = b2g(i, INTBITS);
             pGB[gc] = i;
        }
}

unsigned long GrayCode::b2g(unsigned long bits, unsigned numbits)
{
        int j = numbits - 1;
	unsigned value = 0;
	unsigned last = 0, bit = 0;

	for (; j >= 0; j--) {
             bit = (bits >> j) & 1;
             value |= ((last != bit) << j);
             last = bit;
	}

	return value;
}

unsigned long GrayCode::g2b(unsigned long bits, unsigned numbits)
{
	unsigned i, ii, j;
	unsigned value = 0;
	unsigned bit;

	for (i = 0, ii = 1, j = numbits - 1;
	     i < j;
	     i++, ii++)
	     value |= (((bits >> i) & 1) ^ ((bits >> ii) & 1)) << i;

        value |= ((bits >> j) & 1) << j;

        return value;
}

GrayCode  tGrayCode;
