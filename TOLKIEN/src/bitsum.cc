//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "bitsum.h"
#include "errhndl.h"

BitSum::BitSum()
{
        unsigned char val;
        int  i, j, counts;

        if ( (pTable = new BYTE[256]) == 0 )
             (*lib_error_handler)("BitSum", "memory allocation error");

	for (i=0; i < 256; i++) {
	     for (val = i, counts = 0, j = 7; j >= 0; j--)
		  counts += (val >> j) & 1;
	     pTable[i] = counts;
	}
}

unsigned    BitSum::add(unsigned short val) const
{
	TwoBytes bb;
	int	result;

	bb.bytes = val;
	return pTable[bb.byte.hi] + pTable[bb.byte.low];
}

BitSum bitSum;
