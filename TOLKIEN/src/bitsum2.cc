//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "bitsum2.h"
#include "errhndl.h"

//      this version of Bitsum is a trade off of time for space
//      please refer the file 'bitsum.cpp' for another version
//      which uses the table lookup approach
//
const short BitSum::masks[4] = { 0x5555, // 0101010101010101,
				 0x3333, // 0011001100110011,
				 0x0f0f, // 0000111100001111,
				 0x00ff  // 0000000011111111
			       };

const char BitSum::powerOfTwo[4] = { 2, 4, 8, 16 };

unsigned    BitSum::add(unsigned short val) const
{
	unsigned int i;
	unsigned short wEven, wOdd;

	for (i = 0; i < 4; i++) {
	    wEven = val & masks[i];
            val >>= powerOfTwo[i];  // shift val right pow(2,i) bits
	    wOdd = val & masks[i];
	    val = wEven + wOdd;
	}
	return val;
}

BitSum bitSum;
