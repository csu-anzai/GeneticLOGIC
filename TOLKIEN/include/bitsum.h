//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __BITSUM_H )
#define __BITSUM_H

#include "tdefs.h"

//
//      class BitSum find the number of ones in a short int
//      it is assumed that a short int is 16 bits long
//
//      this version of bitsum is a trade off of space for time
//      please refer the file 'bitsum2.h' for another version
//      which uses the logarithmic sum approach
//
class   BitSum
{
public:
                BitSum();
                ~BitSum();
                unsigned    add(unsigned char) const;
                unsigned    add(unsigned short) const;

private:

	union TwoBytes {
		struct {
			unsigned char hi, low;
		} byte;
		unsigned short bytes;
	}; // assumed that a short int is two bytes long

        BYTE *pTable;   // look up table for bit sum
};

inline BitSum::~BitSum()
{
       delete [] pTable;
}

inline unsigned  BitSum::add(unsigned char val) const
{
	return pTable[val];
}

extern  BitSum bitSum;

#endif

