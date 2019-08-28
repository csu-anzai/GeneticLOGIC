//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __BITSUM_H )
#define __BITSUM_H

//
//      class BitSum find the number of ones in a short int
//      it is assumed that a short int is 16 bits long
//
//      this version of Bitsum is a trade off of time for space
//      please refer the file 'bitsum.h' for another version
//      which uses the table lookup approach
//
class   BitSum
{
public:
                BitSum();
                ~BitSum();
                unsigned    add(unsigned char) const;
                unsigned    add(unsigned short) const;
protected :
		static const short masks[4];
                static const char  powerOfTwo[4];
};

inline BitSum::~BitSum()
{
}

inline BitSum::BitSum()
{
}

inline unsigned BitSum::add(unsigned char val) const
{
	unsigned short nVal = val;
	return add(nVal);
}

extern  BitSum bitSum;

#endif

