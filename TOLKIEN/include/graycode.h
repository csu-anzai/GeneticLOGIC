//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __GRAYCODE_H )
#define __GRAYCODE_H

#include "tdefs.h"
#include "tvalues.h"

class   GrayCode
{
public:
                        GrayCode(unsigned = DEFAULT_NUMBITS);
        virtual         ~GrayCode();

        virtual unsigned long asGray(unsigned long);
        virtual unsigned long asBin(unsigned long);

        virtual unsigned long b2g(unsigned long, unsigned = LONGBITS);
        virtual unsigned long g2b(unsigned long, unsigned = LONGBITS);

private:

        unsigned matsize;
        unsigned *pBG;   // binary to graycode
        unsigned *pGB;   // graycode to binary
};

inline GrayCode::~GrayCode()
{
	if (matsize > 0) {
            delete [] pBG;
            delete [] pGB;
        }
}

inline unsigned long GrayCode::asGray(unsigned long bits)
{
        if (bits < matsize)
            return pBG[bits];
        else
            if (bits > MAXINT)
                return g2b(bits, LONGBITS);
            else
                return g2b(bits, INTBITS);

}

inline unsigned long GrayCode::asBin(unsigned long bits)
{
	if (bits < matsize)
            return pGB[bits];
        else
            if (bits > MAXINT)
                return g2b(bits, LONGBITS);
            else
                return g2b(bits, INTBITS);
}

extern  GrayCode tGrayCode;

#endif

