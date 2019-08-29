#ifndef INDEXLIST_C
#define INDEXLIST_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// indexlist.C: declaration & implementation of indexlist classes


#include "basicincludes.h"
#include "error.h"

#include "indexlist.h"


indexlist::indexlist(clong l, clong h)
{
    if (l < h) { lo = l; hi = h; } else { lo = h; hi = l; }
    numbytes = (hi - lo + 8) >> 3;  // >> 3 == / 8
    pind = new unsigned char[numbytes];
    if (!pind)
    error(2,"Unable to allocate memory for new indexlist; program terminating");
    for (long i = 0; i < numbytes; i++)
        pind[i] = 0;
    next = lo;
}


indexlist::~indexlist()
{
    if (pind) delete pind;
}


void indexlist::dump(ostream& out)
{
    out << lo sp hi sp next sp numbytes nl;
    if (!pind)
    {
        error(1,"dumping an indexlist with no index storage");
        return;
    }
    for (register int i = 0; i < numbytes; i++)
        out << (int)(pind[i]) nl;
}


void indexlist::load(istream& in)
{
    long newbytes;
    in >> lo >> hi >> next >> newbytes;
    if (pind && (newbytes != numbytes))
    {
        char msg[256];
        sprintf(msg,
          "numbytes in indexlist load (%d) not equal to existing numbytes (%d)",
           newbytes,numbytes);
        error(1,msg);
        delete pind;
        pind = NULL;
    }
    if (!pind)
    {
        numbytes = newbytes;
        pind = new unsigned char[numbytes];
        if (!pind)
            error(2,"Insufficient memory for indexlist during load");
    }
    int num = 0;
    for (register int i = 0; i < numbytes; i++)
    {
        in >> num;
        pind[i] = (unsigned char)num;
    }
}


long indexlist::getindex()
{
    if (next < lo)  // no more slots left
        return next;
    long byte = (next - lo) >> 3;  // 0-based  ( >> 3 == / 8 )
    unsigned char bit = (unsigned char)((next - lo) % 8);//0-based,from left
    pind[byte] |= 1 << (7 - bit);
    long curr = next;
    // following is < hi, not <= hi, because we don't want to wrap back
    // around to curr.
    for (long i = lo; i < hi; i++)  // try next, but search entire list
    {
        next++;
        if (next > hi) next = lo;
        long byte = (next - lo) >> 3;  // 0-based  ( >> 3 == / 8 )
        unsigned char bit = (unsigned char)((next - lo) % 8);//0-based,from left
        if (!((pind[byte] >> (7 - bit)) & 1))  // found an available slot
        {
            return curr;
        }
    }
    next = lo - 1; // search failed
    return curr;
}


void indexlist::freeindex(clong i)
{
    if ((i < lo) || (i > hi))
    {
        char msg[256],num[16];
        sprintf(msg,"Attempt to free out-of-range index: %d not in [%d,%d]",
            i,lo,hi);
        error(1,msg);
    }
    else
    {
        long byte = (i - lo) >> 3;  // 0-based  ( >> 3 == / 8 )
        unsigned char bit = (unsigned char)((i - lo) % 8); //0-based, from left
        pind[byte] &= ~(1 << (7 - bit));
        if ( (i < next) || (next < lo) )
            next = i;
    }
}


Boolean indexlist::isone(clong i)
{
    if ((i < lo) || (i > hi))
    {
        char msg[256],num[16];
        sprintf(msg,"isone request for out-of-range index: %d not in [%d,%d]",
            i,lo,hi);
        error(1,msg);
    }
    else
    {
        long byte = (i - lo) >> 3;  // 0-based  ( >> 3 == / 8 )
        unsigned char bit = (unsigned char)((i - lo) % 8); //0-based, from left
        return (Boolean)((pind[byte] >> (7 - bit)) & 1);
    }
}


void indexlist::print(clong loindex, clong hiindex)
{
    cout << "indexlist bits " << loindex << " through " << hiindex << " =" nl;
    for (long i = loindex; i <= hiindex; i++)
    {
        long byte = (i-lo) >> 3; // 0-based
        long bit = (i-lo) % 8; // 0-based,from left
        cout << ((pind[byte] >> (7-bit)) & 1);
    }
    cout nlf;
}


// end of indexlist.C

#endif INDEXLIST_C
