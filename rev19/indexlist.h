#ifndef INDEXLIST_H
#define INDEXLIST_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// indexlist.h: declaration of indexlist classes


class indexlist
{
private:
    long lo;
    long hi;
    long next;
    unsigned char* pind;
    long numbytes;
public:
    indexlist(clong l, clong h);
    ~indexlist();
    void dump(ostream& out);
    void load(istream& in);
    long getindex();
    void freeindex(clong i);
    Boolean isone(clong i);
    void print(clong loindex, clong hiindex);
    void print() { print(lo,hi); }
};


// end of indexlist.h

#endif INDEXLIST_H
