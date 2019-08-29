#ifndef BARRIER_H
#define BARRIER_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// barrier.h - declaration of barrier classes


extern float barrierheight;
extern colf barriercolor;


class barrier : public gpoly
{
protected:
    float xmn;
    float xmx;
    float zmn;
    float zmx;
    float a;
    float b;
    float c;
    float f;
    float sna;
    float csa;
    void init(cfloat xa, cfloat za, cfloat xb, cfloat zb)
    {
        numpts = 4;
        setcolor(barriercolor);
        vert = new float[12];
        if (vert == NULL)
            error(1,"Insufficient memory to setup barrier vertices");
        else
        {
            position(xa,za,xb,zb);
        }
    }
public:
    barrier() { init(0.,0.,0.,-1.); }
    barrier(cfloat xa, cfloat za, cfloat xb, cfloat zb) { init(xa,za,xb,zb); }
    ~barrier() { if (vert) delete vert;}
    void position(cfloat xa, cfloat za, cfloat xb, cfloat zb)
    {
        if (vert)
        {
            float x1; float x2; float z1; float z2;
            vert[ 0] = xa; vert[ 1] = 0.;            vert[ 2] = za;
            vert[ 3] = xa; vert[ 4] = barrierheight; vert[ 5] = za;
            vert[ 6] = xb; vert[ 7] = barrierheight; vert[ 8] = zb;
            vert[ 9] = xb; vert[10] = 0.;            vert[11] = zb;
            if (xa < xb)
            {
                xmn = x1 = xa;
                xmx = x2 = xb;
                z1 = za;
                z2 = zb;
            }
            else
            {
                xmn = x1 = xb;
                xmx = x2 = xa;
                z1 = zb;
                z2 = za;
            }
            if (za < zb)
            {
                zmn = za;
                zmx = zb;
            }
            else
            {
                zmn = zb;
                zmx = za;
            }
            a = z2 - z1;
            b = x1 - x2;
            c = x2 * z1  -  x1 * z2;
            f = 1. / sqrt(a*a + b*b);
            sna = -b * f;
            if (a < 0.) sna *= -1.;
            csa =  abs(a * f);
        }
    }
    float xmin() { return xmn; }
    float xmax() { return xmx; }
    float zmin() { return zmn; }
    float zmax() { return zmx; }
    float dist(cfloat x, cfloat z) { return (a*x + b*z + c) * f; }
    float sina() { return sna; }
    float cosa() { return csa; }
};


typedef barrier* pbarrier;

declare(gdlink,pbarrier)
declare(gdlist,pbarrier)


class bxsortedlist : public gdlist(pbarrier)
{
public:
    bxsortedlist() { }
    ~bxsortedlist() { }
    void add(pbarrier a);
    void sort();
};


// end of barrier.h

#endif BARRIER_H
