#ifndef GRAPHICS_H
#define GRAPHICS_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// graphics.h: declaration of all graphics classes


#define DEFAULTFOVY 90.
#define DEFAULTASPECT 0.
#define DEFAULTNEAR 0.00001
#define DEFAULTFAR 10000.
#define DEFAULTCAMERAZ 10.

const long zbnear = 0x7FFFFF;
const long zbfar = 0x0;

#include <gl/gl.h>
//#include GLFILE

#ifndef MISC_H
#include "misc.h"
#endif MISC_H

#ifndef INDEXLIST_H
#include "indexlist.h"
#endif INDEXLIST_H

#ifndef ERROR_H
#include "error.h"
#endif ERROR_H

#define cAngle const Angle

struct lpt
{
    long x;
    long y;
};

struct cols
{
    short r;
    short g;
    short b;
    short t;
};

struct colf
{
    float r;
    float g;
    float b;
    float t;
};

const colf whitecolf = { 1., 1., 1., 1. };
const colf blackcolf = { 0., 0., 0., 1. };

inline unsigned char lred(clong c) { return (c & 255); }
inline unsigned char lgreen(clong c) { return ((c >> 8) & 255); }
inline unsigned char lblue(clong c) { return ((c >> 16) & 255); }
inline unsigned char lalpha(clong c) { return ((c >> 24) & 255); }

#ifdef PIXMAP2
class pixmap2
{
public:
    pixmap2() { x = y = memsize = 0; p1 = p2 = p = NULL; }
    pixmap2(clong lx, clong ly)
    {
        x = lx;
        y = ly;
        memsize = lx * ly;
        p1 = new long[memsize];
        p2 = new long[memsize];
        if ( (p1 == NULL) || (p2 == NULL) )
            error(1,"Insufficient memory to allocate pixmap storage");
        else
            p = p1;
    }
    ~pixmap2() { delete p1; delete p2; }
    void read(clong x1, clong y1, clong x2, clong y2)
    {
        long lx = x2-x1+1;
        long ly = y2-y1+1;
        if (p != NULL)
        {
            if (lx*ly >= memsize)
            {
                delete p1;
                delete p2;
                memsize = lx * ly;
                p1 = new long[memsize];
                p2 = new long[memsize];
            }
        }
        else
        {
            memsize = lx * ly;
            p1 = new long[memsize];
            p2 = new long[memsize];
        }
        if ( (p1 == NULL) || (p2 == NULL) )
           error(1,"Insufficient memory to allocate pixmap storage");
        else
        {
            p = p1;
            x = lx;
            y = ly;
            long err = lrectread((short)x1,(short)y1,(short)x2,(short)y2,p);
            if (err == 0)
                error(1,"Error trying to read a pixmap");
        }
    }
    void resize(clong xnew, clong ynew)
    {
        long nx = x / xnew;  // find nearest integer multiple for now
        long ny = y / ynew;  // ditto
        nx = nx > 4 ? 4 : nx; // don't average down more than 4-to-1
        ny = ny > 4 ? 4 : ny; // don't average down more than 4-to-1
        long xoff = (x - nx*xnew) / 2  -  1;
        long yoff = (y - ny*ynew) / 2  -  1;
        long* pold;
        long* pnew;
        if (p == p1)
        {
            pold = p1;
            pnew = p2;
        }
        else
        {
            pold = p2;
            pnew = p1;
        }
        long iold;
        long jold;
        for (long inew = 0; inew < xnew; inew++)
        {
            for (long jnew = 0; jnew < ynew; jnew++)
            {
                iold = xoff + inew*nx;
                for (short ix = 0; ix < nx; ix++)
                {
                    iold++;
                    jold = yoff + jnew*ny;
                    for (short iy = 0; iy < ny; iy++)
                    {
                        jold++;
                        pnew[inew+jnew*xnew] += pold[iold+jold*x];
                    }
                }
            }
        }
        p = pnew;
    }
    unsigned char r(clong i, clong j) { return lred(p[i+j*x]); }
    unsigned char g(clong i, clong j) { return lgreen(p[i+j*x]); }
    unsigned char b(clong i, clong j) { return lblue(p[i+j*x]); }
    long x;
    long y;
    long memsize;
    long* p;
    long* p1;
    long* p2;
};
#endif PIXMAP2

overload lcol();
inline long lcol(cshort r, cshort g, cshort b)
    { return ((b & 255) << 16) | ((g & 255) <<  8) | (r & 255); }
inline long lcol(cint r, cint g, cint b)
    { return ((b & 255) << 16) | ((g & 255) <<  8) | (r & 255); }
inline long lcol(clong r, clong g, clong b)
    { return ((b & 255) << 16) | ((g & 255) <<  8) | (r & 255); }
inline long lcol(cfloat r, cfloat g, cfloat b)
    { return ((int(b*255.) & 255) << 16)
           | ((int(g*255.) & 255) <<  8)
           |  (int(r*255.) & 255); }


inline short scol(cfloat c) { return short(c*255.) & 255; }

inline float fcol(cshort c) { return float(c)/255.; }


#include "gdlink.h"
#include "gobject.h"
#include "glight.h"
#include "gmisc.h"
#include "gcamera.h"
#include "gstage.h"
#include "gscene.h"
#include "gwindow.h"


// end of graphics.h

#endif GRAPHICS_H
