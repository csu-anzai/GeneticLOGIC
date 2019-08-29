#ifndef MISC_H
#define MISC_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// misc.h: miscellaneous useful defines & short procedures


#define nl <<"\n"
#define pnl <<")\n"
#define qnl <<"\"\n"
#define nlf <<"\n"; cout.flush()
#define pnlf <<")\n"; cout.flush()
#define qnlf <<"\"\n"; cout.flush()
#define cm <<","<<
#define cms <<", "<<
#define sp <<" "<<
#define sp2 <<"  "<<
#define ses <<" = "<<

#define cfloat const float
#define cint const int
#define cshort const short
#define clong const long
#define cchar const char
#define cBoolean const Boolean
#define cvoid const void
#define uchar unsigned char
#define cuchar const unsigned char

#define forever for (;;)

#define min(a,b) (((a)<(b))?(a):(b))
#define max(a,b) (((a)>(b))?(a):(b))

#define nint(a) (long((a)+(((a)<0.0)?-0.499999999:0.499999999)))

#define interp(x,ylo,yhi) ((ylo)+(x)*((yhi)-(ylo)))

#define rrand(lo,hi) (interp(drand48(),(lo),(hi)))

#define abs(a) (((a)<0)?(-(a)):(a))

#define index2(i,j,nj) ((i)*(nj)+(j))
#define index3(i,j,k,nj,nk) ((i)*(nj)*(nk)+(j)*(nk)+(k))
#define index4(i,j,k,l,nj,nk,nl) ((i)*(nj)*(nk)*(nl)+(j)*(nk)*(nl)+(k)*(nl)+(l))

#define getbit(a,b) (((a)>>(b))&1)


extern char tempstring[256];


overload sign();
inline int   sign(int   x) { return (x<0)  ? -1  : 1 ; }
inline long  sign(long  x) { return (x<0)  ? -1  : 1 ; }
inline float sign(float x) { return (x<0.) ? -1. : 1.; }


overload concat();
char* concat(cchar* s1, cchar* s2);
char* concat(cchar* s1, cchar* s2, cchar* s3);
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4);
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5);
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5, cchar* s6);
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5, cchar* s6,
             cchar* s7);


char* itoa(long i);
char* ftoa(float f);


overload fmax();
inline float fmax(cfloat f1, cfloat f2)
    { return (f2>f1) ? f2 : f1; }
inline float fmax(cfloat f1, cfloat f2, cfloat f3)
    { return (f2>f1) ? fmax(f2,f3) : fmax(f1,f3); }
inline float fmax(cfloat f1, cfloat f2, cfloat f3, cfloat f4)
    { return (f2>f1) ? fmax(f2,f3,f4) : fmax(f1,f3,f4); }
inline float fmax(cfloat f1, cfloat f2, cfloat f3, cfloat f4, cfloat f5)
    { return (f2>f1) ? fmax(f2,f3,f4,f5) : fmax(f1,f3,f4,f5); }


overload fmin();
inline float fmin(cfloat f1, cfloat f2)
    { return (f2<f1) ? f2 : f1; }
inline float fmin(cfloat f1, cfloat f2, cfloat f3)
    { return (f2<f1) ? fmin(f2,f3) : fmin(f1,f3); }
inline float fmin(cfloat f1, cfloat f2, cfloat f3, cfloat f4)
    { return (f2<f1) ? fmin(f2,f3,f4) : fmin(f1,f3,f4); }
inline float fmin(cfloat f1, cfloat f2, cfloat f3, cfloat f4, cfloat f5)
    { return (f2<f1) ? fmin(f2,f3,f4,f5) : fmin(f1,f3,f4,f5); }


#ifndef PI
#define PI M_PI
#endif PI
#define TWOPI 6.28318530717059647602
#define HPI M_PI_2
#define RADTODEG 57.29577951
#define DEGTORAD 0.017453292


// end of misc.h

#endif MISC_H
