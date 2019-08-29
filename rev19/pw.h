#ifndef PW_H
#define PW_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

#define MAXDOMAINS 10

struct domainstruct
{
    float xleft;
    float xright;
    float xsize;
    long minnumcritters;
    long maxnumcritters;
    long initnumcritters;
    long minfoodcount;
    long maxfoodcount;
    long maxfoodgrown;
    long initfoodcount;
    long numcritters;
    long numcreated;
    long numborn;
    long numbornsincecreated;
    long numdied;
    long lastcreate;
    long maxgapcreate;
    long foodcount;
    short ifit;
    short jfit;
    genome** fittest;
    float* fitness;
};

#endif PW_H
