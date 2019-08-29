#ifndef MISC_C
#define MISC_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// misc.C: miscellaneous useful short procedures


#include "basicincludes.h"


char tempstring[256];


char* concat(cchar* s1, cchar* s2)
{
    char* s = new char[strlen(s1)+strlen(s2)+1];
    strcpy(s,s1);
    strcat(s,s2);
    return s;
}
char* concat(cchar* s1, cchar* s2, cchar* s3)
{
    char* s = new char[strlen(s1)+strlen(s2)+strlen(s3)+1];
    strcpy(s,s1);
    strcat(s,s2);
    strcat(s,s3);
    return s;
}
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4)
{
    char* s = new char[strlen(s1)+strlen(s2)+strlen(s3)+strlen(s4)+1];
    strcpy(s,s1);
    strcat(s,s2);
    strcat(s,s3);
    strcat(s,s4);
    return s;
}
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5)
{
    char* s= new char[strlen(s1)+strlen(s2)+strlen(s3)+strlen(s4)+strlen(s5)+1];
    strcpy(s,s1);
    strcat(s,s2);
    strcat(s,s3);
    strcat(s,s4);
    strcat(s,s5);
    return s;
}
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5, cchar* s6)
{
    char* s= new char[strlen(s1)+strlen(s2)+strlen(s3)+strlen(s4)+strlen(s5)+
                      strlen(s6)+1];
    strcpy(s,s1);
    strcat(s,s2);
    strcat(s,s3);
    strcat(s,s4);
    strcat(s,s5);
    strcat(s,s6);
    return s;
}
char* concat(cchar* s1, cchar* s2, cchar* s3, cchar* s4, cchar* s5, cchar* s6,
             cchar* s7)
{
    char* s= new char[strlen(s1)+strlen(s2)+strlen(s3)+strlen(s4)+strlen(s5)+
                      strlen(s6)+strlen(s7)+1];
    strcpy(s,s1);
    strcat(s,s2);
    strcat(s,s3);
    strcat(s,s4);
    strcat(s,s5);
    strcat(s,s6);
    strcat(s,s7);
    return s;
}


char* itoa(long i)
{
    char* b = new char[256];
    sprintf(b,"%d\0",i);
    char* a = new char[strlen(b)+1];
    strcpy(a,b);
    delete b;
    return a;
}


char* ftoa(float f)
{
    char* b = new char[256];
    sprintf(b,"%g\0",f);
    char* a = new char[strlen(b)+1];
    strcpy(a,b);
    delete b;
    return a;
}


float logistic(cfloat x, cfloat slope)
{
    return (1.0 / (1.0 + exp(-1 * x * slope)));
}


// end of misc.C

#endif MISC_C
