#ifndef ERROR_H
#define ERROR_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// error.h: definition of standard error reporting mechanism


#ifndef MISC_H
#include "misc.h"
#endif MISC_H

overload error;
extern void error(     cint level, cint delay, cchar* msg);
extern void error(     cint level, cchar* msg);
extern void errorwait( cint level, cchar* msg);
extern void errorflash(cint level, cchar* msg);
extern void error(     cint level, cchar* msg1, cchar* msg2);
extern void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3);
extern void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3,
                                   cchar* msg4);
extern void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3,
                                   cchar* msg4, cchar* msg5);
extern void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3,
                                   cchar* msg4, cchar* msg5, cchar* msg6);
extern void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3,
                                   cchar* msg4, cchar* msg5, cchar* msg6,
                                   cchar* msg7);
extern void error(     cint level, cchar* msg1, clong  num2, cchar* msg3);
extern void error(     cint level, cchar* msg1, clong  num2, cchar* msg3,
                                   cchar* msg4, cchar* msg5);
extern void error(     cint level, cchar* msg1, clong  num2, cchar* msg3,
                                   clong  num4, cchar* msg5);
extern void error(     cint level, cchar* msg1, cchar* msg2, clong  num3,
                                   cchar* msg4, clong  num5, cchar* msg6,
                                   clong  num7);

// end of error.h

#endif ERROR_H
