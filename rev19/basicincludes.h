#ifndef BASICINCLUDES_H
#define BASICINCLUDES_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/


#include <libc.h>
//#include <gl/gl.h>
#include GLFILE
#include <gl/get.h>
#include <gl/cg2vme.h>
#include <gl/addrs.h>
#include <device.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <ctype.h>
#include <stdarg.h>
#include <stream.h>
#include <stdio.h>
#include <signal.h>

#include "misc.h"
#include "error.h"

extern "C"
{
    int mpin(char* addr, unsigned int len);
    int munpin(char* addr, unsigned int len);
}


#endif BASICINCLUDES_H
