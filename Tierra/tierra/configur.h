/* configur.h  9-9-92  configurable parameters at compile time */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */
/*
#ifndef lint
static char     sccsid[] = "@(#)configur.h	1.5     7/21/92";
#endif
*/

#include "license.h"

#ifndef CONFIGUR_H
#define CONFIGUR_H
#define VER 4.0

#define INSTBITNUM 5
#define INSTNUM 32         /* INSTNUM = 2 ^ INSTBITNUM */
#define PLOIDY 1

#ifndef INST
#define INST 1  /* 1, 2, 3 or 4 */
#endif

#ifndef SHELL
#ifdef unix
/* DAN #define SHELL "xterm" */
#define SHELL "csh"
#else
#define SHELL "command"
#endif
#endif /* shell */

#define STDIO     0
#define BASIC     1

#if INST == 1
#define STACK_SIZE 10
#define ALOC_REG 4
#define NUMREG 4         /* NUMREG = ALOC_REG */
#endif /* INST == 1 */

#if INST == 2
#define STACK_SIZE 10
#define ALOC_REG 8
#define NUMREG 4         /* NUMREG = ALOC_REG / 2 */
#define GETBUFSIZ 3
#define PUTBUFSIZ 3
#endif /* INST == 2 */

#if INST > 2
#define STACK_SIZE 10
#define ALOC_REG 4
#define NUMREG 4         /* NUMREG = ALOC_REG */
#define GETBUFSIZ 3
#define PUTBUFSIZ 3
#endif /* INST > 2 */

#ifndef FRONTEND
#define FRONTEND BASIC  /* BASIC or STDIO */
#endif

#ifdef __TURBOC__
#define FRONTEND BASIC
/* #define MEM_CHK */   /* define for DOS external bounds checker */
#endif

/* #define CM5       */ /* CM5 the version for the CM5 */
#define MICRO        /* define for micro step debugging */
/* #define HSEX      */ /* define for haploid, crossover sex */
#define ICC          /* define for inter-cellular communication, != I/O */
/* #define READPROT  */ /* define to implement read protection of soup */
#define WRITEPROT    /* define to implement write protection of soup */
/* #define EXECPROT  */ /* define to implement execute protection of soup */
/* #define ERROR     */ /* use to include error checking code */
/* #define ALCOMM    */ /* define for socket communications */
/* #define MEM_PROF  */ /* profile dynamic memory usage */

/* the following definitions should be provided by the compilers,
   except for IBM3090 which must be manually defined */

/* __TURBOC__  = Turbo C for DOS */
/* OS2_MC      = Microsoft C for OS/2 */
/* IBM3090     = IBM 3090 the compiler will not define this, do it manually */
/* unix        = for unix systems */
/* __GNUC__    = gcc unix compiler */
/* ANSI        = for ANSI environment */

#endif
