/* portable.h  9-9-92  definitions for portability */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */
/*
#ifndef lint
static char     sccsid[] = "@(#)portable.h	1.30     11/16/91";
#endif
*/

#include "license.h"

#ifndef PORTABLE_H
#define PORTABLE_H

#ifdef __TRS6000__
#define unix
#endif

#ifdef __TURBOC__

#include <stdlib.h>
#include <limits.h>
#include <alloc.h>
#include <dir.h>
#include <dos.h>
#include <mem.h>
#define ANSI
#define Hp     huge  *
#define Fp     far   *
#define Np     near  *
typedef signed char      I8s;   /*  8 bit integer */
typedef unsigned char    I8u;   /*  8 bit integer */
typedef signed int       I16s;  /* 16 bit integer */
typedef unsigned int     I16u;  /* 16 bit integer */
typedef signed long      I32s;  /* 32 bit integer */
typedef unsigned long    I32u;  /* 32 bit integer */

#endif /* __TURBOC__ */

#ifdef MSDOS /* always defined by Microsoft C compiler & NDPC compiler */
             /* Microsoft C sets _osmode == OS2_MODE or DOS_MODE */

             /* for OS/2: */
             /* #define OS2_MC
                #define INCL_DOS
                #include <os2.h>
                #include <limits.h>
                #include <stdlib.h>  */

#define ANSI
#define Hp     huge  *
#define Fp     far   *
#define Np     near  *
typedef signed char      I8s;   /*  8 bit integer */
typedef unsigned char    I8u;   /*  8 bit integer */
typedef signed int       I16s;  /* 16 bit integer */
typedef unsigned int     I16u;  /* 16 bit integer */
typedef signed long      I32s;  /* 32 bit integer */
typedef unsigned long    I32u;  /* 32 bit integer */

#endif /* MSDOS */

#ifdef unix

#ifdef __GNUC__

#include <limits.h>
#define ANSI

#else  /* __GNUC__ not defined */

#define UCHAR_MAX ((unsigned char) (~0))
#define CHAR_MAX ((char) (UCHAR_MAX >> 1))
#define UINT_MAX ((unsigned short) (~0))
#define INT_MAX ((short) (UINT_MAX >> 1))
#define ULONG_MAX ((unsigned int) (~0))
#define LONG_MAX ((int) (ULONG_MAX >> 1))

#endif /* __GNUC__ */

#define Hp     *
#define Fp     *
#define Np     *
typedef char             I8s;   /*  8 bit integer */
typedef unsigned char    I8u;   /*  8 bit integer */
typedef short            I16s;  /* 16 bit integer */
typedef unsigned short   I16u;  /* 16 bit integer */
typedef int              I32s;  /* 32 bit integer */
typedef unsigned int     I32u;  /* 32 bit integer */

#endif /* unix */

#ifdef IBM3090

#define UCHAR_MAX ((unsigned char) (~0))
#define CHAR_MAX ((char) (UCHAR_MAX >> 1))
#define UINT_MAX ((unsigned short) (~0))
#define INT_MAX ((short) (UINT_MAX >> 1))
#define ULONG_MAX ((unsigned int) (~0))
#define LONG_MAX ((int) (ULONG_MAX >> 1))
#define Hp     *
#define Fp     *
#define Np     *
typedef char             I8s;   /*  8 bit integer */
typedef unsigned char    I8u;   /*  8 bit integer */
typedef short            I16s;  /* 16 bit integer */
typedef unsigned short   I16u;  /* 16 bit integer */
typedef int              I32s;  /* 32 bit integer */
typedef unsigned int     I32u;  /* 32 bit integer */

#endif /* IBM3090 */

#ifdef ANSI           /* ANSI prototyping */
#define P_(A) A
#define const const
#else                 /* non-ANSI prototyping */
#define P_(A) ()
#define const
#endif

#endif /* #ifndef PORTABLE_H */
