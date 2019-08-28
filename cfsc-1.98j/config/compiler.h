

/**		COMPILER.H for the CFS-C Classifier System.

This file defines switches to control machine/comipler dependent differences in the CFS-C system.
It must be included *first* in every ?*.C file.

Set one of the following swithces to 1, and the rest to 0, to choose a particular
machine/compiler.  If your machine/compiler is not here, try one that seems close.
If you need to add code that is distinct to your machine/compiler, add a switch and
tell me about what changes you make and I will add it to future versions of the code.

LATTICEC	IBM PC/XT/AT and compatibles  - Lattice 3.00F and beyond.
              Also use this for Microsoft C, Version 4.0 and Version 5.0
                  (This switch needs to split into several...)
APOLLOC		APOLLO workstations (and their C compiler).
MACAZTEC	Apple MacIntosh - Aztec C for the Mac is the compiler.
CI86		CI 86 compiler for ibm-pc type machines.
PERKINEL	for Perkin-Elmer 3220 (Unix V7) machine at Vanderbilt.
              of for SUN3 machine at NRL
VAX			Vax C
SUN3		sun3 and sun4 targets
CBELLMTS	Bell Labs C on MTS on an IBM 3090 (of all machines!)
MPWC		MacInstosh Programmers Workshop C (from APDA)
MACLSC		Mac LightSpeed C

===>SPECIAL NOTES for certain compilers:
1. MACLSC can't do the calcuations of NEWCFSZ and MutdSpec in DSCLEARN.DEF,
 	so put the numbers in by hand when STRNGSZ or CFLSTSZ is changed.
2. NEWCFSZ can't be calculated by PERKINEL either.
3. SUN's (or some of them) have toupper() and tolower() that don't first
	check to see if the conversion needs to be done.
	The symptom is that ALL commands are not recognized.
	If your computer/compiler has that problem, check out toupper/tolower
	as used in USERCMD.C and CFSIO.C .

*/

#define  LATTICEC	0			/* Be sure to pick ONLY ONE */
#define  APOLLOC	0
#define  MACAZTEC	0
#define  CI86		0
#define  PERKINEL	0
#define  VAX		0
#define  SUN3		1
#define  CBELLMTS	0
#define  MPWC		0
#define  MACLSC		0


#if  LATTICEC
#define		INTSZ		16	/* bit length of unsigned int */
#define		USINTMAX	65535	/* largest unsigned int value */
#define		UINTMAX		USINTMAX
#define		INTMAX		32767	/* largest signed int value */
#define		BIGFLOAT	9.999e37/* a big-enough float value */
#define		VOID		void
#endif

#if  APOLLOC
#define		INTSZ		32
#define		USINTMAX	4294967295
#define		UINTMAX		USINTMAX
#define		INTMAX		2147483647
#define		BIGFLOAT	9.999e37
#define		VOID		int
#endif

#if  MACAZTEC
#define		INTSZ		16
#define		USINTMAX	65535
#define		UINTMAX		USINTMAX
#define		INTMAX		32767
#define		BIGFLOAT	9.999e37
#define		VOID		int
#endif

#if  CI86
#define		INTSZ		16
#define		USINTMAX	65535
#define		UINTMAX		USINTMAX
#define		INTMAX		32767
#define		BIGFLOAT	9.999e37
#define		VOID		int
#endif

#if  PERKINEL
#define		INTSZ		32
#define		USINTMAX	4294967295
#define		UINTMAX		USINTMAX
#define		INTMAX		2147483647
#define		BIGFLOAT	9.999e37
#define		VOID		int
#endif

#if  SUN3
#ifdef __STDC__
#include <stdlib.h>
#include <limits.h>
#include <float.h>
#endif
#define		INTSZ		32
#define		USINTMAX	UINT_MAX
#define		UINTMAX		UINT_MAX
#define		INTMAX		INT_MAX
#define		BIGFLOAT	FLT_MAX
#define		VOID		void
#define		getch()		getchar()
#endif

#if  VAX
#define		INTSZ		32
#define		USINTMAX	4294967295
#define		UINTMAX		USINTMAX
#define		INTMAX		2147483647
#define		BIGFLOAT	9.999e37
#define		VOID		int
#endif

#if  CBELLMTS
#define		INTSZ		32
#define		USINTMAX	 4294967295
#define		UINTMAX		USINTMAX
#define		INTMAX		2147483647
#define		BIGFLOAT	9.999e37
#define		VOID		void
#endif

#if  MPWC
#define		INTSZ		32
#define		USINTMAX	4294967295
#define		UINTMAX		USINTMAX
#define		INTMAX		2147483647
#define		BIGFLOAT	9.999e37
#define		VOID		void
#define		getch()		getchar()
#endif


#if  MACLSC
#define		INTSZ		16
#define		USINTMAX	65535
#define		UINTMAX		USINTMAX
#define		INTMAX		32767
#define		BIGFLOAT	9.999e37
#define		VOID		void
#define		_MC68881_
#define		_ERRORCHECK_
#endif
