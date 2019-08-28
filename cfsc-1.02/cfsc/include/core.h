/*		CORE.H	for the CFS-C Classifier System.
 
Global #define'd constants for the major CFS-C classifier system data structures.
These are used in the file CORE.DEF to define the basic  classifier system data structures,
and these must be used in any functions that access those data structures.

NB: Pick a machine to control the compilation of dependent constants and code.
	The #define's for machine type are in the file UTILITY.H .

*/

#ifndef STRNGSZ
#if __HIGHC__
#define     STRNGSZ     32
#else 
#define		STRNGSZ		16				/* Basic number of loci in messages, conditions, action-parts, etc. */
#endif
#endif
#define		STRNGMX		(STRNGSZ-1)

#if	( STRNGSZ > INTSZ )
#define		INTPRSTR	(STRNGSZ/INTSZ)	/* The number of integers needed to represent one string. */
#define		LOCIPRI		INTSZ			/* Number of loci represented by an int variable. */
#else
#define		INTPRSTR	1				/* If STRNGSZ <= INTSZ all loci fit in one int */
#define		LOCIPRI		STRNGSZ
#endif

#ifndef     MSGLSTSZ
#define		MSGLSTSZ	64				/* Maximum number of  messages. */
#endif
#define		MSGLSTMX	(MSGLSTSZ-1)	/* NOTE: If you change MSGLSTSZ, change NmIMsgMx and NmDMsgMx in CORE.DEF */

#define		DMSGSSZ		 4				/* Maximum number of messages from detectors. */
#define		DMSGSMX		(DMSGSSZ-1)

#ifndef CFLSTSZ
#if	( __HIGHC__ || DECSTATION )
#define		CFLSTSZ		1024			/* CFLSTSZ is maximum number of classifiers */
#else
#if	( APOLLOC || SUN3 || VAX )
#define		CFLSTSZ		1024			/* CFLSTSZ is maximum number of classifiers */
#else 
#if	CBELLMTS
#define		CFLSTSZ		1024
#else 
#if	( M_I86LM || MPWC )
#define		CFLSTSZ		 400
#else
#define		CFLSTSZ		200
#endif /* default size 200 */
#endif /* Microsoft Large Size or MPW C */
#endif /* Apollo, Sun, etc. size */
#endif /* HighC */
#endif /* not already defined */

#define		CFLSTMX		(CFLSTSZ-1)

#define		INTPRML		(MSGLSTSZ/INTSZ)	/* Number of integers to store 1 / bit per message */

#define		CFNAMESZ	1				/* Buffer size for storing 'name' of each classifier. **** NOT USED ****  */
#define		CFNAMEMX	(CFLSTSZ-1)

#define		EFFLSTSZ	6				/* number of effectors (i.e., EffNode's in the EffLst. */
#define		EFFLSTMX	(EFFLSTSZ-1)
#define		EFFNAMSZ	16				/* size of Storage for effector names */
#define		EFFNAMMX	(EFFNAMSZ-1)


#define		CMATCH		0				/* Condition type "Match" code */
#define		CNOTMATCH	1				/* Condition type "Not-Match" code */

#define		MSGBUFSZ	STRNGSZ+60		/* Size of buffer for message display */

#if	M_I86MM
#define		CFBUFSZ		(STRNGSZ*3)+200	/* Size of buffer for classifier display */
#else
#define		CFBUFSZ		(STRNGSZ*3)+500
#endif

#define		DSCFLSZ		32				/* size of array for displaying selected classifiers */
#define		DSCFLMX		(DSCFLSZ-1)   
