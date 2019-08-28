/*		UTILITY.H	for the CFS-C Classifier System.
 
Definitions of general-purpose constants used everywhere.

*/
	/*  Set  switches to control compilation of various things. */

#ifndef	CTEST
#define		CTEST		0	/* 1 => compile test-messages code. */
#endif
#define		GENLOG		0	/* 1 => compile code to write geneology log */
#define		CDSC_ALL	1	/* 1 => compile discover code and calls to it  */
#define		SINGLEXO	0	/* 1 => complie CrFullCf() in DSCOPS.C, 0 => DCrFullCf() */
#define		CRANDCFS	1	 /* 1 => compile code that generates random cfs */

#define		CMAXCFSTR	1	/* 1 => compile code to enforce a maximum classifier strength */

#define		CMINCFSTR	0	/* 1 => compile code to enforce a minimum classifier strength */

	/*	Control Constants  */

#define		ADOFF		UINTMAX	/* AutoDisplay off (set ADxxxInt to this to give biigg interval */

	/*	Output Format control constants */

#define		DEMOFF		0	/* display and/or demonstration turned off */
#define		DEMOOFF		0
#define		DSPLOFF		0

#define		DFLTFMT		1
#define		DEMOFMT1	DFLTFMT	/* Bassic Demonstation format (for all relevant displays) */

#define		DEMOFMT2		2	/* 2..9 are, in general, for more detailed displays */
#define		DEMOFMT3		3
#define		DEMOFMT4		4
#define		DEMOFMT5		5
#define		DEMOFMT6		6
#define		DEMOFMT7		7
#define		DEMOFMT8		8
#define		DEMOFMT9		9
#define		GENFMT			10	/* Format for 'geneology' records */
#define		ENVFMT1			21	/* Domain specific format */
#define		ENVFMT10		30
#define		SAVEFMT			99	/* Format for SAVE... commands */
#define		DEBUGFMT		999	/* General debug format --- displays lots of info. */

	/* The following are old format constants--these should be removed from all code... */

#define		DEMON			1	/* demo on - compile all that code */
#define		DEMDEBUG		999
#define		DEMOFMT			1
#define		SHORTFMT 		2         

	/* Note: One of STRSORTFMT and IDSORTFMT should be set to DEMOFMT, the other to some unused number,
			depending on what demonstrations should look like 
	*/
#define		STRSORTFMT		1	/* display classifiers in order from low strength to high */
#define		IDSORTFMT		4	/* display classifiers in Id order */


    /* Some other constants */

#define		OK			1
#define		YES			1
#define		TRUE		1
#define		ON			1
#define		NO			0
#define		FALSE		0
#define		OFF			0
#define		ERROR		-1
#define		LSB			01
 
	/*	Macro "Functions" */

#ifndef	abs 
#define		abs(a)		((a)>0?(a):-(-a))
#endif

#ifndef min
#define		min(a,b)	((a)<=(b)?(a):(b))
#define		max(a,b)	((a)>(b)?(a):(b))
#endif

    /* URandN and URandF each return a uniform random sample from 0 to Max. */
float URand01();
#define URandN(Max)  (unsigned int)((Max+1)*URand01())  /* Expects an unsigned int: add 1 because result is truncated (not rounded) */
#define URandF(Max)	 (Max*URand01())    /* Expects a float */

	/*	Characters */
 
#define		CR			'\015'
#define		LF			'\012'
#define		BACKSPAC	'\b'
#define		DELETE		'\177'
#define		ATTN		'\005'

#ifndef	NULL 
#define		NULL		'\0'
#endif
