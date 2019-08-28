/*		DSCLEARN.H  for the CFS-C Classifier System.

Define constants for the data structures in DSCLEARN.DEF and 
the code in DSCLEARN.C  for the CFS-C classifier system.

*/

#ifndef	CFLSTSZ
#include	"core.h"			/* Core system constants (STRNGSZ, CFLSTSZ, etc.) */
#endif

    /* Constants for structures for storing new classifiers and pointers to classifiers to replace. */

#if	( PERKINEL || MACLSC )
#define		NEWCFSZ		64                
#else
#define		NEWCFSZ		(CFLSTSZ / 4)	/* Size of array of nodes for new classifiers  */
#endif
#define		NEWCFMX		(NEWCFSZ-1) 

#define		NWSTRFA		1				/* new cf strength = Fraction of Average */
#define		NWSTRHPA	2				/* new cf strength = Halfway between Parent and average */
#define		NWSTRAP		3				/* new cf strength = average of parents */

#if	PERKINEL
#define		CRWDSZ		50                
#else
#define		CRWDSZ		(CFLSTSZ/4)		/* maximum crowding factor */
#endif
#define		CRWDMX		(CRWDSZ-1)

#define		RPOOLSZ		CRWDSZ
#define		RPOOLMX		(RPOOLSZ-1)

#define		MUTABSZ		5				/* Mutation prob. table size */
#define		MUTABMX		(MUTABSZ-1)

	/* Constants to pass as parameter to ReprodCf() subroutine. */

#define		REPLCF		1				/* Reproduce by replacing existing cf. */
#define		NEWCF		0				/* Reproduce by adding new cf (if there is room), or replace one. */

		/* Constants to pass as parameters to PckPrnts() subroutine. */

#define		PKSTRPNT	1				/* Pick parent from cflist, prob = strength */

#define		PKBDSPNT	2				/* Pick parent from bidders, prob = biased strength */

#define		PKCL1PNT	3				/* Pick parent from cflist, closest match on 1 condition */

#define		PKBDPNT		4				/* Pick parent from bidders--prob = biased bid */

	/* Constants for the DscCEff() subroutine */

#define		SAMEACT		1				/* generate same action string */
#define		DIFFACT		2				/* generate different action (random choice amoung those) */
#define		RANDACT		3				/* generate at random */
#define		SPECACT		4				/* specialize condition part of predictor */
