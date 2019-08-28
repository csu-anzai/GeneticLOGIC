/*		FSW1.H - constants
*/

#if M_I86LM
#define		STATEBTS	8			/* bits to represent all states */
#define		STATESZ		300			/* max number states = 2 ** STATEBTS */
#else
#define		STATEBTS	 7			/* bits to represent all states */
#define		STATESZ		128			/* max number states = 2 ** STATEBTS */
#endif
#define		STATEMX		(STATESZ-1)

#define		EFFATTSZ	4			/* maximum number of settable "effector" attributes */
#define		EFFATTMX	(EFFATTSZ-1)
#define		EFFVALMX	15 			/* (2 ** EFFATTSZ) - 1 */

#define		DFTEFFVA	255			/* Default effector setting */

#define		STRATTSZ	(EFFATTSZ*2)	/* a pair of loci for setting each attribute */
#define		STRATTMX	(STRATTSZ-1)

