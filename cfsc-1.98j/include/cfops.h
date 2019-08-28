
/*		CFOPS.H  for the CFS-C Classifier System.

Constants for classifier operators.
Used primarily by CFOPS.DEF, CFOPS.EXT, and CFOPS.C files.

*/

#define		OC_PASS		0	/* Standard "pass-through" */
#define		OC_AND		1
#define		OC_OR		2
#define		OC_XOR		3
#define		OC_ADD		4
#define		OC_ADD1		5
#define		OC_SUB		6
#define		OC_SUB1		7

#define		OC_ENDL		8	/* end-of-list */

/* Define size of operator 'registers', i.e., the number of
   of loci on right-most end of operand message(s). */

#define		PASSSZ		STRNGSZ
#define		ANDSZ		STRNGSZ
#define		ORSZ		STRNGSZ
#define		XORSZ		STRNGSZ

#define		ADDSZ		(LOCIPRI-8)
#define		ADD1SZ		4
#define		SUBSZ		(LOCIPRI-8)
#define		SUB1SZ		4
