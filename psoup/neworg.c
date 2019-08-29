#include "opcodes.h"
#include "soup.h"
#include <stdio.h>

unsigned char bytes[] = {

	LBL5,		/* Mark start of organism */
	LBL1,		/* Branch destination */
	CLLF, LBL2,	/* Call the routine that sets up source addr and len */
	CLLF, LBL3,	/* Call the routine that bumps the dest addr */
	CLLF, LBL4,	/* Call the routine that spawns */
	JMPB, LBL1,	/* Loop back */
	LBL3,
	PSH1,		/* Save r1 */
	PCR1,		/* Current PC in r1 */
	SCB1, LBL5,	/* Start addr in r1 */
	ADD2,		/* Add destination addr */
	NNR2, 0xf0,	/* Get byte offset */
	ADD2,		/* Add to destination addr */
	R1R2,		/* Put destination addr back */
	POP1,		/* Restore r1 */
	RETN,		/* Return */
	LBL4,
	SPWN,		/* Make a baby */
	RETN,		/* Return */
	LBL2,
	PSH2,		/* Save r2 */
	PCR1,		/* Get current PC in r1 */
	NNR2, 0x05,	/* Point past SCF1 instruction */
	ADD2,
	SCF1, LBL6,	/* Get end of organism in r1 */
	PCR2,		/* Current PC in r2 */	
	SCB2, LBL5,	/* Get addr of last SCB1 operand */
	SCB2, LBL5,	/* Get start addr of organism */
	PSH2,		/* Save start addr */
	SUB2,		/* Subtract end - start */
	INC1,		/* Length of organism in r1 */
	R1R3,		/* Move to r3 */
	POP1,		/* Start addr in r1 */
	POP2,		/* Restore r2 */
	RETN,		/* Return */
	LBL6
};

char *name = "org.out";

void
main()
{
	FILE *fp;
	int nitems;

	if ( (fp = fopen(name, WMODE)) == NULL ) {
		printf("Open error\n");
		exit();
	} else {
		nitems = fwrite(bytes, 1, sizeof(bytes), fp);
		printf("%d bytes written.\n", nitems);
	}
}
