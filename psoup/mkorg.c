#include "opcodes.h"
#include "soup.h"
#include <stdio.h>

unsigned char bytes[] = {
	PCR4,		/* Start addr of organism in r4 */
	NNR1, 0x77,	/* 0x77 in r1 */
	ADD4,		/* Add r4 to 0x77, leave sum in r1 */
	R1R2,		/* Put in r2.  This is destination addr for spawn */
	NNR3, 0x12,	/* Length of organism in r3 for spawn	*/
	R4R1,		/* Put start of organism (source addr) in r1 */
	SPWN,		/* Copy (r1) to (r2) for r3 bytes, strt proc at (r2) */
	R2R1,		/* Old destination addr in r1 */
	ADD4,		/* Add source addr to old destination, result in r1 */
	NNR2, 0x77,	/* 0x77 in r2 */
	ADD2,		/* Add 0x77 to destination addr */
	R1R2,		/* Put destination addr back in r2 */
	R4R1,		/* Put source addr in r1 */
	JMPB, SPWN	/* Jump back to SPWN instruction */
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
