#include "opcodes.h"
#include "soup.h"
#include <stdio.h>

unsigned char bytes[] = {
	PCR1,		/* Start addr of organism in r1 */
	NNR3, 0x08,	/* Length of organism in r3 for spawn	*/
	SCB2, 0x95,	/* Search backward for 95.  This is dest for spawn */
	SPWN,		/* Copy (r1) to (r2) for r3 bytes, strt proc at (r2) */
	JMPB, SCB2,	/* Jump back to SCB2 instruction		*/
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
