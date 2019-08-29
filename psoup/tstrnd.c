/*
 * Test the new random number generator
 */
#include <stdio.h>

long counts[262144];
long countcounts[50];

void
main()
{
	long i;
	unsigned long r;

	for (i = 0; i < 256; i++) {
		counts[i] = 0L;
	}
	rndseed(time( (long *) 0 ));
	for (i = 0; i < 1000000; i++) {
		(void) awcrnd();
	}
	i = 262144 * 16;
	do {
		r = rnd(262144);
		counts[r]++;
	} while (--i);
	for (i = 0; i < 50; i++) {
		countcounts[i] = 0L;
	}
	for (i = 0; i < 262144; i++) {
		countcounts[counts[i]]++;
	}
	for (i = 0; i < 50; i++) {
		printf("%ld ", countcounts[i]);
	}
	printf("\n");
}
