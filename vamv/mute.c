/*
 * Copyright 1990 Rick McGowan
 */
#include <stdio.h>
#include "prog.h"
#ifndef MARK	/* for profiling */
#define MARK(x)
#endif

static int bitlist[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };

/*
 * mutate an organism in place: remove the exp, then mutate the genes, then
 * re-process.
 */

mutate(o)
	register struct org *o;
{
	register int i, j, k;

	mutpref(o);

MARK(mute1);
	k = range(4) + 1;
	while (k--) {
	    if (probability(75)) {
		i = range(NGENE);
		if (probability(50)) {	/* one bit flipped on */
			o->prog[i] = o->prog[i] | bitlist[rand() % 8];
		}
		else { /* one bit flipped off */
			o->prog[i] = o->prog[i] & ~(bitlist[rand() % 8]);
		}
	    }
	}
MARK(mute2);
	if (probability(60)) {	/* n bits flip */
		i = range(8);
		j = range(NGENE);
		if (probability(50))
			o->prog[j] = o->prog[j] & ~(bitlist[rand() % 8]);
		else
			o->prog[j] = o->prog[j] | bitlist[rand() % 8];
	}
/*
 * keep probability of new random gene higher than lost gene
 */
MARK(mute3);
	if (probability(25)) {	/* lost gene */
		i = range(NGENE-10) + 5;
		for (j = i; j < NGENE-1; j++)
			o->prog[j] = o->prog[j+1];
		o->prog[NGENE-1] = range(256);
		if (probability(15))
			return;
	}
MARK(mute4);
	if (probability(35)) {	/* swapped genes */
		i = range(NGENE);
		j = range(NGENE);
		k = o->prog[i];
		o->prog[i] = o->prog[j];
		o->prog[j] = k;
		if (probability(20))
			return;
	}
MARK(mute5);
	if (probability(25)) {	/* gene randomized */
		i = range(NGENE);
		o->prog[i] = range(256);
	}
}

mutpref(o)
	register struct org *o;
{
	register int i, j, k;

/*
 * keep probability of new random gene higher than lost gene
 */
MARK(mutpref1);
	if (probability(15)) {	/* swapped genes */
		i = range(NPREF);
		j = range(NPREF);
		k = o->pref[i];
		o->pref[i] = o->pref[j];
		o->pref[j] = k;
		if (probability(30))
			return;
	}
MARK(mutpref2);
	if (probability(20)) {	/* gene randomized */
		i = range(NPREF);
		o->pref[i] = range(16);
	}
}

