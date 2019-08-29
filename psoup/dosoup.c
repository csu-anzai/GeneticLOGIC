/* Guts of Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 01Feb92 MdG	0.3d	Changed population low-water and maximum messages.
 */

#include "soup.h"
#include "organism.h"
#include "protos.h"
#include <stdio.h>

/* Externs */
extern	unsigned char *soup;
extern	ORGANISM	organisms[];
extern	long		births;
extern	long		deaths;
extern	long		curr;
extern	long		instructions;
extern	short		monflag;
extern	long		mutate_counter;
extern	long		mutate_limit;
extern	short		adjflag;
extern	long		heartbeat;
extern	long		lifetime;
extern	long		notify;
extern	long		watch;
extern	long		noparentflag;
extern	long		noparentlength;
extern	long		seesaw;

#ifdef	GBASE
extern	short		gbaseflag;
#endif

void
do_soup()
{
	for(;;) {

		/* Make the next living organism the current one */
		do {
			curr++;
			if (curr >= NORGANISMS) {
				curr = 0;
			}

			/* If no organisms, and not running adjust_population,
			 * print msg and enter monitor
			 */
			if (adjflag == 0 && births - deaths == 0) {
				printf("*** There is no life in the soup.\n");
				monflag = 1;	/* Force monitor invokation */
			}

#ifdef MSDOS
			if ( kbhit() ) {
				monflag = 1;
			}
#endif

			/* If SIGINT occurred on Unix systems, or if a key was
			 * pressed on MSDOS systems, monflag got set.
			 * Run the monitor.
			 */
			if (monflag) {
				mon();
				monflag = 0;
			}

			/* If below minimum population (low-water mark)
			 * spawn processes till high-water mark reached.
			 */
			adjust_population();
		} while (organisms[curr].alive == 0);

		/* If human is following this organism, disassemble */
		if (watch == curr) {
			printf("%8.8lX %8.8lX %8.8lX %8.8lX -- ",
				organisms[curr].r1, organisms[curr].r2,
				organisms[curr].r3, organisms[curr].r4);
			printf("%8.8lX: ", organisms[curr].pc);
			disassemble(&soup[organisms[curr].pc],
						organisms[curr].pc,
						(short) 1, (short) 0);
			printf("\n");
		}

		execute();	/* Execute one instruction	*/

		/* If the organism is getting to an advanced age,
		 * let the human know.
		 */
		if (lifetime - organisms[curr].lifetime >= notify) {
			printf("*** Organism %6ld is %3ld instructions old.",
				organisms[curr].number,
				lifetime - organisms[curr].lifetime);
			printf(" pc=%8.8lX  population=%3ld\n",
				organisms[curr].pc, births - deaths);
		}

		/* If no heartbeat or too old, kill organism */
		if (organisms[curr].heartbeat == 0 ||
					organisms[curr].lifetime == 0) {
			kill_organism(curr);
			if (watch == curr) {
				watch = -1;
			}
		}

		/* Simulate mutation from background radiation */
		if (--mutate_counter == 0) {
			mutate_counter = mutate_limit;
			mutate();
		}
	}
}

void
adjust_population()
{
	if (adjflag == 0) {
		return;
	}
	if (births - deaths < LOW_WATER) {

		/* If at low-water, and seesaw indicates that last
		 * message was for max. population, print message
		 * and toggle seesaw.
		 */
		if (seesaw) {
printf(
"*** Instructions = %lu.  Population has reached the low-water mark.\n",
instructions);
			seesaw = 0;
		}

		do {
			noparentflag = 1;
			noparentlength = 0L;
			(void) spawn(rnd(SOUPLEN));
			noparentflag = 0;
		} while (births - deaths < HIGH_WATER);
	}
}

int
spawn(newpc)
long newpc;
{
	long	i;
	long	j;

	/* Find a dead organism */
	for (i = 0; i < NORGANISMS; i++) {
		if (organisms[i].alive == 0) {
			break;
		}
	}

	/* If there are none dead, just forget it */
	if (i == NORGANISMS) {
		return 0;				/* Return false				*/
	}

	/* Initialize the newborn */
	organisms[i].alive = 1;
	organisms[i].pc = organisms[i].initpc = newpc & SOUPLEN - 1;
	organisms[i].r1 = 0L;
	organisms[i].r2 = 0L;
	organisms[i].r3 = 0L;
	organisms[i].r4 = 0L;
	organisms[i].heartbeat = heartbeat;
	organisms[i].lifetime = lifetime;
	organisms[i].birthtick = instructions;
	organisms[i].sp = 0L;
	for (j = 0; j < STACKLEN; j++) {
		organisms[i].stack[j] = 0L;
	}

	/* Organism has no parent if started with the monitor's
	 * (N)ew command.  Set parent to -1 and its length to
	 * noparentlength, which is set in the monitor.
	 */
	if (noparentflag) {
		organisms[i].parent = -1L;
		organisms[i].length = noparentlength;
	} else {
		organisms[i].parent = organisms[curr].number;
		organisms[i].length = organisms[curr].r3 & 0xff;
	}
	organisms[i].number = births;

	/* Record the new organism in the genealogy database */
#ifdef	GBASE
	if (gbaseflag) {
		record_birth(i);
	}
#endif

	/* Add to census */
	births++;

	/* If adjust_population is on and population is at maximum
	 * and seesaw shows that last message was about low-water mark,
	 * print message.
	 */ 
	if (births - deaths == NORGANISMS && seesaw == 0 && adjflag) {
printf(
"*** Instructions = %lu.  Population has reached the maximum.\n", 
instructions);
		seesaw = 1;
	}

	return 1;			/* Indicate success						*/
}

/*
 * Given the organism #, kill the organism
 */
int
reap(org)
long org;
{
	long ind;

	if ((ind = orgindex(org)) == -1) {
		return 0;		/* Indicate failure						*/
	}
	kill_organism(ind);
	return 1;
}

/*
 * Kill the organism whose array index is given in ind.
 */
void
kill_organism(ind)
long ind;
{
#ifdef	GBASE
	if (gbaseflag) {
		record_death(ind);
	}
#endif
	organisms[ind].alive = 0;
	deaths++;
}


/*
 * Given the organism #, find the index into the
 * organisms[] structure.
 */
long
orgindex(org)
long org;
{
	int i;

	for (i = 0; i < NORGANISMS; i++) {
		if (org == organisms[i].number && organisms[i].alive != 0) {
			return i;
		}
	}
	return -1;
}

void
mutate()
{
	long addr;
	long bit;

	addr = rnd(SOUPLEN);
	bit = rnd(8);
	soup[addr] = soup[addr] ^ (1 << bit);
}
