/* Primordial soup.  A sterile environment predisposed
 * to spontaneously generating a-life.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 19Jan92 MdG	0.2		Added the sign-on banner.
 */

#include "soup.h"
#include "protos.h"

/* External objects */
extern	unsigned char	*soup;
extern	int		births;
extern	int		deaths;
extern	int		instructions;
extern	int		curr;
extern	long		mutate_counter;
extern	short		monflag;
extern	short		adjflag;
extern	long		heartbeat;
extern	long		lifetime;
extern	long		mutate_limit;
extern	long		notify;
extern	long		watch;
extern	long		noparentflag;

void
main()
{
	banner();	/* Print sign-on banner				*/
	init();		/* Init the world.				*/
	do_soup();	/* Never returns.				*/
}

void
banner()
{
	printf("Primordial Soup %d.%d%c\n", VERSION, REVISION, MdGREV);
	printf("Soup is %ld bytes.\n", (long) SOUPLEN);
	printf("Maximum population is %ld.\n", (long) NORGANISMS);
	printf("\n");
}

void
init()
{
	init_random();		/* Init rand().  Print seed.		*/
	init_soup();		/* Fill the soup.			*/
	init_organisms();	/* Init the process structures		*/

#ifdef	UNIX
	init_signals();		/* Set up SIGINT handling.		*/
#endif

#ifdef	GBASE
	init_gbase();
#endif

	births = 0L;
	deaths = 0L;
	instructions = 0L;
	curr = 0L;
	adjflag = 0;
	monflag = 0;
	heartbeat = INIT_HEARTBEAT;
	lifetime = INIT_LIFETIME;
	mutate_limit = MUTATE_TIME;
	mutate_counter = mutate_limit;
	notify = NOTIFY_AGE;
	watch = -1;
	noparentflag = 0;
}

/*
 * Initialize the random number generator
 */
void
init_random()
{
	long seed;

	seed = time((long *) 0);

	printf("Random seed is %ld.\n", seed);
	rndseed(seed);
}
