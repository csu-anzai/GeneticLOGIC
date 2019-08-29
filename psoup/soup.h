/* Header file for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 27Jan92 MdG	0.3c	Changed initial heartbeat to 50.
 * 21Jan92 MdG	0.2b	Put the organism struct in organism.h
 * 12Jan92 MdG	0.2a	Added version stuff
 */

/* Version, revision, and internal development revision.  When
 * changing this, also change the version number in Makefile and README
 */
#define	VERSION			1
#define	REVISION		0
#define	MdGREV			' '

/* Size of the soup in bytes - MUST be a power of 2! */
#define	SOUPLEN			(1048576L)

/* Time between mutations from background radiation */
#define	MUTATE_TIME		(20)

/* Maximum number of organisms */
#define	NORGANISMS		(50)

/* Initial value of the heartbeat field in an organism */
#define	INIT_HEARTBEAT	(50)

/* Initial value of the lifetime field in an organism */
#define	INIT_LIFETIME	(500)

/* Low-water and high-water marks for spawning organisms */
#define	LOW_WATER		(5)
#define	HIGH_WATER		(10)

/* Age of an organism at which to notify the human */
#define	NOTIFY_AGE		(INIT_LIFETIME + 1)

/* Wrap the PC around the end of the soup space */
#define	ADJPC	organisms[curr].pc &= SOUPLEN-1;

#ifdef	UNIX
#define	RMODE			"r"
#define	WMODE			"w"
#define	RUMODE			"r+"
#define	WUMODE			"w+"
#endif
#ifdef	MSDOS
#define	RMODE			"rb"
#define	WMODE			"wb"
#define	RUMODE			"rb+"
#define	WUMODE			"wb+"
#endif

/* This is the number of elements in the organism's stack
 * This MUST be a power of 2.
 */
#define	STACKLEN		(16)

/* Push and pop the organism's stack. The stack grows downward, and
 * uses predecrementing and postincrementing pointers.
 */
#define	PUSH(x)	organisms[curr].sp--; \
		organisms[curr].sp &= STACKLEN - 1; \
		organisms[curr].stack[organisms[curr].sp] = \
		(unsigned long) x;
#define	POP	(dummy1 = organisms[curr].stack[organisms[curr].sp], \
		 organisms[curr].sp++, \
		 organisms[curr].sp &= STACKLEN - 1, \
		 dummy2 = dummy1);
