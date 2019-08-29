/* Header file for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 21Jan92 MdG	0.2b		File created.
 */

/* Structure of an organism */
typedef struct {
	short alive;		/* =TRUE if alive			*/
	long pc;		/* Program counter			*/
	long heartbeat;		/* Heartbeat down-counter		*/
	long lifetime;		/* Lifetime down-counter		*/
	long number;		/* Organism #				*/
	long parent;		/* Parent's organism #			*/
	long initpc;		/* Initial PC				*/
	long length;		/* Length of block copy			*/
	long birthtick;		/* Time of spawning			*/
	long r1;		/* Registers				*/
	long r2;
	long r3;
	long r4;
	long sp;		/* Stack pointer			*/
	long stack[STACKLEN];	/* The stack				*/
#ifdef	GBASE
	char genome[10];
#endif
} ORGANISM;

