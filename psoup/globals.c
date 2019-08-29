/*
 * Global variables for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 */
#include "soup.h"
#include "organism.h"
#include <stdio.h>

unsigned char	*soup;		/* The array of instructions		*/
ORGANISM	organisms[NORGANISMS];	/* The process structures	*/
long		births;		/* # of organism births			*/
long		deaths;		/* # of organism deaths			*/
long		instructions;	/* # of instructions executed		*/
long		curr;		/* Current organism			*/
short		monflag;	/* =T if mon should run			*/
long		mutate_counter;	/* Cntr for delay btwn mutations	*/
short		adjflag;	/* =T if adjust_population should run	*/
long		lifetime;	/* Lifetime of organism			*/
long		heartbeat;	/* Heartbeat of organism		*/
long		mutate_limit;	/* Ticks btwn mutation bit flips	*/
long		notify;		/* Age at which to notify human		*/
long		watch;		/* Watch execution of this organism	*/
short		noparentflag;	/* TRUE if spawning from N command
				 * in monitor or adjust_population()
				 */
long		noparentlength;	/* Length of organism if no parent	*/
short		seesaw;		/* For adjust_population msgs.		*/

#ifdef	GBASE
FILE		*genealogyfile;	/* Genealogy database file		*/
short		gbaseflag;	/* TRUE if recording turned on		*/
#endif
