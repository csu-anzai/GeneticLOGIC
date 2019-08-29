/* Organism management routines for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 */

#include "soup.h"
#include "organism.h"
#include "protos.h"

/* Externs */
extern	ORGANISM	organisms[];

void
init_organisms()
{
	long	i;

	for (i = 0; i < NORGANISMS; i++) {
			organisms[i].alive = 0;	/* Mark all dead */
	}
}

