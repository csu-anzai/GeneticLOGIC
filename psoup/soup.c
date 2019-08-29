/* Soup management routines for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 */

#include "soup.h"
#include "protos.h"

/* Externs */
extern		unsigned char	*soup;

void
init_soup()
{
	long	i;	

	if ( (soup = (unsigned char *) malloc(SOUPLEN)) == 
						(unsigned char *) 0) {
		printf("Couldn't allocate space for the soup.\n");
		exit(1);
	}

	printf("Initializing soup to random values, please be patient.\n");
	for (i = 0; i < SOUPLEN; i++) {
		soup[i] = (unsigned char) awcrnd();
	}
}
