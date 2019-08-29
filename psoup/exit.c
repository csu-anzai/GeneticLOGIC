/* Primordial soup exit routine.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 19Jan92 MdG	0.2		File created.
 */

#include <stdio.h>
#include "soup.h"
#include "protos.h"

#ifdef	GBASE
extern	FILE *genealogyfile;
#endif

void
soup_exit()
{
#ifdef	GBASE
	fclose(genealogyfile);
#endif
	exit();
}
