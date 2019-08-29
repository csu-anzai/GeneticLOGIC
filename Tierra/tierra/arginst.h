/* arginst.h   9-9-92  Artificial Life simulator setup routines */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */
/*
#ifndef lint
static char sccsid[] = "@(#)arginst.h        1.5        7/21/92";
#endif
*/

#include "license.h"

#ifndef ARGINST_H
#define ARGINST_H

I32s  GeneBnker = 1; /* turn genebanker on and off */
I32s  RateMut = 2000; /* mutation rate control by raw rate, ("cosmic ray") */
I32s  RateMovMut = 2000; /* mut rate control by raw rate, (copy mutation) */
I32s  RateFlaw = 2000; /* flaw control by raw rate */
I32s  NumCells = 1; /* # of creatures and gaps used to inoculate new soup */

InstDef id[INSTNUM];

#endif /* ARGINST_H */
