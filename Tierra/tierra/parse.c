/* parse.c   9-9-92  parser functions for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#include "license.h"
#include "tierra.h"
#include "extern.h"


#ifdef MEM_CHK
#include <memcheck.h>
#endif

#if INST == 1

#include "parse1.c"

#endif /* INST == 1 */

#if INST == 2

#include "parse2.c"

#endif /* INST == 2 */

#if INST == 3

#include "parse3.c"

#endif /* INST == 3 */

#if INST == 4

#include "parse4.c"

#endif /* INST == 4 */
