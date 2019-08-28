
/* scs.h -- external references to everything

   Copyright (C) 1993 Joerg Heitkoetter

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/* $Id: scs.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __SCS_H__
#define __SCS_H__

/* SCS-C system and configuration interface:
   implementation dependend defs (OS, C compiler, etc.)
   application dependend defs (RSC_FILE, RSC_PATH, V_MAINTAINER, etc. */

#include "config.h"		/* link to (or copy of) system file */


/* SCS-C program interface:
   constants, prototypes and variable defs */

#include "declare.h"

#include "aoc.h"

#include "environ.h"
#include "effector.h"
#include "detector.h"

#include "advance.h"
#include "ga.h"
#include "initial.h"
#include "io.h"
#include "perform.h"
#include "reinfor.h"
#include "report.h"
#include "timekeep.h"
#include "utility.h"
#include "random.h"

#include "panic.h"


/* ./scs.c */
#ifdef __STDC__
void main (int argc, char **argv);
void usage (int code);
void version (int code);
void warranty (int code);
#else
void main ( /* int argc, char **argv */ );
void usage ( /* int code */ );
void version ( /* int code */ );
void warranty ( /* int code */ );
#endif /* !__STDC__ */

#endif /* __SCS_H__ */
