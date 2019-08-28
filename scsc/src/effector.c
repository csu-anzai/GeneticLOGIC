
/* effector.c -- effector routine(s)

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

#ifndef lint
static char *rcsid = "$Id: effector.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

void
effector (pop, crec, erec)
     class_p *pop;
     crecord_t *crec;
     erecord_t *erec;
{
  /* set action in object as dictated by auction winner */
  erec->classifieroutput = pop->classifier[crec->winner].a;
}
