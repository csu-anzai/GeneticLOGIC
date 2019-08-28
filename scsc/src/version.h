
/* version.h -- version and release handling

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

/* $Id: version.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __VERSION_H__
#define __VERSION_H__

#define	V_NAME		"SCS-C"	/* the name of the game */
#define V_MAJOR		0	/* major version number */
#define V_MINOR		99	/* minor version number: release */
#define	V_MAGIC		'j'	/* joke's magic sign */
#define V_MODF		29	/* times of modification */

#ifndef LOCAL_MAINTAINER	/* SCS-C not locally maintained... */
#define V_DATE		"October 1993"
#define V_EMAIL		"<joke@ls11.informatik.uni-dortmund.de>"
#else
#ifndef __DATE__
#define V_DATE		"October 1993"
#else /* ...else ask this guy... */
#define V_DATE		__DATE__
#endif
#define V_MAINTAINER	"NN"
#define V_EMAIL		"<nn@somewhere.edu>"
#endif
#endif /* __VERSION_H__ */
