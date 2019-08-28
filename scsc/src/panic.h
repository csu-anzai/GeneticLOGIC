
/* panic.h -- error handler for noninteractive utilities

   Copyright (C) 1992, 1993 Joerg Heitkoetter

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* $Id: panic.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

/* stripped from `error.c' by David MacKenzie */

#ifndef __PANIC_H__
#define __PANIC_H__

#define E_FATAL		(-1)
#define E_WARN		(1)

extern char *program_name;	/* pointer to argv[0] */

#ifdef __STDC__
extern int panic (int level, char *caller, char *message,...);
#else
extern int panic ();
#endif

#endif /* __PANIC_H__ */
