
/* initial.h -- initialization routines interface

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

/* $Id: initial.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __INITIAL_H__
#define __INITIAL_H__

/* ./initial.c */
#ifdef __STDC__
void interactiveheader (FILE * rep);
void initialization (int batchflag);
void deinitialization (void);
#else
void interactiveheader ( /* FILE *rep */ );
void initialization ( /* int batchflag */ );
void deinitialization ( /* void */ );
#endif /* !__STDC__ */

#endif /* __INITIAL_H__ */
