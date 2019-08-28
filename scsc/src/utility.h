
/* utility.h -- untility functions routines interface

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

/* $Id: utility.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __UTILITY_H__
#define __UTILITY_H__

#define TIMES		(100)
/*#define KEYPRESSED    defined in: `config.h' due to hardware specificity */

/* ./utility.c */
#ifdef __STDC__
double poweri (double x, int i);
double my_max (double x, double y);
double my_min (double x, double y);
double avg (double x, double y);
int round (double v);
int readln (FILE * fp, char *fmt, void *v);
boolean halt (void);
#else
double poweri ( /* double x, int i */ );
double my_max ( /* double x, double y */ );
double my_min ( /* double x, double y */ );
double avg ( /* double x, double y */ );
int round ( /* double v */ );
int readln ( /* FILE *fp, char *fmt, void *v */ );
boolean halt ( /* void */ );
#endif /* !__STDC__ */

#endif /* __UTILITY_H__ */
