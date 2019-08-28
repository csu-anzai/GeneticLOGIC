
/* detector.h -- detector routines interface

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

/* $Id: detector.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __DETECTOR_H__
#define __DETECTOR_H__

typedef struct
  {

/* for this problem no detectro record is
   required. Normally the detector record contains
   information for mapping environmental state
   variables into environmental bit-strings */

    int dummy;			/* needed for stupid K&R C compilers */
  }

drecord_t, DRECORD;

extern drecord_t detecrec;	/* detector inputs go here */
extern FILE *dfile;		/* detector parameters */
extern char dfilename[MAXFILENAME];	/* parameter file's name */

/* ./detector.c */
#ifdef __STDC__
void detectors (erecord_t * envrec, drecord_t * drec, message_t envmesg);
void writemessage (FILE * rep, message_t envmesg, int nposition);
void reportdetectors (FILE * rep, message_t envmesg, int nposition);
void initdetectors (FILE * dfile, drecord_t * detecrec);
void initrepdetectors (FILE * rep, drecord_t * detecrec);
#else
void detectors ( /* erecord_t *envrec, drecord_t *drec, message_t envmesg */ );
void writemessage ( /* FILE *rep, message_t envmesg, int nposition */ );
void reportdetectors ( /* FILE *rep, message_t envmesg, int nposition */ );
void initdetectors ( /* FILE *dfile, drecord_t *detecrec */ );
void initrepdetectors ( /* FILE *rep, drecord_t *detecrec */ );
#endif /* !__STDC__ */

#endif /* __DETECTOR_H__ */
