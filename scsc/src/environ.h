
/* environ.h -- environment routines interface

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

/* $Id: environ.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __ENVIRON_H__
#define __ENVIRON_H__

typedef struct
  {
    int laddress;
    int ldata;
    int lsignal;
    int address;
    int output;
    int classifieroutput;

    message_t signal;
  }

erecord_t, ERECORD;		/* 6 bit multiplexer environment */

extern erecord_t environrec;	/* the environment goes here */
extern FILE *efile;		/* environment parameters file */
extern char efilename[MAXFILENAME];	/* name of parameters file */

/* ./environ.c */
#ifdef __STDC__
void generatesignal (erecord_t * erec);
int decode (message_t mess, int start, int length);
void multiplexeroutput (erecord_t * erec);
void environment (erecord_t * erec);
void initenvironment (FILE * efile, erecord_t * erec);
void initrepenvironment (FILE * rep, erecord_t * erec);
void writesignal (FILE * rep, message_t signal, int lsignal);
void reportenvironment (FILE * rep, erecord_t * erec);
#else
void generatesignal ( /* erecord_t *erec */ );
int decode ( /* message_t mess, int start, int length */ );
void multiplexeroutput ( /* erecord_t *erec */ );
void environment ( /* erecord_t *erec */ );
void initenvironment ( /* FILE *efile, erecord_t *erec */ );
void initrepenvironment ( /* FILE *rep, erecord_t *erec */ );
void writesignal ( /* FILE *rep, message_t signal, int lsignal */ );
void reportenvironment ( /* FILE *rep, erecord_t *erec */ );
#endif /* !__STDC__ */

#endif /* __ENVIRON_H__ */
