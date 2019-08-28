
/* perform.h -- performance system, classifier matching routines interface

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

/* $Id: perform.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __PERFORM_H__
#define __PERFORM_H__

extern FILE *cfile;		/* classifier file */
extern char cfilename[MAXFILENAME];	/* name of classifier file */

/* ./perform.c */
#ifdef __STDC__
int randomchar (double pgeneral);
void readcondition (FILE * cfile, condition_t c, double pgeneral, int nposition);
void readaction (FILE * cfile, action_t a, double pgeneral, int nposition);
void readclassifier (FILE * cfile, class_t * class, double pgeneral, int nposition);
int countspecificity (condition_t c, int npos);
void initclassifiers (FILE * cfile, class_p * pop);
void initrepclassifiers (FILE * rep, class_p * pop);
void writecondition (FILE * rep, condition_t c, int npos);
void writeaction (FILE * rep, action_t a, int npos);
void writeclassifier (FILE * rep, class_t * class, int number, int nposition);
void reportclassifiers (FILE * rep, class_p * pop);
boolean match (condition_t c, message_t m, int nposition);
void matchclassifiers (class_p * pop, message_t envmesg, class_l * mlist);
#else
int randomchar ( /* double pgeneral */ );
void readcondition ( /* FILE *cfile, condition_t c, double pgeneral, int nposition */ );
void readaction ( /* FILE *cfile, action_t a, double pgeneral, int nposition */ );
void readclassifier ( /* FILE *cfile, class_t *class, double pgeneral, int nposition */ );
int countspecificity ( /* condition_t c, int npos */ );
void initclassifiers ( /* FILE *cfile, class_p *pop */ );
void initrepclassifiers ( /* FILE *rep, class_p *pop */ );
void writecondition ( /* FILE *rep, condition_t c, int npos */ );
void writeaction ( /* FILE *rep, action_t a, int npos */ );
void writeclassifier ( /* FILE *rep, class_t *class, int number, int nposition */ );
void reportclassifiers ( /* FILE *rep, class_p *pop */ );
boolean match ( /* condition_t c, message_t m, int nposition */ );
void matchclassifiers ( /* class_p *pop, message_t envmesg, class_l *mlist */ );
#endif /* !__STDC__ */

#endif /* __PERFORM_H__ */
