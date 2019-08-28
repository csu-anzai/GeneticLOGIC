
/* reinfor.h -- reinforcement and criterion procedures interface

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

/* $Id: reinfor.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __REINFOR_H__
#define __REINFOR_H__

typedef struct
  {
    double reward;
    double rewardcount;
    double totalcount;
    double count50;
    double rewardcount50;
    double proportionreward;
    double proportionreward50;

    int lastwinner;
  }

rrecord_t, RRECORD;


extern rrecord_t reinforcementrec;	/* reinforcement data goes here */
extern FILE *rfile;		/* reinforcement parameters file */
extern char rfilename[MAXFILENAME];	/* parameters file name */

/* ./reinfor.c */
#ifdef __STDC__
void initreinforcement (FILE * rfile, rrecord_t * rrec);
void initrepreinforcement (FILE * rep, rrecord_t * rrec);
boolean criterion (rrecord_t * rrec, erecord_t * envrec);
void payreward (class_p * pop, rrecord_t * rrec, crecord_t * clrec);
void reportreinforcement (FILE * rep, rrecord_t * rrec);
void reinforcement (rrecord_t * rrec, class_p * pop, crecord_t * clrec, erecord_t * envrec);
#else
void initreinforcement ( /* FILE *rfile, rrecord_t *rrec */ );
void initrepreinforcement ( /* FILE *rep, rrecord_t *rrec */ );
boolean criterion ( /* rrecord_t *rrec, erecord_t *envrec */ );
void payreward ( /* class_p *pop, rrecord_t *rrec, crecord_t *clrec */ );
void reportreinforcement ( /* FILE *rep, rrecord_t *rrec */ );
void reinforcement ( /* rrecord_t *rrec, class_p *pop, crecord_t *clrec, erecord_t *envrec */ );
#endif /* !__STDC__ */

#endif /* __REINFOR_H__ */
