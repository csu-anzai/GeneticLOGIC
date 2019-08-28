
/* declare.h -- various consts, typedefs and global vars

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

/* $Id: declare.h,v 1.1 1993/10/04 12:00:09 joke Exp $ */

#ifndef __DECLARE_H__
#define __DECLARE_H__

#define MAXACTION		(1)
#define MAXCLASS		(100)
#define MAXFILENAME		(256)
#define MAXPOSITION		(50)

#define WILDCARD		-1
#define ZERO			'0'
#define ONE			'1'

#define D_ZERO			((double )0.0)
#define D_HALF			((double )0.5)
#define D_ONE			((double )1.0)
#define D_TWO			((double )2.0)
#define D_FIFTY			((double )50.0)
#define D_JOKE			((double )0.02021965)	/* random seed */

#define TRUE			(1)
#define FALSE			(0)
#define boolean			int


#define bit_t			int	/* ( 0, 1 ) tuple */
#define trit_t			int	/* ( 0, 1, WILDCARD ) triple */

typedef bit_t
  action_t, ACTION;

typedef bit_t
  message_t[MAXPOSITION], MESSAGE[MAXPOSITION];

typedef trit_t
  condition_t[MAXPOSITION], CONDITION[MAXPOSITION];


/* single classifier */
typedef struct
  {
    condition_t c;
    action_t a;

    double strength;
    double bid;
    double ebid;

    boolean matchflag;

    int specificity;
  }

classtype, class_t;


/* array of classifiers */
typedef class_t
  classarray[MAXCLASS], class_a[MAXCLASS];


/* list of classifiers */
typedef struct
  {
    int clist[MAXCLASS];
    int nactive;
  }

classlist, class_l;


/* population of classifiers */
typedef struct
  {
    class_a classifier;

    int nclassifier;
    int nposition;

    double pgeneral;
    double cbid;
    double bidsigma;
    double bidtax;
    double lifetax;
    double bid1;
    double bid2;
    double ebid1;
    double ebid2;
    double minstrength;
    double maxstrength;
    double sumstrength;
    double avgstrength;
  }

poptype, class_p;

extern class_p population;	/* classifier population */
extern class_l matchlist;	/* matching list */
extern message_t envmessage;	/* environmental message */
extern FILE *rep;		/* log file pointer */
extern char lfilename[MAXFILENAME];	/* log file name */
#endif /* __DECLARE_H__ */
