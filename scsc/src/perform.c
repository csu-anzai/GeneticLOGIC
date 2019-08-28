
/* perform.c -- performance system, classifier matching

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
static char *rcsid = "$Id: perform.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"

FILE *cfile;			/* classifier file */
char cfilename[MAXFILENAME]
= "scs.cls";			/* name of classifier file */


/*
 *    randomchar -- set position at random with specified generality
 */
int
randomchar (pgeneral)
     double pgeneral;
{
  if (flip (pgeneral))
    return (WILDCARD);
  else if (flip (D_HALF))
    return (1);
  else
    return (0);
}

/*
 *    readcondition -- read a single condition
 */
void
readcondition (cfile, c, pgeneral, nposition)
     FILE *cfile;
     condition_t c;
     double pgeneral;
     int nposition;
{
  int j;
  char ch;

  for (j = 0; j < nposition; j++)
    {
      if (fscanf (cfile, "%c", &ch) != 1)
	panic (E_FATAL, "readcondition", "failed to fscanf input (%%c)");

      if (isspace (ch))
	{
	  --j;
	  continue;
	}
      switch (ch)
	{

	case '0':
	  c[j] = 0;
	  break;

	case '1':
	  c[j] = 1;
	  break;

	case '#':
	  c[j] = WILDCARD;
	  break;

	case 'r':
	case 'R':
	  c[j] = randomchar (pgeneral);
	  break;

	default:
	  panic (E_FATAL, "readcondition", "undefined char `%c'\n", ch);
	  break;
	}
    }
}

#if 0
/*
 *    readaction -- read a single action      (NEW!)
 */
void
readaction (cfile, a, pgeneral, nposition)
     FILE *cfile;
     action_t a;
     double pgeneral;
     int nposition;
{
  int j;
  char ch;

  for (j = 0; j < nposition; j++)
    {
      if (fscanf (cfile, "%c", &ch) != 1)
	panic (E_FATAL, "readcondition", "failed to fscanf input (%%c)");

      if (isspace (ch))
	{
	  --j;
	  continue;
	}
      switch (ch)
	{

	case '0':
	  a[j] = 0;
	  break;

	case '1':
	  a[j] = 1;
	  break;

	case 'r':
	case 'R':
	  a[j] = randomchar (pgeneral);
	  break;

	default:
	  panic (E_FATAL, "readaction", "undefined char `%c'\n", ch);
	  break;
	}
    }
}

#endif

/*
 *    readclassifier -- read a single classifier
 */
void
readclassifier (cfile, class, pgeneral, nposition)
     FILE *cfile;
     class_t *class;
     double pgeneral;
     int nposition;
{
  char ch;

  /* condition */
  readcondition (cfile, class->c, pgeneral, nposition);

  /* : */
  if (fscanf (cfile, "%c", &ch) != 1 || ch != ':')
    panic (E_FATAL, "readclassifier", "failed to fscanf ':' (%%c)");

  /* action */
  if (fscanf (cfile, "%d", &(class->a)) != 1)
    panic (E_FATAL, "readclassifier", "failed to fscanf 'action' (%%d)");

  /* strength */
  if (readln (cfile, "%lf", &(class->strength)))
    panic (E_FATAL, "readclassifier", "readln failed for 'strength' (%%lf)");

  class->bid = D_ZERO;
  class->ebid = D_ZERO;
  class->matchflag = FALSE;
}

/*
 *    countspecificity -- count condition specificity
 */
int
countspecificity (c, npos)
     condition_t c;
     int npos;
{
  int tmp = 0;

  while (npos--)
    {
      if (c[npos] != WILDCARD)
	tmp++;
    }
  return (tmp);
}

/*
 *    initclassifiers -- initialize classifiers
 */
void
initclassifiers (cfile, pop)
     FILE *cfile;
     class_p *pop;
{
  int j;

  if (readln (cfile, "%d", &(pop->nposition)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `nposition'");
  if (readln (cfile, "%d", &(pop->nclassifier)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `nclassifier'");

  if (readln (cfile, "%lf", &(pop->pgeneral)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `pgeneral'");
  if (readln (cfile, "%lf", &(pop->cbid)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `cbid'");
  if (readln (cfile, "%lf", &(pop->bidsigma)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `bidsigma'");
  if (readln (cfile, "%lf", &(pop->bidtax)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `bidtax'");
  if (readln (cfile, "%lf", &(pop->lifetax)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `lifetax'");
  if (readln (cfile, "%lf", &(pop->bid1)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `bid1'");
  if (readln (cfile, "%lf", &(pop->bid2)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `bid2'");
  if (readln (cfile, "%lf", &(pop->ebid1)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `ebid1'");
  if (readln (cfile, "%lf", &(pop->ebid2)))
    panic (E_FATAL, "initclassifiers", "readln: %s", "failed to read `ebid2'");

  /* read classifiers from a data file */
  for (j = 0; j < pop->nclassifier; j++)
    {
      readclassifier (cfile, &(pop->classifier[j]), pop->pgeneral,
		      pop->nposition);
      pop->classifier[j].specificity = countspecificity (pop->classifier[j].c, pop->nposition);
    }
}

/*
 *    initrepclassifiers -- initial report on population parameters
 */
void
initrepclassifiers (rep, pop)
     FILE *rep;
     class_p *pop;
{
  fprintf (rep, "\n\n");
  fprintf (rep, "Population Parameters\n");
  fprintf (rep, "---------------------\n");

  fprintf (rep, "Number of classifiers     = %8d\n", pop->nclassifier);
  fprintf (rep, "Number of positions       = %8d\n", pop->nposition);
  fprintf (rep, "Bid coefficient           = %8.4lf\n", pop->cbid);
  fprintf (rep, "Bid spread                = %8.4lf\n", pop->bidsigma);
  fprintf (rep, "Bidding tax               = %8.4lf\n", pop->bidtax);
  fprintf (rep, "Existence tax             = %8.4lf\n", pop->lifetax);
  fprintf (rep, "Generality probability    = %8.4lf\n", pop->pgeneral);
  fprintf (rep, "Bid specificity base      = %8.4lf\n", pop->bid1);
  fprintf (rep, "Bid specificity mult.     = %8.4lf\n", pop->bid2);
  fprintf (rep, "Ebid specificity base     = %8.4lf\n", pop->ebid1);
  fprintf (rep, "Ebid specificity mult.    = %8.4lf\n", pop->ebid2);
}

/*
 *    writecondition -- convert internal condition format to external
 */
void
writecondition (rep, c, npos)
     FILE *rep;
     condition_t c;
     int npos;
{
  int j;

  for (j = 0; j < npos; j++)
    switch (c[j])
      {

      case 0:
	fputc ('0', rep);
	break;

      case 1:
	fputc ('1', rep);
	break;

      case WILDCARD:
	fputc ('#', rep);
	break;

      default:
	panic (E_FATAL, "writecondition",
	       "undefined char in condition `%c'\n", c[j]);
	break;
      }
}

/*
 *    writeaction -- convert internal action format to external
 */
void
writeaction (rep, a, npos)
     FILE *rep;
     action_t a;
     int npos;
{
  int j;

  for (j = 0; j < npos; j++)
    switch (a)
      {

      case 0:
	fputc ('0', rep);
	break;

      case 1:
	fputc ('1', rep);
	break;

      case WILDCARD:
	fputc ('#', rep);
	break;

      default:
	panic (E_FATAL, "writeaction",
	       "undefined char in action `%c'\n", a);
	break;
      }
}

/*
 *    writeclassifier -- write a single classifier
 */
void
writeclassifier (rep, class, number, nposition)
     FILE *rep;
     class_t *class;
     int number;
     int nposition;
{
  fprintf (rep, "%5d %8.2lf %8.2lf %8.2lf", number,
	   class->strength,
	   class->bid,
	   class->ebid);

  if (class->matchflag)
    fprintf (rep, "   X ");
  else
    fprintf (rep, "     ");

  writecondition (rep, class->c, nposition);
  fprintf (rep, ":[");
  writeaction (rep, class->a, MAXACTION);
  fprintf (rep, "]");
  fprintf (rep, "     %d", class->specificity);
  fprintf (rep, "\n");
}

/*
 *    reportclassifiers -- print out the complete population
 */
void
reportclassifiers (rep, pop)
     FILE *rep;
     class_p *pop;
{
  int j;

  fprintf (rep, "\n\n");
  fprintf (rep, "No.     Strength   bid      ebid   M Classifier  specif.\n");
  fprintf (rep, "--------------------------------------------------------\n");

  for (j = 0; j < pop->nclassifier; j++)
    writeclassifier (rep, &(pop->classifier[j]), j + 1,
		     pop->nposition);
  fprintf (rep, "\n");
}

/*
 *    match -- match a single condition to a single message
 */
boolean
match (c, m, nposition)
     condition_t c;
     message_t m;
     int nposition;
{
  boolean matchtemp = TRUE;

  while (matchtemp && nposition--)
    {
      matchtemp = (c[nposition] == WILDCARD ||
		   c[nposition] == m[nposition]);
    }

  return (matchtemp);
}


/*
 *    matchclassifiers -- match all classifiers against environmental
 *              message and create match list
 */
void
matchclassifiers (pop, envmesg, mlist)
     class_p *pop;
     message_t envmesg;
     class_l *mlist;
{
  int j;

  mlist->nactive = 0;
  for (j = 0; j < pop->nclassifier; j++)
    {
      pop->classifier[j].matchflag = match (pop->classifier[j].c,
					    envmesg,
					    pop->nposition);
      if (pop->classifier[j].matchflag)
	{
	  mlist->clist[mlist->nactive++] = j;
	}
      else
	{
	  pop->classifier[j].bid = D_ZERO;	/* zero out old bids */
	  pop->classifier[j].ebid = D_ZERO;
	}
    }
}
