/* $Id: cross.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : cross.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <string.h>
#else
#include <memory.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "define.h"
#include "global.h"
#include "bit.h"
#include "chrom.h"
#include "error.h"
#include "eval.h"
#include "other.h"
#include "random.h"
#include "trace.h"
#include "cross.h"


#define JOBSIZE  c1->myrep->n
#define C1_N     c1->myrep->n
#define C2_N     c2->myrep->n
#define P1_N     p1->myrep->n
#define P2_N     p2->myrep->n
#define C1_JOB   c1->myrep->job
#define C2_JOB   c2->myrep->job
#define P1_JOB   p1->myrep->job
#define P2_JOB   p2->myrep->job
#define HELP_JOB help->myrep->job


typedef struct t_edgelist {
  TOUR jobnum;
  unsigned char numedges;
  struct t_edgelist *edge[4];
} EDGELIST, *EDGELISTPTR;	/* Edge list for ERX */


/*****************************************************************************/
/* Edge recombination crossover (ERX)                                        */
/*****************************************************************************/
void cr_edge_recombination(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register int c, coff;
  register TOUR i, j, k, l;
  CHROM *help;
  EDGELIST *list;
  BOOLEAN found;
  TOUR actjob, newjob, adr, val, minlinks, num, numposs = 0;
  TOUR poss[4], startjob[2], *cjob;

  trace("cr_edge_recombination() entered");

  coff = (c2 != NULL) ? 2 : 1;

  list = (EDGELIST *) emalloc((unsigned long) JOBSIZE * sizeof(EDGELIST),
    TRUE);

  for (c = 0; c < coff; c++)
  { cjob = (c) ? C2_JOB : C1_JOB;

    for (i = 0; i < JOBSIZE; i++)
    { list[i].jobnum = i;
      list[i].numedges = 0;
      for (j = 0; j < 4; j++)
      { list[i].edge[j] = NULL;
      }
    }

    for (i = 0; i < 2; i++)
    { help = (i ? p2 : p1);
      startjob[i] = HELP_JOB[0];
      for (j = 0; j < JOBSIZE; j++)
      { for (k = 0; k < 2; k++)
        { adr = HELP_JOB[(k ? (j == JOBSIZE - 1 ? 0 : j + 1) : j)];
          val = HELP_JOB[(k ? j : (j == JOBSIZE - 1 ? 0 : j + 1))];
          for (l = 0, found = FALSE; (l < list[adr].numedges) && (! found);
               l++)
          { if (list[adr].edge[l]->jobnum == val)
            { found = TRUE;
            }
          }
          if (! found)
          { list[adr].edge[list[adr].numedges++] = &list[val];
          }
        }
      }
    }

    actjob = startjob[c];
    for (i = 0; i < JOBSIZE - 1; i++)
    { cjob[i] = actjob;
      minlinks = 5;
      for (j = 0; j < 4; j++)
      { if (list[actjob].edge[j])
        { num = list[actjob].edge[j]->jobnum;
          if (list[num].numedges < minlinks)
          { minlinks = list[num].numedges;
            poss[0] = num;
            numposs = 1;
          }
          else
          { if (list[num].numedges == minlinks)
            { poss[numposs++] = num;
            }
          }
        }
      }
      if (minlinks < 5)
      { newjob = poss[equal_unsigned_random(numposs)];
      }
      else
      { do
        { newjob = equal_unsigned_random(JOBSIZE);
          for (k = 0, found = FALSE; k < i + 1; k++)
          { if (cjob[k] == newjob)
            { found = TRUE;
            }
          }
        }
        while (found);
      }
      for (k = 0; k < JOBSIZE; k++)
      { if (k != actjob)
        { for (l = 0, found = FALSE; (l < 4) && (! found); l++)
          { if (list[k].edge[l] && (list[k].edge[l]->jobnum == actjob))
            { list[k].edge[l] = NULL;
              list[k].numedges--;
              found = TRUE;
            }
          }
        }
      }
      actjob = newjob;
    }
    cjob[JOBSIZE - 1] = actjob;
  }

  free(list);

  trace("cr_edge_recombination() completed");
}


/*****************************************************************************/
/* Interval partially matched crossover (IPMX)                               */
/*****************************************************************************/
void cr_interval_PMX(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register TOUR i, j, n1, n2, min, len, l1 = 0, l2 = 0;
  BOOLEAN correct, twin;
  char *s1, *s2 = NULL;

  trace("cr_interval_PMX() entered");

  twin = (c2 != NULL) ? TRUE : FALSE;

  s1 = (char *) emalloc((unsigned long) JOBSIZE, TRUE);
  memset(s1, 0, JOBSIZE);
  memset(C1_JOB, 0xFF, JOBSIZE * sizeof(TOUR));
  if (twin)
  { s2 = (char *) emalloc((unsigned long) JOBSIZE, TRUE);
    memset(s2, 0, JOBSIZE);
    memset(C2_JOB, 0xFF, JOBSIZE * sizeof(TOUR));
  }

  len = JOBSIZE / CrossInt;

  for (i = 0; i < CrossInt; i++)
  { min = i * len;
    n1 = min + equal_unsigned_random(len);
    n2 = min + equal_unsigned_random(len);
    if (n1 > n2)
    { SWAP(n1, n2);
    }
    for (j = n1; j <= n2; j++)
    { C1_JOB[j] = P2_JOB[j];
      s1[C1_JOB[j]] = 1;
      if (twin)
      { C2_JOB[j] = P1_JOB[j];
        s2[C2_JOB[j]] = 1;
      }
    }
  }

  for (i = 0; i < JOBSIZE; i++)
  { if ((C1_JOB[i] & 0xFFFF) == 0xFFFF)
    { if (! s1[P1_JOB[i]])
      { C1_JOB[i] = P1_JOB[i];
        s1[C1_JOB[i]] = 1;
      }
      if (twin)
      { if (! s2[P2_JOB[i]])
        { C2_JOB[i] = P2_JOB[i];
          s2[C2_JOB[i]] = 1;
        }
      }
    }
  }

  for (i = 0; i < JOBSIZE; i++)
  { if ((C1_JOB[i] & 0xFFFF) == 0xFFFF)
    { for (j = l1, correct = FALSE; (j < JOBSIZE) && (! correct); j++)
      { if (! s1[j])
        { C1_JOB[i] = j;
          l1 = j + 1;
          correct = TRUE;
        }
      }
    }
    if (twin)
    { if ((C2_JOB[i] & 0xFFFF) == 0xFFFF)
      { for (j = l2, correct = FALSE; (j < JOBSIZE) && (! correct); j++)
        { if (! s2[j])
          { C2_JOB[i] = j;
            l2 = j + 1;
            correct = TRUE;
          }
        }
      }
    }
  }

  if (twin)
  { free(s2);
  }
  free(s1);

  trace("cr_interval_PMX() completed");
}


/*****************************************************************************/
/* Partially matched crossover (PMX)                                         */
/*****************************************************************************/
void cr_goldberg_PMX(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register TOUR i, k, n1, n2;
  BOOLEAN correct, twin;

  trace("cr_goldberg_PMX() entered");

  twin = (c2 != NULL) ? TRUE : FALSE;

  /* Intervall waehlen */
  n1 = equal_unsigned_random(JOBSIZE);
  do
  { n2 = equal_unsigned_random(JOBSIZE);
  }
  while (n2 == n1);
  if (n1 > n2)
  { SWAP(n1, n2);
  }

  /* Im Intervall tauschen, ausserhalb kopieren bzw. korrigieren */
  for (i = 0; i < JOBSIZE; i++)
  { if ((i < n1) || (i > n2))
    { C1_JOB[i] = P1_JOB[i];
      do
      { for (k = n1, correct = FALSE; (k <= n2) && (! correct); k++)
	{ if (C1_JOB[i] == P2_JOB[k])
	  { C1_JOB[i] = P1_JOB[k];
	    correct = TRUE;
	  }
        }
      }
      while (correct);
      if (twin)
      { C2_JOB[i] = P2_JOB[i];
        do
        { for (k = n1, correct = FALSE; (k <= n2) && (! correct); k++)
          { if (C2_JOB[i] == P1_JOB[k])
            { C2_JOB[i] = P2_JOB[k];
              correct = TRUE;
            }
          }
        }
        while (correct);
      }
    }
    else
    { C1_JOB[i] = P2_JOB[i];
      if (twin)
      { C2_JOB[i] = P1_JOB[i];
      }
    }
  }

  trace("cr_goldberg_PMX() completed");
}


/*****************************************************************************/
/* Order crossover (OX)                                                      */
/*****************************************************************************/
void cr_goldberg_OX(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register int c, coff;
  register TOUR i, n1, n2, width, idx;
  TOUR *t1, *t2, *cjob;
  char *set;

  trace("cr_goldberg_OX() entered");

  coff = (c2 != NULL) ? 2 : 1;

  set = (char *) emalloc((unsigned long) JOBSIZE, TRUE);

  n1 = equal_unsigned_random(JOBSIZE);
  do
  { n2 = equal_unsigned_random(JOBSIZE);
  }
  while (n2 == n1);
  if (n1 > n2)
  { SWAP(n1, n2);
  }

  width = n2 - n1 + 1;

  for (c = 0; c < coff; c++)
  { if (c)
    { cjob = C2_JOB;
      t1 = P1_JOB;
      t2 = P2_JOB;
    }
    else
    { cjob = C1_JOB;
      t1 = P2_JOB;
      t2 = P1_JOB;
    }
    memset(set, 0, JOBSIZE);
    for (idx = 0; idx < width; idx++)
    { cjob[idx] = t1[n1 + idx];
      set[cjob[idx]] = 1;
    }
    for (i = n1; (i < JOBSIZE) && (idx < JOBSIZE); i++)
    { if (! set[t2[i]])
      { cjob[idx] = t2[i];
        set[cjob[idx]] = 1;
        idx++;
      }
    }
    for (i = 0; (i < n1) && (idx < JOBSIZE); i++)
    { if (! set[t2[i]])
      { cjob[idx] = t2[i];
        set[cjob[idx]] = 1;
        idx++;
      }
    }
  }

  free(set);

  trace("cr_goldberg_OX() completed");
}


/*****************************************************************************/
/* Cycle crossover                                                           */
/*****************************************************************************/
void cr_goldberg_CX(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register TOUR i, start, act, src, dst;
  BOOLEAN twin;
  char *set;

  trace("cr_goldberg_CX() entered");

  twin = (c2 != NULL) ? TRUE : FALSE;

  set = (char *) emalloc((unsigned long) JOBSIZE, TRUE);
  memset(set, 0, JOBSIZE);
  memset(C1_JOB, 0xFF, JOBSIZE * sizeof(TOUR));

  start = equal_unsigned_random(JOBSIZE);
  act = start;
  do
  { C1_JOB[act] = P1_JOB[act];
    set[P1_JOB[act]] = 1;
    for (i = 0; (i < JOBSIZE) && (P1_JOB[i] != P2_JOB[act]); i++) ;
    act = i;
  }
  while (act != start);

  for (src = 0, dst = 0; dst < JOBSIZE;)
  { while (((C1_JOB[dst] & 0xFFFF) != 0xFFFF) && (dst < JOBSIZE))
    { dst++;
    }
    if (dst < JOBSIZE)
    { while (set[P2_JOB[src]])
      { src++;
      }
      C1_JOB[dst++] = P2_JOB[src++];
    }
  }

  if (twin)
  { for (i = 0; i < JOBSIZE; i++)
    { C2_JOB[i] = (C1_JOB[i] == P1_JOB[i]) ? P2_JOB[i] : P1_JOB[i];
    }
  }

  free(set);

  trace("cr_goldberg_CX() completed");
}


/*****************************************************************************/
/* Uniform order based crossover (UOBX)                                      */
/*****************************************************************************/
void cr_uniform_order_based(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register TOUR i, j, f;
  BOOLEAN twin;
  char *s1, *s2 = NULL;

  trace("cr_uniform_order_based() entered");

  twin = (c2 != NULL) ? TRUE : FALSE;

  s1 = (char *) emalloc((unsigned long) JOBSIZE, TRUE);
  memset(s1, 0, JOBSIZE);
  memset(C1_JOB, 0xFF, JOBSIZE * sizeof(TOUR));
  if (twin)
  { s2 = (char *) emalloc((unsigned long) JOBSIZE, TRUE);
    memset(s2, 0, JOBSIZE);
    memset(C2_JOB, 0xFF, JOBSIZE * sizeof(TOUR));
  }

  for (i = 0; i < JOBSIZE; i++)
  { if (equal_unsigned_random(2))
    { C1_JOB[i] = P1_JOB[i];
      s1[C1_JOB[i]] = 1;
    }
    else
    { if (twin)
      { C2_JOB[i] = P2_JOB[i];
        s2[C2_JOB[i]] = 1;
      }
    }
  }

  for (i = 0, f = 0; i < JOBSIZE; i++)
  { if ((C1_JOB[i] & 0xFFFF) == 0xFFFF)
    { for (j = f; (j < JOBSIZE) && (s1[P2_JOB[j]]); j++) ;
      C1_JOB[i] = P2_JOB[j];
      f = j + 1;
    }
  }

  if (twin)
  { for (i = 0, f = 0; i < JOBSIZE; i++)
    { if ((C2_JOB[i] & 0xFFFF) == 0xFFFF)
      { for (j = f; (j < JOBSIZE) && (s2[P1_JOB[j]]); j++) ;
        C2_JOB[i] = P1_JOB[j];
        f = j + 1;
      }
    }

    free(s2);
  }

  free(s1);

  trace("cr_uniform_order_based() completed");
}


/*****************************************************************************/
/* Find position of town index in a tour                                     */
/*****************************************************************************/
static TOUR find_position(CHROM *p1, TOUR t)
{ register TOUR i;

  for (i = 0; (i < P1_N) && (P1_JOB[i] != t); i++) ;

  return(i);
}


/*****************************************************************************/
/* Nearest neighbour crossover (NNX)                                         */
/*****************************************************************************/
void cr_nearest_neighbour(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register int k, coff;
  register TOUR i, t, t1 = 0, t2 = 0;
  register unsigned rnd;
  CHROM *help;
  BIT *bit;
  BOOLEAN ok1, ok2;
  double l1, l2;

  trace("cr_nearest_neighbour() entered");

  coff = (c2 != NULL) ? 2 : 1;

  bit = alloc_bit(JOBSIZE);

  for (k = 0; k < coff; k++)
  { help = (k) ? c2 : c1;

    clear_bits(bit);

    t = (TOUR) equal_unsigned_random(JOBSIZE);
    set_bit(bit, t);
    HELP_JOB[0] = t;

    ok1 = FALSE;
    ok2 = FALSE;

    for (i = 1; i < JOBSIZE; i++)
    { if (! ok1)
      { t1 = (1 + find_position(p1, t)) % JOBSIZE;
      }
      if (! ok2)
      { t2 = (1 + find_position(p2, t)) % JOBSIZE;
      }

      if (CalcLine)
      { l1 = calc_line(t, P1_JOB[t1]);
        l2 = calc_line(t, P2_JOB[t2]);
      }
      else
      { l1 = get_line(t, P1_JOB[t1]);
        l2 = get_line(t, P2_JOB[t2]);
      }

      t = (l1 < l2) ? P1_JOB[t1] : P2_JOB[t2];

      if (get_bit(bit, t))
      { rnd = 1 + equal_unsigned_random(JOBSIZE - i);
        t = set_next_bit(bit, rnd);
        ok1 = FALSE;
        ok2 = FALSE;
      }
      else
      { set_bit(bit, t);
        if (l1 < l2)
        { ok1 = TRUE;
          ok2 = FALSE;
        }
        else
        { ok1 = FALSE;
          ok2 = TRUE;
        }
      }

      HELP_JOB[i] = t;
    }
  }

  free_bit(bit);

  trace("cr_nearest_neighbour() completed");
}


/*****************************************************************************/
/* Cheapest insertion crossover (CIX)                                        */
/*****************************************************************************/
void cr_cheapest_insertion(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register int k, coff;
  register TOUR i, t, start, t1 = 0, t2 = 0;
  register unsigned rnd;
  CHROM *help;
  BIT *bit;
  BOOLEAN ok1, ok2;
  double l1, l2;

  trace("cr_cheapest_insertion() entered");

  coff = (c2 != NULL) ? 2 : 1;

  bit = alloc_bit(JOBSIZE);

  for (k = 0; k < coff; k++)
  { help = (k) ? c2 : c1;

    clear_bits(bit);

    t = (TOUR) equal_unsigned_random(JOBSIZE);
    set_bit(bit, t);
    HELP_JOB[0] = t;
    start = t;

    ok1 = FALSE;
    ok2 = FALSE;

    for (i = 1; i < JOBSIZE; i++)
    { if (! ok1)
      { t1 = (1 + find_position(p1, t)) % JOBSIZE;
      }
      if (! ok2)
      { t2 = (1 + find_position(p2, t)) % JOBSIZE;
      }

      if (CalcLine)
      { l1 = calc_line(t, P1_JOB[t1]);
        l1 += calc_line(P1_JOB[t1], start);
        l2 = calc_line(t, P2_JOB[t2]);
        l2 += calc_line(P2_JOB[t2], start);
      }
      else
      { l1 = get_line(t, P1_JOB[t1]);
        l1 += get_line(P1_JOB[t1], start);
        l2 = get_line(t, P2_JOB[t2]);
        l2 += get_line(P2_JOB[t2], start);
      }

      t = (l1 < l2) ? P1_JOB[t1] : P2_JOB[t2];

      if (get_bit(bit, t))
      { rnd = 1 + equal_unsigned_random(JOBSIZE - i);
        t = set_next_bit(bit, rnd);
        ok1 = FALSE;
        ok2 = FALSE;
      }
      else
      { set_bit(bit, t);
        if (l1 < l2)
        { ok1 = TRUE;
          ok2 = FALSE;
        }
        else
        { ok1 = FALSE;
          ok2 = TRUE;
        }
      }

      HELP_JOB[i] = t;
    }
  }

  free_bit(bit);

  trace("cr_cheapest_insertion() completed");
}


/*****************************************************************************/
/* Farthest insertion crossover (FIX)                                        */
/*****************************************************************************/
void cr_farthest_insertion(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register int k, coff;
  register TOUR i, t, start, t1 = 0, t2 = 0;
  register unsigned rnd;
  CHROM *help;
  BIT *bit;
  BOOLEAN ok1, ok2;
  double l1, l2;

  trace("cr_farthest_insertion() entered");

  coff = (c2 != NULL) ? 2 : 1;

  bit = alloc_bit(JOBSIZE);

  for (k = 0; k < coff; k++)
  { help = (k) ? c2 : c1;

    clear_bits(bit);

    t = (TOUR) equal_unsigned_random(JOBSIZE);
    set_bit(bit, t);
    HELP_JOB[0] = t;
    start = t;

    ok1 = FALSE;
    ok2 = FALSE;

    for (i = 1; i < JOBSIZE; i++)
    { if (! ok1)
      { t1 = (1 + find_position(p1, t)) % JOBSIZE;
      }
      if (! ok2)
      { t2 = (1 + find_position(p2, t)) % JOBSIZE;
      }

      if (CalcLine)
      { l1 = calc_line(t, P1_JOB[t1]);
        l1 += calc_line(P1_JOB[t1], start);
        l2 = calc_line(t, P2_JOB[t2]);
        l2 += calc_line(P2_JOB[t2], start);
      }
      else
      { l1 = get_line(t, P1_JOB[t1]);
        l1 += get_line(P1_JOB[t1], start);
        l2 = get_line(t, P2_JOB[t2]);
        l2 += get_line(P2_JOB[t2], start);
      }

      t = (l1 > l2) ? P1_JOB[t1] : P2_JOB[t2];

      if (get_bit(bit, t))
      { rnd = 1 + equal_unsigned_random(JOBSIZE - i);
        t = set_next_bit(bit, rnd);
        ok1 = FALSE;
        ok2 = FALSE;
      }
      else
      { set_bit(bit, t);
        if (l1 > l2)
        { ok1 = TRUE;
          ok2 = FALSE;
        }
        else
        { ok1 = FALSE;
          ok2 = TRUE;
        }
      }

      HELP_JOB[i] = t;
    }
  }

  free_bit(bit);

  trace("cr_farthest_insertion() completed");
}


/*****************************************************************************/
/* Random crossover                                                          */
/*****************************************************************************/
void cr_random(p1, p2, c1, c2)
  CHROM *p1;		/* pointer to first  parent */
  CHROM *p2;		/* pointer to second parent */
  CHROM *c1;		/* pointer to first  child */
  CHROM *c2;		/* pointer to second child */
{ register unsigned rnd;

  trace("cr_random() entered");

  rnd = equal_unsigned_random(9);

  switch (rnd)
  { case 0:
      cr_edge_recombination(p1, p2, c1, c2);
      break;
    case 1:
      cr_interval_PMX(p1, p2, c1, c2);
      break;
    case 2:
      cr_goldberg_PMX(p1, p2, c1, c2);
      break;
    case 3:
      cr_goldberg_OX(p1, p2, c1, c2);
      break;
    case 4:
      cr_goldberg_CX(p1, p2, c1, c2);
      break;
    case 5:
      cr_uniform_order_based(p1, p2, c1, c2);
      break;
    case 6:
      cr_nearest_neighbour(p1, p2, c1, c2);
      break;
    case 7:
      cr_cheapest_insertion(p1, p2, c1, c2);
      break;
    case 8:
      cr_farthest_insertion(p1, p2, c1, c2);
      break;
    default:
      break;
  }

  trace("cr_random() completed");
}


/*****************************************************************************/
/* Crossover                                                                 */
/*****************************************************************************/
void crossover(p1, p2, c1, c2)
  CHROM *p1, *p2, *c1, *c2;
{
  trace("crossover() entered");

  if (Crossover == CRO_NOP)
  { order_copy(c1, p1);
    if (c2 != NULL)
    { order_copy(c2, p2);
    }
  }
  else
  { switch (Crossover)
    { case CRO_EDG:
        cr_edge_recombination(p1, p2, c1, c2);
        break;
      case CRO_INT:
        cr_interval_PMX(p1, p2, c1, c2);
        break;
      case CRO_PMX:
        cr_goldberg_PMX(p1, p2, c1, c2);
        break;
      case CRO_OX:
        cr_goldberg_OX(p1, p2, c1, c2);
        break;
      case CRO_CX:
        cr_goldberg_CX(p1, p2, c1, c2);
        break;
      case CRO_UNI:
        cr_uniform_order_based(p1, p2, c1, c2);
        break;
      case CRO_NEA:
        cr_nearest_neighbour(p1, p2, c1, c2);
        break;
      case CRO_CHE:
        cr_cheapest_insertion(p1, p2, c1, c2);
        break;
      case CRO_FAR:
        cr_farthest_insertion(p1, p2, c1, c2);
        break;
      case CRO_RND:
        cr_random(p1, p2, c1, c2);
        break;
      default:
        break;
    }
    c1->needsEval = TRUE;
    c1->mutprob = (p1->mutprob + p2->mutprob) / 2.0;
    c1->mutdim = (p1->mutdim + p2->mutdim) / 2.0;
    if (c2 != NULL)
    { c2->needsEval = TRUE;
      c2->mutprob = c1->mutprob;
      c2->mutdim = c1->mutdim;
    }
  }

  trace("crossover() completed");
}


/*** end of file ***/
