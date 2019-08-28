/* $Id: random.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : random.c                                                      */
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
#include <math.h>
#include <time.h>
#include "define.h"
#include "global.h"
#include "error.h"
#include "other.h"
#include "trace.h"
#include "random.h"


extern double drand48();
extern void srand48();


#define RND_FAC		7		/* random seed factor */


static BOOLEAN first_random = TRUE;	/* random generator init flag */
static double z[98], c, cd, cm;		/* Marsaglia vars */
static int i97, j97;


/*****************************************************************************/
/* Marsaglia's random number generator initialization                        */
/*****************************************************************************/
static void ranmar_init(void)
{ register int i, j, k, l, ii, jj, m, ij, kl;
  double s, t;

  ij = rand() % 31328;
  kl = rand() % 30081;

  if ((ij < 0) || (ij > 31328) || (kl < 0) || (kl > 30081))
  { sprintf(Msg, "RanmarInit: Internal error in random number generator");
    critical_error(ERR_RANDOM, Msg);
  }

  i = (ij / 177) % 177 + 2;
  j = ij % 177 + 2;
  k = (kl / 169) % 178 + 1;
  l = kl % 169;

  for (ii = 1; ii <= 97; ii++)
  { s = 0.0;
    t = 0.5;
    for (jj = 1; jj <= 24; jj++)
    { m = (((i * j) % 179) * k) % 179;
      i = j;
      j = k;
      k = m;
      l = (53 * l + 1) % 169;
      if ((l * m) % 64 >= 32)
      { s += t;
      }
      t *= 0.5;
    }
    z[ii] = s;
  }

  c = 362436.0 / 16777216.0;
  cd = 7654321.0 / 16777216.0;
  cm = 16777213.0 / 16777216.0;
  i97 = 97;
  j97 = 33;
}


/*****************************************************************************/
/* Marsaglia's random number generator                                       */
/*****************************************************************************/
static double ranmar(void)
{ double uni;

  uni = z[i97] - z[j97];
  if (uni < 0.0)
  { uni += 1.0;
  }
  z[i97] = uni;
  i97--;
  if (i97 == 0)
  { i97 = 97;
  }
  j97--;
  if (j97 == 0)
  { j97 = 97;
  }
  c -= cd;
  if (c < 0.0)
  { c += cm;
  }
  uni -= c;
  if (uni < 0.0)
  { uni += 1.0;
  }

  return(uni);
}


/*****************************************************************************/
/* Random number generator initialization                                    */
/*****************************************************************************/
static void random_init(void)
{
  if (first_random)
  { init_rnd();
  }
}


/*****************************************************************************/
/* Get double random number                                                  */
/*****************************************************************************/
static double rand_double(void)
{ double rnd;

  switch (RandomType)
  { case RND_MAR:
      rnd = ranmar();
      break;
    case RND_PRG:
      rnd = drand48();
      break;
    default:
      rnd = 0.0;
      break;
  }

  return(rnd);
}


/*****************************************************************************/
/* Random number generator initialization for each random type               */
/*****************************************************************************/
void init_rnd()
{
  trace("init_rnd() entered");

  switch (RandomType)
  { case RND_MAR:
      if (InitSeed == DEF_OrigSeed)
      { srand(time(NULL) + MyProcID * RND_FAC);
      }
      else
      { srand(InitSeed + MyProcID * RND_FAC);
      }
      ranmar_init();
      break;
    case RND_PRG:
      if (InitSeed == DEF_OrigSeed)
      { srand48(time(NULL) + MyProcID * RND_FAC);
      }
      else
      { srand48(InitSeed + MyProcID * RND_FAC);
      }
      break;
    default:
      break;
  }

  first_random = FALSE;

  trace("init_rnd() completed");
}


/*****************************************************************************/
/* Get equal distributed double random number in [0,1)                       */
/*****************************************************************************/
double equal_random()
{ double res;

  trace("equal_random() entered");

  random_init();
  res = rand_double();

  trace("equal_random() completed");

  return(res);
}

 
/*****************************************************************************/
/* Get equal distributed double random number in [0,d)                       */
/*****************************************************************************/
double equal_double_random(d)
  double d;		/* interval limit */
{ double res;

  trace("equal_double_random() entered");

  res = d * equal_random();

  trace("equal_double_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get equal distributed unsigned random number in [0,u)                     */
/*****************************************************************************/
unsigned equal_unsigned_random(u)
  unsigned u;		/* interval limit */
{ unsigned res;

  trace("equal_unsigned_random() entered");

  res = u * equal_random();

  trace("equal_unsigned_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get equal distributed unsigned long random number in [0,l)                */
/*****************************************************************************/
unsigned long equal_long_random(l)
  unsigned long l;	/* interval limit */
{ unsigned long res;

  trace("equal_long_random() entered");

  res = l * equal_random();

  trace("equal_long_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get equal distributed int random number vector                            */
/*****************************************************************************/
void equal_random_int_vec(vec, n, mx)
  unsigned *vec;	/* pointer to vector */
  unsigned n;		/* length of vector */
  unsigned mx;		/* interval limit */
{ register unsigned u, v;
  char *set;

  trace("equal_random_int_vec() entered");

  set = (char *) emalloc((unsigned long) mx, TRUE);
  memset(set, 0, mx);

  for (u = 0; u < n; u++)
  { do
    { v = equal_unsigned_random(mx);
    }
    while (set[v]);
    set[v] = 1;
    vec[u] = v;
  }

  free(set);

  trace("equal_random_int_vec() completed");
}


/*****************************************************************************/
/* Get normal distributed double random number in [0,1)                      */
/*****************************************************************************/
double normal_random()
{ static int flag;
  static double x;
  double fac, r1, r2, s, res;

  trace("normal_random() entered");

  random_init();

  if (flag == 0)
  { do
    { r1 = rand_double();
      r2 = rand_double();
      s = (r1 * r1) + (r2 * r2);
    }
    while (s >= 1.0);
    fac = sqrt(-2.0 * log(s) / s);
    x = r2 * fac;
    flag = 1;
    res = r1 * fac;
  }
  else
  { flag = 0;
    res = x;
  }

  trace("normal_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get normal distributed double random number in [0,d)                      */
/*****************************************************************************/
double normal_double_random(d)
  double d;		/* interval limit */
{ double res;

  trace("normal_double_random() entered");

  res = d * normal_random();

  trace("normal_double_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get normal distributed unsigned random number in [0,u)                    */
/*****************************************************************************/
unsigned normal_unsigned_random(u)
  unsigned u;		/* interval limit */
{ unsigned res;

  trace("normal_unsigned_random() entered");

  res = u * normal_random();

  trace("normal_unsigned_random() completed");

  return(res);
}


/*****************************************************************************/
/* Get normal distributed unsigned long random number in [0,l)               */
/*****************************************************************************/
unsigned long normal_long_random(l)
  unsigned long l;	/* interval limit */
{ unsigned long res;

  trace("normal_long_random() entered");

  res = l * normal_random();

  trace("normal_long_random() completed");

  return(res);
}


/*** end of file ***/
