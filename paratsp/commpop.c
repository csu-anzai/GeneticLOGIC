/* $Id: commpop.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : commpop.c                                                     */
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
#include "define.h"
#include "global.h"
#include "chrom.h"
#include "eval.h"
#include "generate.h"
#include "other.h"
#include "parallel.h"
#include "random.h"
#include "trace.h"
#include "commpop.h"


/*****************************************************************************/
/* Evaluate an individual                                                    */
/*****************************************************************************/
static void eval_myp(int myp, CHROM *c)
{ register int tmp;

  c->needsEval = TRUE;
  tmp = P;
  P = myp;
  evaluate(c);
  P = tmp;
}


/*****************************************************************************/
/* Insert an individual in all other populations                             */
/*****************************************************************************/
static void insert_allpop(int myp, int idx, CHROM *c)
{ register int i;
  CHROM *ind;

  for (i = 0; i < MyPopNum; i++)
  { if (i != myp)
    { ind = (CHROM *) &OldPop[i]->rep[OldPop[i]->fitvec[idx]];
      order_copy(ind, c);
      eval_myp(i, ind);
    }
  }
  send_allpop(c);
}


/*****************************************************************************/
/* Sort all populations                                                      */
/*****************************************************************************/
static void sort_allpop(int myp, BOOLEAN b)
{ register int i;

  for (i = 0; i < MyPopNum; i++)
  { if (b || (i != myp))
    { sort_pop(OldPop[i]);
    }
  }
}


/*****************************************************************************/
/* Token parallel model                                                      */
/*****************************************************************************/
void pm_token()
{ register int i, j, pnum, myp;
  CHROM *ind, *ind1;
  BOOLEAN ok;

  trace("pm_token() entered");

  pnum = ((Generation[P] - CommInt) / CommInt) % PopNum;
  ok = (pnum % NProcs == MyProcID) ? TRUE : FALSE;

  if (ok)
  { myp = pnum / NProcs;
    for (i = 0; i < IndNum; i++)
    { ind = (CHROM *) &OldPop[myp]->rep[OldPop[myp]->fitvec[PopSize - 1 - i]];
      insert_allpop(myp, i, ind);
    }
    sort_allpop(myp, FALSE);
  }
  else
  { for (i = 0; i < IndNum; i++)
    { ind = (CHROM *) &OldPop[0]->rep[OldPop[0]->fitvec[i]];
      recv_allpop(ind);
      for (j = 0; j < MyPopNum; j++)
      { if (j > 0)
        { ind1 = (CHROM *) &OldPop[j]->rep[OldPop[j]->fitvec[i]];
	  order_copy(ind1, ind);
	}
        else
        { ind1 = ind;
        }
        eval_myp(j, ind1);
      }
    }
    sort_allpop(0, TRUE);
  }

  trace("pm_token() completed");
}


/*****************************************************************************/
/* Get the number of pollen which go from a population to other population   */
/*****************************************************************************/
static int get_num_pollen(int src, int dest, int indnum, int pdir, int wdir,
			  int wforce)
{ register int res, x1, y1, x2, y2, dx, dy, dir;
  double len, wlen, max, angle, dd;

  if (wforce > 0)
  { if (wforce < MAX_WINDFORCE)
    { x1 = src % GridXDim;
      y1 = src / GridXDim;
      x2 = dest % GridXDim;
      y2 = dest / GridXDim;
      dx = abs(x2 - x1);
      dy = abs(y2 - y1);
      len = sqrt((double) (dx * dx + dy * dy));
      wlen = (double) GridXDim * (double) wforce / (double) MAX_WINDFORCE;
      if (len <= wlen)
      { max = (double) indnum + 1 - (len / wlen) * (double) indnum;
      }
      else
      { max = 0.0;
      }
      if (wforce < MAX_WINDFORCE / 3)
      { wdir = pdir;
      }
      if (dx == 0)
      { angle = 90.0;
      }
      else
      { angle = 180.0 * atan((double) dy / (double) dx) / 3.1415927;
      }
      if (x2 < x1)
      { if (y2 < y1)
        { angle = 180.0 - angle;
        }
        else
        { angle = 180.0 + angle;
        }
      }
      else
      { if (y2 > y1)
        { angle = 360.0 - angle;
        }
      }
      dir = (int) angle;
      dd = (double) (MAX_ANGLE - abs(dir - wdir)) / (double) MAX_ANGLE;
      if (dd >= 0.0)
      { res = (int) (max - dd * max);
        if (res > max)
        { res--;
        }
      }
      else
      { res = 0;
      }
    }
    else
    { res = equal_unsigned_random(indnum + 1);
    }
  }
  else
  { res = 0;
  }

  return(res);
}


/*****************************************************************************/
/* Insert an individual in a population                                      */
/*****************************************************************************/
static void insert_in_pop(int pnum, int *idx, CHROM *c)
{ register int myp;
  CHROM *ind;

  if (pnum % NProcs == MyProcID)
  { myp = pnum / NProcs;
    ind = (CHROM *) &OldPop[myp]->rep[OldPop[myp]->fitvec[idx[myp]]];
    idx[myp] = (idx[myp] + 1) % PopSize;
    order_copy(ind, c);
    eval_myp(myp, ind);
  }
  else
  { send_to_pop(pnum, c);
  }
}


/*****************************************************************************/
/* Pollen parallel model                                                     */
/*****************************************************************************/
void pm_pollen()
{ register int i, j, k, x, numsend;
  CHROM *ind;
  BOOLEAN endsig;
  int wind_direction, wind_force, myp, *idx;

  trace("pm_pollen() entered");

  if (MyProcID == 0)
  { wind_direction = equal_unsigned_random(360);
    wind_force = LowerWindForce +
      equal_unsigned_random((UpperWindForce - LowerWindForce) + 1);
  }

  comm_wind(&wind_direction, &wind_force);

  idx = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  memset(idx, 0, MyPopNum * sizeof(int));

  for (x = 0; x < NProcs; x++)
  { if (x == MyProcID)
    { for (i = 0; i < MyPopNum; i++)
      { for (j = 0; j < PopNum; j++)
        { if (MyProcID + i * NProcs != j)
          { numsend = get_num_pollen(MyProcID + i * NProcs, j, IndNum,
              PollenDirection, wind_direction, wind_force);
            for (k = 0; k < numsend; k++)
            { ind = (CHROM *)
                &OldPop[i]->rep[OldPop[i]->fitvec[PopSize - 1 - k]];
              insert_in_pop(j, idx, ind);
            }
          }
        }
      }
      send_to_pop(-1, NULL);
    }
    else
    { do
      { endsig = recv_all_my_pop(x, idx, &myp, &ind);
        if (ind != NULL)
        { eval_myp(myp, ind);
        }
      }
      while (! endsig);
    }
  }
  sort_allpop(0, TRUE);

  free(idx);

  trace("pm_pollen() completed");
}


/*****************************************************************************/
/* Insert an individual in neighbour population                              */
/*****************************************************************************/
static void insert_in_neighbour(int dir, int destid, int myp, int *idx,
                                CHROM *c)
{ CHROM *ind;

  if (destid == MyProcID)
  { ind = (CHROM *) &OldPop[myp]->rep[OldPop[myp]->fitvec[idx[myp]]];
    idx[myp] = (idx[myp] + 1) % PopSize;
    order_copy(ind, c);
    eval_myp(myp, ind);
  }
  else
  { sendnode_to_pop(dir, destid, c);
  }
}


/*****************************************************************************/
/* Neighbour parallel model                                                  */
/*****************************************************************************/
void pm_neighbour()
{ register int i, j, k, m, srcid, destid, sp, dp, dir;
  CHROM *ind;
  int *idx;

  trace("pm_neighbour() entered");

  idx = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  memset(idx, 0, MyPopNum * sizeof(int));

  for (i = 0; i < PopNum; i++)
  { srcid = i % NProcs;
    sp = i / NProcs;
    for (j = 0; j < NeighbourNum; j++)
    { if (MyNeighbour[i][j] != NO_NEIGHBOUR)
      { destid = MyNeighbour[i][j] % NProcs;
        dp = MyNeighbour[i][j] / NProcs;
        for (m = 0, dir = -1; (m < NeighbourNum) && (dir < 0); m++)
        { if (MyNeighbour[MyNeighbour[i][j]][m] == i)
          { dir = m;
          }
        }
        for (k = 0; k < IndNum; k++)
        { if (srcid == MyProcID)
          { ind = (CHROM *)
              &OldPop[sp]->rep[OldPop[sp]->fitvec[PopSize - 1 - k]];
            insert_in_neighbour(j, destid, dp, idx, ind);
          }
          else
          { if (destid == MyProcID)
            { ind = (CHROM *)
                &OldPop[dp]->rep[OldPop[dp]->fitvec[idx[dp]]];
              idx[dp] = (idx[dp] + 1) % PopSize;
              recvnode_from_pop(dir, srcid, ind);
              eval_myp(dp, ind);
            }
          }
        }
      }
    }
  }
  sort_allpop(0, TRUE);

  free(idx);

  trace("pm_neighbour() completed");
}


/*****************************************************************************/
/* Communication between populations                                         */
/*****************************************************************************/
void comm_pop()
{
  trace("comm_pop() entered");

  switch (ParModel)
  { case PAR_ISL:
      break;
    case PAR_TOK:
      pm_token();
      break;
    case PAR_POL:
      pm_pollen();
      break;
    case PAR_NEI:
      pm_neighbour();
      break;
    default:
      break;
  }

  trace("comm_pop() completed");
}


/*** end of file ***/
