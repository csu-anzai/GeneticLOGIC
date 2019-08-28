/* $Id: parallel.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : parallel.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifdef PARIX
#include <sys/root.h>
#include <sys/topology.h>
#include <sys/link.h>
#include <sys/comm.h>
#include <sys/sys_rpc.h>
#include <sys/rrouter.h>
#include <sys/select.h>
#include <virt_top.h>
#endif

#ifdef PARIX
#include <string.h>
#else
#include <memory.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <values.h>
#include "define.h"
#include "global.h"
#include "best.h"
#include "error.h"
#include "fopen.h"
#include "other.h"
#include "logit.h"
#include "trace.h"
#include "parallel.h"


/* Request ID for topologies */
#define RING		101
#define PIPE		102
#define GRID		103
#define TORUS		104
#define HYPERCUBE	105
#define TREE		106
#define SYN_REQ		201
#define NODE_REQ	202


#ifdef PARIX
typedef struct {
  int myp;
  double mutprob;
  double mutdim;
} SENDBUF;		/* send buffer */
#endif


#ifdef PARIX
static int DimX, DimY, DimZ;			/* dimension */
static int MyX, MyY, MyZ;			/* position */
static int RingID, TopID;			/* topology ID */
static int RLinkPrev, RLinkNext;		/* ring link ID */
static int LinkNeighbour[MAX_NEIGHBOUR];	/* link ID */
static int Error;				/* error status */
static char *SBuf;				/* send buffer */
static int SBufLen;				/* size of send buffer */
#endif

static BOOLEAN TopFlag;		/* create topology */


/*****************************************************************************/
/* Exit program                                                              */
/*****************************************************************************/
void exit_parallel(code)
  int code;		/* termination code */
{
  trace("exit_parallel() entered");

#ifdef PARIX
  AbortServer(code);
#else
  exit(code);
#endif

  trace("exit_parallel() completed");
}


/*****************************************************************************/
/* Make ring                                                                 */
/*****************************************************************************/
void make_ring()
{ register int i;

  trace("make_ring() entered");

  NeighbourNum = 2;
  if (PopNum > 2)
  { for (i = 0; i < PopNum; i++)
    { MyNeighbour[i][0] = (i) ? i - 1 : PopNum - 1;
      MyNeighbour[i][1] = (i + 1) % PopNum;
    }
  }
  else
  { for (i = 0; i < PopNum; i++)
    { MyNeighbour[i][0] = (i) ? i - 1 : NO_NEIGHBOUR;
      MyNeighbour[i][1] = (i < PopNum - 1) ? i + 1 : NO_NEIGHBOUR;
    }
  }

#ifdef PARIX
  if (NProcs > 1)
  { TopID = NewTop(2);
    if (MyProcID == 0)
    { LinkNeighbour[1] = AddTop(TopID, ConnectLink((MyProcID + 1) % NProcs,
        RING, &Error));
      LinkNeighbour[0] = AddTop(TopID, ConnectLink((NProcs + MyProcID - 1) %
        NProcs, RING, &Error));
    }
    else
    { LinkNeighbour[0] = AddTop(TopID, ConnectLink((NProcs + MyProcID - 1) %
        NProcs, RING, &Error));
      LinkNeighbour[1] = AddTop(TopID, ConnectLink((MyProcID + 1) % NProcs,
        RING, &Error));
    }
    for (i = 0; i < NeighbourNum; i++)
    { if (LinkNeighbour[i] < 0)
      { sprintf(Msg, "Failed to make ring (err=%d)", Error);
        critical_error(ERR_MAKELINK, Msg);
      }
    }
    RingID = TopID;
    RLinkPrev = LinkNeighbour[0];
    RLinkNext = LinkNeighbour[1];
  }
#endif

  trace("make_ring() completed");
}


/*****************************************************************************/
/* Make pipe                                                                 */
/*****************************************************************************/
void make_pipe()
{ register int i;

  trace("make_pipe() entered");

  NeighbourNum = 2;
  for (i = 0; i < PopNum; i++)
  { MyNeighbour[i][0] = (i) ? i - 1 : NO_NEIGHBOUR;
    MyNeighbour[i][1] = (i < PopNum - 1) ? i + 1 : NO_NEIGHBOUR;
  }

#ifdef PARIX
  if (NProcs > 1)
  { TopID = NewTop(2);
    if (MyProcID > 0)
    { LinkNeighbour[0] = AddTop(TopID, ConnectLink(MyProcID - 1, PIPE,
        &Error));
    }
    else
    { LinkNeighbour[0] = AddTop(TopID, NULL);
    }
    if (MyProcID < NProcs - 1)
    { LinkNeighbour[1] = AddTop(TopID, ConnectLink(MyProcID + 1, PIPE,
        &Error));
    }
    else
    { LinkNeighbour[1] = AddTop(TopID, NULL);
    }
    for (i = 0; i < NeighbourNum; i++)
    { if (LinkNeighbour[i] < 0)
      { sprintf(Msg, "Failed to make pipe (err=%d)", Error);
        critical_error(ERR_MAKELINK, Msg);
      }
    }
  }
#endif

  trace("make_pipe() completed");
}


/*****************************************************************************/
/* Make 2d-grid                                                              */
/*****************************************************************************/
void make_grid()
{ register int i, north, east, south, west;

  trace("make_grid() entered");

  NeighbourNum = 4;
  for (i = 0; i < PopNum; i++)
  { north = i - GridXDim;
    if (north < 0)
    { north = NO_NEIGHBOUR;
    }

    east = i + 1;
    if ((east >= PopNum) || (east % GridXDim == 0))
    { east = NO_NEIGHBOUR;
    }

    south = i + GridXDim;
    if (south >= PopNum)
    { south = NO_NEIGHBOUR;
    }

    west = i - 1;
    if ((west < 0) || (i % GridXDim == 0))
    { west = NO_NEIGHBOUR;
    }

    MyNeighbour[i][0] = north;
    MyNeighbour[i][1] = east;
    MyNeighbour[i][2] = south;
    MyNeighbour[i][3] = west;
  }

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag)
    { TopID = NewTop(2);
      if (MyProcID % GridXDim == 0)
      { if (MyNeighbour[MyProcID][1] == NO_NEIGHBOUR)
        { LinkNeighbour[1] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[1] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][1], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][3] == NO_NEIGHBOUR)
        { LinkNeighbour[3] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[3] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][3], GRID, &Error));
        }
      }
      else
      { if (MyNeighbour[MyProcID][3] == NO_NEIGHBOUR)
        { LinkNeighbour[3] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[3] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][3], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][1] == NO_NEIGHBOUR)
        { LinkNeighbour[1] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[1] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][1], GRID, &Error));
        }
      }
      if (MyProcID < GridXDim)
      { if (MyNeighbour[MyProcID][2] == NO_NEIGHBOUR)
        { LinkNeighbour[2] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[2] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][2], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][0] == NO_NEIGHBOUR)
        { LinkNeighbour[0] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[0] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][0], GRID, &Error));
        }
      }
      else
      { if (MyNeighbour[MyProcID][0] == NO_NEIGHBOUR)
        { LinkNeighbour[0] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[0] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][0], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][2] == NO_NEIGHBOUR)
        { LinkNeighbour[2] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[2] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][2], GRID, &Error));
        }
      }
      for (i = 0; i < NeighbourNum; i++)
      { if (LinkNeighbour[i] < 0)
        { sprintf(Msg, "Failed to make grid (err=%d)", Error);
          critical_error(ERR_MAKELINK, Msg);
        }
      }
    }
  }
#endif

  trace("make_grid() completed");
}


/*****************************************************************************/
/* Make 2d-torus                                                             */
/*****************************************************************************/
void make_torus()
{ register int i, d, m, north, east, south, west;

  trace("make_torus() entered");

  d = PopNum / GridXDim;
  m = PopNum % GridXDim;

  NeighbourNum = 4;
  for (i = 0; i < PopNum; i++)
  { north = i - GridXDim;
    if (north < 0)
    { if (i + GridXDim < PopNum)
      { north = (d * GridXDim) + i - GridXDim * ((i < m) ? 0 : 1);
      }
      else
      { north = NO_NEIGHBOUR;
      }
    }

    east = i + 1;
    if ((east >= PopNum) || (east % GridXDim == 0))
    { if ((GridXDim > 1) && (i % GridXDim != 0))
      { if (east % GridXDim == 0)
        { east -= GridXDim;
        }
        else
        { east = d * GridXDim;
        }
      }
      else
      { east = NO_NEIGHBOUR;
      }
    }

    south = i + GridXDim;
    if (south >= PopNum)
    { if (i - GridXDim >= 0)
      { south = i % GridXDim;
      }
      else
      { south = NO_NEIGHBOUR;
      }
    }

    west = i - 1;
    if ((west < 0) || (i % GridXDim == 0))
    { if ((GridXDim > 1) && (i + 1 < PopNum))
      { west += GridXDim; 
        if (west >= PopNum)
        { west = PopNum - 1;
        }
      }
      else
      { west = NO_NEIGHBOUR;
      }
    }

    if (north == south)
    { if (i < north)
      { north = NO_NEIGHBOUR;
      }
      else
      { south = NO_NEIGHBOUR;
      }
    }
    if (west == east)
    { if (i < west)
      { west = NO_NEIGHBOUR;
      }
      else
      { east = NO_NEIGHBOUR;
      }
    }

    MyNeighbour[i][0] = north;
    MyNeighbour[i][1] = east;
    MyNeighbour[i][2] = south;
    MyNeighbour[i][3] = west;
  }

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag)
    { TopID = NewTop(2);
      if (MyProcID % GridXDim == 0)
      { if (MyNeighbour[MyProcID][1] == NO_NEIGHBOUR)
        { LinkNeighbour[1] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[1] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][1], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][3] == NO_NEIGHBOUR)
        { LinkNeighbour[3] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[3] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][3], GRID, &Error));
        }
      }
      else
      { if (MyNeighbour[MyProcID][3] == NO_NEIGHBOUR)
        { LinkNeighbour[3] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[3] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][3], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][1] == NO_NEIGHBOUR)
        { LinkNeighbour[1] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[1] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][1], GRID, &Error));
        }
      }
      if (MyProcID < GridXDim)
      { if (MyNeighbour[MyProcID][2] == NO_NEIGHBOUR)
        { LinkNeighbour[2] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[2] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][2], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][0] == NO_NEIGHBOUR)
        { LinkNeighbour[0] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[0] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][0], GRID, &Error));
        }
      }
      else
      { if (MyNeighbour[MyProcID][0] == NO_NEIGHBOUR)
        { LinkNeighbour[0] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[0] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][0], GRID, &Error));
        }
        if (MyNeighbour[MyProcID][2] == NO_NEIGHBOUR)
        { LinkNeighbour[2] = AddTop(TopID, NULL); 
        }
        else
        { LinkNeighbour[2] = AddTop(TopID,
            ConnectLink(MyNeighbour[MyProcID][2], GRID, &Error));
        }
      }
      for (i = 0; i < NeighbourNum; i++)
      { if (LinkNeighbour[i] < 0)
        { sprintf(Msg, "Failed to make torus (err=%d)", Error);
          critical_error(ERR_MAKELINK, Msg);
        }
      }
    }
  }
#endif

  trace("make_torus() completed");
}


/*****************************************************************************/
/* Make hypercube                                                            */
/*****************************************************************************/
void make_hypercube()
{ register int i, j, mask;

  trace("make_hypercube() entered");

  NeighbourNum = (int) (log(PopNum - 1) / log(2)) + 1;
  for (i = 0; i < PopNum; i++)
  { for (j = 0, mask = 1; j < NeighbourNum; mask <<= 1, j++)
    { if ((mask < PopNum) && ((i ^ mask) < PopNum))
      { MyNeighbour[i][j] = (i ^ mask);
      }
      else
      { MyNeighbour[i][j] = NO_NEIGHBOUR;
      }
    }
  }

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag)
    { TopID = NewTop(0);
      for (i = 0, mask = 1; i < NeighbourNum; mask <<= 1, i++)
      { if ((mask < NProcs) && ((MyProcID ^ mask) < NProcs))
        { LinkNeighbour[i] = AddTop(TopID,
            ConnectLink((MyProcID ^ mask), HYPERCUBE, &Error));
        }
        else
        { LinkNeighbour[i] = AddTop(TopID, NULL);
        }
      }
      for (i = 0; i < NeighbourNum; i++)
      { if (LinkNeighbour[i] < 0)
        { sprintf(Msg, "Failed to make hypercube (err=%d)", Error);
          critical_error(ERR_MAKELINK, Msg);
        }
      }
    }
  }
#endif

  trace("make_hypercube() completed");
}


/*****************************************************************************/
/* Make tree                                                                 */
/*****************************************************************************/
void make_tree()
{ register int i, j, k, mask;
  BOOLEAN parent;

  trace("make_tree() entered");

  NeighbourNum = (int) (log(PopNum - 1) / log(2)) + 2;
  for (i = 0; i < PopNum; i++)
  { for (mask = 1, parent = FALSE; (mask < PopNum) && (! parent); mask <<= 1)
    { if (i & mask)
      { MyNeighbour[i][0] = (i ^ mask);
        parent = TRUE;
      }
    }
    if (! parent)
    { MyNeighbour[i][0] = NO_NEIGHBOUR;
    }
    for (mask = 1, j = 1; (! (i & mask)) && (mask < PopNum); mask <<= 1)
    { if ((i ^ mask) < PopNum)
      { MyNeighbour[i][j++] = (i ^ mask);
      }
    }
    for (k = j; k < NeighbourNum; k++)
    { MyNeighbour[i][k] = NO_NEIGHBOUR;
    }
  }

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag)
    { TopID = NewTop(0);
      for (mask = 1, parent = FALSE; (mask < NProcs) && (! parent); mask <<= 1)
      { if (MyProcID & mask)
        { LinkNeighbour[0] = AddTop(TopID,
            ConnectLink((MyProcID ^ mask), TREE, &Error));
          parent = TRUE;
        }
      }
      if (! parent)
      { LinkNeighbour[0] = AddTop(TopID, NULL);
      }
      for (mask = 1, i = 1; (! (MyProcID & mask)) && (mask < NProcs);
           mask <<= 1)
      { if ((MyProcID ^ mask) < NProcs)
        { LinkNeighbour[i++] = AddTop(TopID,
            ConnectLink((MyProcID ^ mask), TREE, &Error));
        }
      }
      for (j = i; j < NeighbourNum; j++)
      { LinkNeighbour[j] = AddTop(TopID, NULL);
      }
      for (i = 0; i < NeighbourNum; i++)
      { if (LinkNeighbour[i] < 0)
        { sprintf(Msg, "Failed to make tree (err=%d)", Error);
          critical_error(ERR_MAKELINK, Msg);
        }
      }
    }
  }
#endif

  trace("make_tree() completed");
}


/*****************************************************************************/
/* Initialize topology                                                       */
/*****************************************************************************/
void init_topology()
{ register int i;

  trace("init_topology() entered");

  MyNeighbour = (int **) emalloc((unsigned long) PopNum * sizeof(int *), TRUE);
  for (i = 0; i < PopNum; i++)
  { MyNeighbour[i] = (int *) emalloc((unsigned long) MAX_NEIGHBOUR *
      sizeof(int), TRUE);
  }

  GridXDim = 1 + (int) sqrt(PopNum - 1);

  make_ring();

  switch (Topology)
  { case TOP_RIN:
      break;
    case TOP_PIP:
      make_pipe();
      break;
    case TOP_GRI:
      make_grid();
      break;
    case TOP_TOR:
      make_torus();
      break;
    case TOP_HYP:
      make_hypercube();
      break;
    case TOP_TRE:
      make_tree();
      break;
    default:
      break;
  }

  trace("init_topology() completed");
}


/*****************************************************************************/
/* Free topology                                                             */
/*****************************************************************************/
void free_topology()
{ register int i;

  trace("free_topology() entered");

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag && (Topology != TOP_RIN))
    { FreeTop(TopID);
    }
    FreeTop(RingID);
  }
#endif

  for (i = 0; i < PopNum; i++)
  { free(MyNeighbour[i]);
  }
  free(MyNeighbour);

  trace("free_topology() completed");
}


/*****************************************************************************/
/* Initialize parallel variables                                             */
/*****************************************************************************/
void init_parvars()
{
  trace("init_parvars() entered");

#ifdef PARIX
  NProcs = PC_nProcs;
  NProcsOrig = NProcs;
  MyProcID = PC_MyProcID;
  DimX = PC_DimX;
  DimY = PC_DimY;
  DimZ = PC_DimZ;
  MyX = PC_MyX;
  MyY = PC_MyY;
  MyZ = PC_MyZ;
#else
  NProcs = 1;
  NProcsOrig = 1;
  MyProcID = 0;
#endif

  trace("init_parvars() completed");
}


/*****************************************************************************/
/* Start of parallelity                                                      */
/*****************************************************************************/
void init_parallel()
{
  trace("init_parallel() entered");

  if (PopNum > 1)
  { switch (ParModel)
    { case PAR_ISL:
        Topology = TOP_RIN;
        TopFlag = FALSE;
        break;
      case PAR_TOK:
        Topology = TOP_RIN;
        TopFlag = FALSE;
        break;
      case PAR_POL:
        Topology = TOP_PIP;
        TopFlag = TRUE;
        break;
      case PAR_NEI:
        TopFlag = (NProcs == PopNum) ? TRUE : FALSE;
        break;
      default:
        break;
    }

    init_topology();

#ifdef PARIX
    SBufLen = sizeof(SENDBUF) + OrderLen * sizeof(TOUR);
    SBuf = (char *) emalloc((unsigned long) SBufLen * sizeof(char), TRUE);
#endif
  }

  trace("init_parallel() completed");
}


/*****************************************************************************/
/* End of parallelity                                                        */
/*****************************************************************************/
void end_parallel()
{
  trace("end_parallel() entered");

  if (PopNum > 1)
  {
#ifdef PARIX
    free(SBuf);
#endif
    free_topology();
  }

  trace("end_parallel() completed");
}


/*****************************************************************************/
/* Initialize population variables                                           */
/*****************************************************************************/
void init_population()
{
  trace("init_population() entered");

  if ((ProcNum >= 1) && (ProcNum <= NProcs))
  { NProcs = ProcNum;
  }

  if ((PopNum >= 1) && (PopNum < NProcs))
  { NProcs = PopNum;
  }

  if (MyProcID >= NProcs)
  { exit(ERR_NO);
  }

  MyPopNum = PopNum / NProcs;
  if (MyProcID < (PopNum % NProcs))
  { MyPopNum++;
  }

  if (TraceFlag)
  { log_num_int(stdout, "ProcNum", ProcNum, TRUE); 
    log_num_int(stdout, "NProcs", NProcs, TRUE); 
    log_num_int(stdout, "PopNum", PopNum, TRUE); 
    log_num_int(stdout, "MyPopNum", MyPopNum, TRUE); 
  }

  init_parallel();

  trace("init_population() completed");
}


/*****************************************************************************/
/* Send function                                                             */
/*****************************************************************************/
int my_send(top, dir, data, size)
  int top;		/* topology ID */
  int dir;		/* direction */
  void *data;		/* pointer of buffer */
  int size;		/* size of buffer */
{ register int err = 0;

  trace("my_send() entered");

#ifdef PARIX

#ifdef DEBUG
  printf("DEBUG: Send from Proc #%d in Direction %d --> %d Bytes\n",
    MyProcID, dir, size);
  printf("DEBUG: Proc #%d sends Data: %d %d ...\n", MyProcID,
    ((int *) data)[0], ((int *) data)[1]);
#endif

  err = Send(top, dir, data, size);

#ifdef DEBUG
  printf("DEBUG: Proc #%d has sended in direction %d --> err = %d\n",
    MyProcID, dir, err);
#endif

#endif

  trace("my_send() completed");

  return(err);
}


/*****************************************************************************/
/* Receive function                                                          */
/*****************************************************************************/
int my_recv(top, dir, data, size)
  int top;		/* topology ID */
  int dir;		/* direction */
  void *data;		/* pointer of buffer */
  int size;		/* size of buffer */
{ register int err = 0;

  trace("my_recv() entered");

#ifdef PARIX

#ifdef DEBUG
  printf("DEBUG: Receive to Proc #%d from Direction %d --> %d Bytes\n",
    MyProcID, dir, size);
#endif

  err = Recv(top, dir, data, size);

#ifdef DEBUG
  printf("DEBUG: Proc #%d has received from direction %d --> err = %d\n",
    MyProcID, dir, err);
  printf("DEBUG: Proc #%d has received Data: %d %d ...\n", MyProcID,
    ((int *) data)[0], ((int *) data)[1]);
#endif

#endif

  trace("my_recv() completed");

  return(err);
}


/*****************************************************************************/
/* SendNode function                                                         */
/*****************************************************************************/
int my_sendnode(procid, reqid, data, size)
  int procid;		/* processor ID */
  int reqid;		/* request ID */
  void *data;		/* pointer of buffer */
  int size;		/* size of buffer */
{ register int err = 0;

  trace("my_sendnode() entered");

#ifdef PARIX

#ifdef DEBUG
  printf("DEBUG: SendNode from Proc #%d to Proc #%d --> %d Bytes\n",
    MyProcID, procid, size);
  printf("DEBUG: Proc #%d sends Data: %d %d ...\n", MyProcID,
    ((int *) data)[0], ((int *) data)[1]);
#endif

  err = SendNode(procid, reqid, data, size);

#ifdef DEBUG
  printf("DEBUG: Proc #%d has sended to proc #%d --> err = %d\n",
    MyProcID, procid, err);
#endif

#endif

  trace("my_sendnode() completed");

  return(err);
}


/*****************************************************************************/
/* RecvNode function                                                         */
/*****************************************************************************/
int my_recvnode(procid, reqid, data, size)
  int procid;		/* processor ID */
  int reqid;		/* request ID */
  void *data;		/* pointer of buffer */
  int size;		/* size of buffer */
{ register int err = 0;

  trace("my_recvnode() entered");

#ifdef PARIX

#ifdef DEBUG
  printf("DEBUG: RecvNode to Proc #%d from Proc #%d --> %d Bytes\n",
    MyProcID, procid, size);
#endif

  err = RecvNode(procid, reqid, data, size);

#ifdef DEBUG
  printf("DEBUG: Proc #%d has received from proc #%d --> err = %d\n",
    MyProcID, procid, err);
  printf("DEBUG: Proc #%d has received Data: %d %d ...\n", MyProcID,
    ((int *) data)[0], ((int *) data)[1]);
#endif

#endif

  trace("my_recvnode() completed");

  return(err);
}


#ifdef PARIX
/*****************************************************************************/
/* Set all values for send buffer                                            */
/*****************************************************************************/
static void set_sendbuf(int myp, CHROM *c)
{ SENDBUF *sendbuf = (SENDBUF *) SBuf;

  if (c != NULL)
  { sendbuf->myp = myp;
    sendbuf->mutprob = c->mutprob;
    sendbuf->mutdim = c->mutdim;
    memcpy(&sendbuf[1], c->myrep->job, OrderLen * sizeof(TOUR));
  }
}


/*****************************************************************************/
/* Get all values from send buffer                                           */
/*****************************************************************************/
static void get_sendbuf(int *myp, CHROM *c)
{ SENDBUF *sendbuf = (SENDBUF *) SBuf;
 
  if (myp != NULL)
  { *myp = sendbuf->myp;
  }

  if (c != NULL)
  { c->mutprob = sendbuf->mutprob; 
    c->mutdim = sendbuf->mutdim;
    memcpy(c->myrep->job, &sendbuf[1], OrderLen * sizeof(TOUR));
  }
}
#endif


/*****************************************************************************/
/* Send to all populations                                                   */
/*****************************************************************************/
void send_allpop(c)
  CHROM *c;		/* pointer to individual */
{
  trace("send_allpop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { set_sendbuf(-1, c);
    my_send(RingID, RLinkNext, SBuf, SBufLen);
    my_recv(RingID, RLinkPrev, SBuf, SBufLen);
  }
#endif

  trace("send_allpop() completed");
}


/*****************************************************************************/
/* Receive from all populations                                              */
/*****************************************************************************/
void recv_allpop(c)
  CHROM *c;		/* pointer to individual */
{
  trace("recv_allpop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { my_recv(RingID, RLinkPrev, SBuf, SBufLen);
    my_send(RingID, RLinkNext, SBuf, SBufLen);
    get_sendbuf(NULL, c);
  }
#endif

  trace("recv_allpop() completed");
}


/*****************************************************************************/
/* Communication for wind values                                             */
/*****************************************************************************/
void comm_wind(wdir, wforce)
  int *wdir;		/* pointer to wind direction */
  int *wforce;		/* pointer to wind force */
{
#ifdef PARIX
  int buf[2];
#endif

  trace("comm_wind() entered");

#ifdef PARIX
  if (NProcs > 1)
  { if (MyProcID == 0)
    { buf[0] = *wdir;
      buf[1] = *wforce;
      my_send(RingID, RLinkNext, buf, sizeof(buf));
      my_recv(RingID, RLinkPrev, buf, sizeof(buf));
    }
    else
    { my_recv(RingID, RLinkPrev, buf, sizeof(buf));
      my_send(RingID, RLinkNext, buf, sizeof(buf));
      *wdir = buf[0];
      *wforce = buf[1];
    }
  }
#endif

  trace("comm_wind() completed");
}


/*****************************************************************************/
/* Send to a population                                                      */
/*****************************************************************************/
void send_to_pop(pnum, c)
  int pnum;		/* population number */
  CHROM *c;		/* pointer to individual */
{
#ifdef PARIX
  register int myp;
  int procid;
#endif

  trace("send_to_pop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { procid = pnum % NProcs;

    if (MyProcID > 0)
    { my_send(TopID, LinkNeighbour[0], &procid, sizeof(procid));
    }
    if (MyProcID < NProcs - 1)
    { my_send(TopID, LinkNeighbour[1], &procid, sizeof(procid));
    }

    if ((procid >= 0) && (procid < NProcs))
    { myp = pnum / NProcs;
      set_sendbuf(myp, c);
      my_sendnode(procid, NODE_REQ, SBuf, SBufLen);
    }
  }
#endif

  trace("send_to_pop() completed");
}


/*****************************************************************************/
/* Receive from all populations if it for me                                 */
/*****************************************************************************/
BOOLEAN recv_all_my_pop(srcid, idx, myp, c)
  int srcid;		/* source processor ID */
  int *idx;		/* index of individual */
  int *myp;		/* population number */
  CHROM **c;		/* pointer to individual */
{ BOOLEAN res = TRUE;
#ifdef PARIX
  CHROM *ind;
  int destid;
#endif

  trace("recv_all_my_pop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { if (MyProcID < srcid)
    { my_recv(TopID, LinkNeighbour[1], &destid, sizeof(destid));
      if (MyProcID > 0)
      { my_send(TopID, LinkNeighbour[0], &destid, sizeof(destid));
      }
    }
    else
    { my_recv(TopID, LinkNeighbour[0], &destid, sizeof(destid));
      if (MyProcID < NProcs - 1)
      { my_send(TopID, LinkNeighbour[1], &destid, sizeof(destid));
      }
    }

    if (destid == MyProcID)
    { my_recvnode(srcid, NODE_REQ, SBuf, SBufLen);
      get_sendbuf(myp, NULL);
      ind = (CHROM *) &OldPop[*myp]->rep[OldPop[*myp]->fitvec[idx[*myp]]];
      idx[*myp] = (idx[*myp] + 1) % PopSize;
      get_sendbuf(NULL, ind);
    }
    else
    { ind = NULL;
    }
    *c = ind;
    res = (destid == -1) ? TRUE : FALSE;
  }
#endif

  trace("recv_all_my_pop() completed");

  return(res);
}


/*****************************************************************************/
/* SendNode to a population                                                  */
/*****************************************************************************/
void sendnode_to_pop(dir, procid, c)
  int dir;		/* direction */
  int procid;		/* destination processor */
  CHROM *c;		/* pointer to individual */
{
  trace("sendnode_to_pop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { set_sendbuf(-1, c);
    if (TopFlag)
    { my_send(TopID, LinkNeighbour[dir], SBuf, SBufLen);
    }
    else
    { my_sendnode(procid, NODE_REQ, SBuf, SBufLen);
    }
  }
#endif

  trace("sendnode_to_pop() completed");
}


/*****************************************************************************/
/* RecvNode from a population                                                */
/*****************************************************************************/
void recvnode_from_pop(dir, procid, c)
  int dir;		/* direction */
  int procid;		/* source processor */
  CHROM *c;		/* pointer to individual */
{
  trace("recvnode_from_pop() entered");

#ifdef PARIX
  if (NProcs > 1)
  { if (TopFlag)
    { my_recv(TopID, LinkNeighbour[dir], SBuf, SBufLen);
    }
    else
    { my_recvnode(procid, NODE_REQ, SBuf, SBufLen);
    }
    get_sendbuf(NULL, c);
  }
#endif

  trace("recvnode_from_pop() completed");
}


/*****************************************************************************/
/* Synchronize processors                                                    */
/*****************************************************************************/
void synchronize()
{
#ifdef PARIX
  char c = ' ';
#ifdef DEBUG
  unsigned int t1, t2;
#endif
#endif

  trace("synchronize() entered");

#ifdef PARIX
  if (NProcs > 1)
  {
#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Synchronization started\n");
      t1 = TimeNowHigh();
    }
#endif
    if (MyProcID == 0)
    { my_send(RingID, RLinkNext, &c, sizeof(c));
      my_recv(RingID, RLinkPrev, &c, sizeof(c));
      my_send(RingID, RLinkNext, &c, sizeof(c));
      my_recv(RingID, RLinkPrev, &c, sizeof(c));
    }
    else
    { my_recv(RingID, RLinkPrev, &c, sizeof(c));
      my_send(RingID, RLinkNext, &c, sizeof(c));
      my_recv(RingID, RLinkPrev, &c, sizeof(c));
      my_send(RingID, RLinkNext, &c, sizeof(c));
    }
#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Synchronization completed\n");
      t2 = TimeNowHigh();
      printf("DEBUG: Elapsed time = %d ticks\n", t2 - t1);
    }
#endif
  }
#endif

  trace("synchronize() completed");
}


/*****************************************************************************/
/* Synchronize generations on processors                                     */
/*****************************************************************************/
BOOLEAN sync_done(flag)
  BOOLEAN flag;		/* done flag */
{ BOOLEAN res = flag;
#ifdef PARIX
#ifdef DEBUG
  unsigned int t1, t2;
#endif
#endif

  trace("sync_done() entered");

#ifdef PARIX
  if (NProcs > 1)
  {
#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Generation-Synchronization started\n");
      t1 = TimeNowHigh();
    }
#endif
    if (MyProcID == 0)
    { my_send(RingID, RLinkNext, &flag, sizeof(BOOLEAN));
      my_recv(RingID, RLinkPrev, &res, sizeof(BOOLEAN));
      my_send(RingID, RLinkNext, &res, sizeof(BOOLEAN));
      my_recv(RingID, RLinkPrev, &res, sizeof(BOOLEAN));
    }
    else
    { my_recv(RingID, RLinkPrev, &res, sizeof(BOOLEAN));
      res |= flag;
      my_send(RingID, RLinkNext, &res, sizeof(BOOLEAN));
      my_recv(RingID, RLinkPrev, &res, sizeof(BOOLEAN));
      my_send(RingID, RLinkNext, &res, sizeof(BOOLEAN));
    }
#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Generation-Synchronization completed\n");
      t2 = TimeNowHigh();
      printf("DEBUG: Elapsed time = %d ticks\n", t2 - t1);
    }
#endif
  }
#endif

  trace("sync_done() completed");

  return(res);
}


/*****************************************************************************/
/* Get global optimum of all populations                                     */
/*****************************************************************************/
double get_global_optimum()
{ register int i, bestpop, num;
  FILE *fp;
  double bestqual;
#ifdef PARIX
  TOUR *recvtour;
  double recvqual;
#ifdef DEBUG
  unsigned int t1, t2;
#endif
#endif

  trace("get_global_optimum() entered");

  bestqual = MAXDOUBLE;
  bestpop = 0;

  for (i = 0; i < MyPopNum; i++)
  { if (SaveSize)
    { if (BestSet[i][0].quality < bestqual)
      { bestqual = BestSet[i][0].quality;
        bestpop = i;
      }
    }
    else
    { if (Best[i] < bestqual)
      { bestqual = Best[i];
      }
    }
  }

#ifdef PARIX
  if (NProcs > 1)
  { recvtour = (TOUR *) emalloc((unsigned long) OrderLen * sizeof(TOUR),
      TRUE);

#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Global Optimum Communication started\n");
      t1 = TimeNowHigh();
    }
#endif

    if (MyProcID == 0)
    { my_send(RingID, RLinkNext, &bestqual, sizeof(bestqual));
      my_recv(RingID, RLinkPrev, &bestqual, sizeof(bestqual));
      if (SaveSize)
      { my_send(RingID, RLinkNext, BestSet[bestpop][0].myrep->job,
          OrderLen * sizeof(TOUR));
        my_recv(RingID, RLinkPrev, BestSet[0][0].myrep->job, OrderLen *
          sizeof(TOUR));
        bestpop = 0;
        BestSet[bestpop][0].quality = bestqual;
      }
    }
    else
    { my_recv(RingID, RLinkPrev, &recvqual, sizeof(recvqual));
      if (recvqual < bestqual)
      { bestqual = recvqual;
        my_send(RingID, RLinkNext, &bestqual, sizeof(bestqual));
        if (SaveSize)
        { my_recv(RingID, RLinkPrev, BestSet[0][0].myrep->job, OrderLen *
            sizeof(TOUR));
          bestpop = 0;
        }
      }
      else
      { my_send(RingID, RLinkNext, &bestqual, sizeof(bestqual));
        if (SaveSize)
        { my_recv(RingID, RLinkPrev, recvtour, OrderLen * sizeof(TOUR));
        }
      }
      if (SaveSize)
      { my_send(RingID, RLinkNext, BestSet[bestpop][0].myrep->job,
          OrderLen * sizeof(TOUR));
      }
    }

#ifdef DEBUG
    if (MyProcID == 0)
    { printf("DEBUG: Global Optimum Communication completed\n");
      t2 = TimeNowHigh();
      printf("DEBUG: Elapsed time = %d ticks\n", t2 - t1);
    }
#endif

    free(recvtour);
  }
#endif

  if (PopNum > 1)
  { if (OutFlag && (MyProcID == 0))
    { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
      { sprintf(Msg, "Global Optimum for Experiment %d", Experiment);
        log_double(fp, Msg, bestqual);
        fprintf(fp, "\n");
        fclose(fp);
      }
      else
      { sprintf(Msg, "Main: can't open '%s'", AllLogFile);
        critical_error(ERR_FILE_OPEN, Msg);
      }

      if (SaveSize)
      { num = PopNum;
        PopNum = 1;
        P = bestpop;
        print_best(BestFile, "w");
        PopNum = num;
      }
    }
  }

  trace("get_global_optimum() completed");

  return(bestqual);
}


/* ------------------------------------------------------------------------- */
/* Parallel functions for SATSP                                              */
/* ------------------------------------------------------------------------- */

/*****************************************************************************/
/* Send configuration to neighbour                                           */
/*****************************************************************************/
void sa_send_to_neighbour(dir, tour)
  int dir;		/* direction */
  TOUR *tour;		/* pointer to tour */
{
  trace("sa_send_to_neighbour() entered");

#ifdef PARIX
  if (NProcs > 1)
  { my_send(TopID, LinkNeighbour[dir], tour, OrderLen * sizeof(TOUR));
  }
#endif

  trace("sa_send_to_neighbour() completed");
}


/*****************************************************************************/
/* Recv configuration from neighbour                                         */
/*****************************************************************************/
void sa_recv_from_neighbour(dir, tour)
  int dir;		/* direction */
  TOUR *tour;		/* pointer to tour */
{
  trace("sa_recv_from_neighbour() entered");

#ifdef PARIX
  if (NProcs > 1)
  { my_recv(TopID, LinkNeighbour[dir], tour, OrderLen * sizeof(TOUR));
  }
#endif

  trace("sa_recv_from_neighbour() completed");
}


/*****************************************************************************/
/* Synchronize time on processors                                            */
/*****************************************************************************/
BOOLEAN sa_sync_done(flag, allflag)
  BOOLEAN flag;		/* done flag */
  BOOLEAN allflag;	/* doneall flag */
{ int res;
#ifdef PARIX
  int own;
#endif

  trace("sa_sync_done() entered");

  res = (flag) ? 1 : 0;

#ifdef PARIX
  if (NProcs > 1)
  { own = res;
    if (allflag)
    { own |= 2;
    }
    if (MyProcID == 0)
    { my_send(RingID, RLinkNext, &own, sizeof(int));
      my_recv(RingID, RLinkPrev, &res, sizeof(int));
      my_send(RingID, RLinkNext, &res, sizeof(int));
      my_recv(RingID, RLinkPrev, &res, sizeof(int));
    }
    else
    { my_recv(RingID, RLinkPrev, &res, sizeof(int));
      res = (res & 2) | (res & own);
      my_send(RingID, RLinkNext, &res, sizeof(int));
      my_recv(RingID, RLinkPrev, &res, sizeof(int));
      my_send(RingID, RLinkNext, &res, sizeof(int));
    }
  }
#endif

  trace("sa_sync_done() completed");

  return((res) ? TRUE : FALSE);
}


/*****************************************************************************/
/* Get global optimum of all processors                                      */
/*****************************************************************************/
double sa_get_global_optimum(localopt)
  double localopt;	/* local optimum */
{ double globopt;
#ifdef PARIX
  double recvopt;
#endif

  trace("sa_get_global_optimum() entered");

  globopt = localopt;

#ifdef PARIX
  if (NProcs > 1)
  { if (MyProcID == 0)
    { my_send(RingID, RLinkNext, &globopt, sizeof(globopt));
      my_recv(RingID, RLinkPrev, &globopt, sizeof(globopt));
    }
    else
    { my_recv(RingID, RLinkPrev, &recvopt, sizeof(recvopt));
      if (recvopt < globopt)
      { globopt = recvopt;
      }
      my_send(RingID, RLinkNext, &globopt, sizeof(globopt));
    }
  }
#endif

  trace("sa_get_global_optimum() completed");

  return(globopt);
}


/*** end of file ***/
