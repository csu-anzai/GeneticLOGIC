/* $Id: interfac.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : interfac.c                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "define.h"
#include "global.h"
#include "init.h"
#include "input.h"
#include "trace.h"
#include "interfac.h"


#define RDDATA	1		/* read input file */
#define WTDATA	0		/* write input file */

/* input file lines */

#define IOS_TspFileName "TSP file name                          : %s\n"
#define IOD_TspFileName	"TSP file name                          [%9s] : "
#define IOP_TspFileName IOS_TspFileName

#define IOS_PopSize	"Population Size                        : %d\n"
#define IOD_PopSize	"Population Size                        [%9d] : "
#define IOP_PopSize	IOS_PopSize

#define IOS_PopInit	"Population Initialization Mechanism    : %c\n"
#define IOD_PopInit	"Population Initialization Mechanism    [%9c] : "
#define IOP_PopInit	IOS_PopInit

#define IOS_TourFileName "TOUR file name                         : %s\n"
#define IOD_TourFileName "TOUR file name                         [%9s] : "
#define IOP_TourFileName IOS_TourFileName

#define IOS_GapSize	"Generation Gap                         : %lf\n"
#define IOD_GapSize	"Generation Gap                         [%9.3f] : "
#define IOP_GapSize	"Generation Gap                         : %.3f\n"

#define IOS_BreederSelect "Breeder Selection Mechanism            : %c\n"
#define IOD_BreederSelect "Breeder Selection Mechanism            [%9c] : "
#define IOP_BreederSelect IOS_BreederSelect

#define IOS_Whitley	"Value of Whitley's Constant 'a'        : %lf\n"
#define IOD_Whitley	"Value of Whitley's Constant 'a'        [%9.3f] : "
#define IOP_Whitley	"Value of Whitley's Constant 'a'        : %.3f\n"

#define IOS_EtaMax	"Maximum Expected Value for Ranking     : %lf\n"
#define IOD_EtaMax	"Maximum Expected Value for Ranking     [%9.3f] : "
#define IOP_EtaMax	"Maximum Expected Value for Ranking     : %.3f\n"

#define IOS_CtrlParam	"Temperature Control Parameter          : %lf\n"
#define IOD_CtrlParam	"Temperature Control Parameter          [%9.3f] : "
#define IOP_CtrlParam	"Temperature Control Parameter          : %.3f\n"

#define IOS_ChnLen	"Constant Temp. Cooling Interval (Gens) : %d\n"
#define IOD_ChnLen	"Constant Temp. Cooling Interval (Gens) [%9d] : "
#define IOP_ChnLen	IOS_ChnLen

#define IOS_LowerIndex	"Lower Index for Breeder Selection      : %d\n"
#define IOD_LowerIndex	"Lower Index for Breeder Selection      [%9d] : "
#define IOP_LowerIndex	IOS_LowerIndex

#define IOS_UpperIndex	"Upper Index for Breeder Selection      : %d\n"
#define IOD_UpperIndex	"Upper Index for Breeder Selection      [%9d] : "
#define IOP_UpperIndex	IOS_UpperIndex

#define IOS_WindowSize	"Scaling Window Size                    : %d\n"
#define IOD_WindowSize	"Scaling Window Size                    [%9d] : "
#define IOP_WindowSize	IOS_WindowSize

#define IOS_MateSelect	"Mates Selection Mechanism              : %c\n"
#define IOD_MateSelect	"Mates Selection Mechanism              [%9c] : "
#define IOP_MateSelect	IOS_MateSelect

#define IOS_Crossover	"Crossover Mechanism                    : %c\n"
#define IOD_Crossover	"Crossover Mechanism                    [%9c] : "
#define IOP_Crossover	IOS_Crossover

#define IOS_CrossRate	"Crossover Application Rate             : %lf\n"
#define IOD_CrossRate	"Crossover Application Rate             [%9.3f] : "
#define IOP_CrossRate	"Crossover Application Rate             : %.3f\n"

#define IOS_CrossTwoOff	"Crossover creates 2 Offsprings         : %s\n"
#define IOD_CrossTwoOff	"Crossover creates 2 Offsprings         [%9s] : "
#define IOP_CrossTwoOff	IOS_CrossTwoOff

#define IOS_CrossInt	"Number of Crossover Intervals          : %d\n"
#define IOD_CrossInt	"Number of Crossover Intervals          [%9d] : "
#define IOP_CrossInt	IOS_CrossInt

#define IOS_OptLocal	"Local Tour Optimization Mechanism      : %c\n"
#define IOD_OptLocal	"Local Tour Optimization Mechanism      [%9c] : "
#define IOP_OptLocal	IOS_OptLocal

#define IOS_Mutation	"Mutation Mechanism                     : %s\n"
#define IOD_Mutation	"Mutation Mechanism                     [%9s] : "
#define IOP_Mutation	IOS_Mutation

#define IOS_MutRate	"Mutation Probability                   : %lf\n"
#define IOD_MutRate	"Mutation Probability                   [%9.4f] : "
#define IOP_MutRate	"Mutation Probability                   : %.4f\n"

#define IOS_MutDim	"Mutation Step Dimension                : %lf\n"
#define IOD_MutDim	"Mutation Step Dimension                [%9.2f] : "
#define IOP_MutDim	"Mutation Step Dimension                : %.2f\n"

#define IOS_Replace	"Replace Mechanism (Dispersal)          : %c\n"
#define IOD_Replace	"Replace Mechanism (Dispersal)          [%9c] : "
#define IOP_Replace	IOS_Replace

#define IOS_CrowFactor	"Crowding Factor                        : %d\n"
#define IOD_CrowFactor	"Crowding Factor                        [%9d] : "
#define IOP_CrowFactor	IOS_CrowFactor

#define IOS_Elite	"Elite Mechanism                        : %c\n"
#define IOD_Elite	"Elite Mechanism                        [%9c] : "
#define IOP_Elite	IOS_Elite

#define IOS_EliteSelect	"Elite Selection Mechanism              : %c\n"
#define IOD_EliteSelect	"Elite Selection Mechanism              [%9c] : "
#define IOP_EliteSelect	IOS_EliteSelect

#define IOS_EliteHold	"Number of Elites                       : %d\n"
#define IOD_EliteHold	"Number of Elites                       [%9d] : "
#define IOP_EliteHold	IOS_EliteHold

#define IOS_FitnessScale "Fitness Scaling Mechanism              : %c\n"
#define IOD_FitnessScale "Fitness Scaling Mechanism              [%9c] : "
#define IOP_FitnessScale IOS_FitnessScale

#define IOS_FitParA	"Fitness Scaling Parameter A            : %d\n"
#define IOD_FitParA	"Fitness Scaling Parameter A            [%9d] : "
#define IOP_FitParA	IOS_FitParA

#define IOS_FitParB	"Fitness Scaling Parameter B            : %d\n"
#define IOD_FitParB	"Fitness Scaling Parameter B            [%9d] : "
#define IOP_FitParB	IOS_FitParB

#define IOS_FitParC	"Fitness Scaling Parameter C            : %d\n"
#define IOD_FitParC	"Fitness Scaling Parameter C            [%9d] : "
#define IOP_FitParC	IOS_FitParC

#define IOS_FitParD	"Fitness Scaling Parameter D            : %d\n"
#define IOD_FitParD	"Fitness Scaling Parameter D            [%9d] : "
#define IOP_FitParD	IOS_FitParD

#define IOS_FilterTwins	"Filtering of Twins                     : %s\n"
#define IOD_FilterTwins	"Filtering of Twins                     [%9s] : "
#define IOP_FilterTwins	IOS_FilterTwins

#define IOS_Normalize	"Normalization of Tour                  : %s\n"
#define IOD_Normalize	"Normalization of Tour                  [%9s] : "
#define IOP_Normalize	IOS_Normalize

#define IOS_NormNum	"Number of Normalization                : %d\n"
#define IOD_NormNum	"Number of Normalization                [%9d] : "
#define IOP_NormNum	IOS_NormNum

#define IOS_ProcNum	"Number of Processors                   : %d\n"
#define IOD_ProcNum	"Number of Processors                   [%9d] : "
#define IOP_ProcNum	IOS_ProcNum

#define IOS_PopNum	"Number of Populations                  : %d\n"
#define IOD_PopNum	"Number of Populations                  [%9d] : "
#define IOP_PopNum	IOS_PopNum

#define IOS_ParModel	"Parallel Model                         : %c\n"
#define IOD_ParModel	"Parallel Model                         [%9c] : "
#define IOP_ParModel	IOS_ParModel

#define IOS_PollenDirection "Spread Direction of Pollen (Deg)       : %d\n"
#define IOD_PollenDirection "Spread Direction of Pollen (Deg)       [%9d] : "
#define IOP_PollenDirection IOS_PollenDirection

#define IOS_LowerWindForce "Lower Limit to Wind Force                 : %d\n"
#define IOD_LowerWindForce "Lower Limit to Wind Force                 [%9d] : "
#define IOP_LowerWindForce IOS_LowerWindForce

#define IOS_UpperWindForce "Upper Limit to Wind Force                 : %d\n"
#define IOD_UpperWindForce "Upper Limit to Wind Force                 [%9d] : "
#define IOP_UpperWindForce IOS_UpperWindForce

#define IOS_Topology	"Type of Topology for Communication     : %c\n"
#define IOD_Topology	"Type of Topology for Communication     [%9c] : "
#define IOP_Topology	IOS_Topology

#define IOS_CommInt	"Communication Interval (Gens)          : %d\n"
#define IOD_CommInt	"Communication Interval (Gens)          [%9d] : "
#define IOP_CommInt	IOS_CommInt

#define IOS_IndNum	"Number of Individuals to Change        : %d\n"
#define IOD_IndNum	"Number of Individuals to Change        [%9d] : "
#define IOP_IndNum	IOS_IndNum

#define IOS_TotalExperiments "Number of Experiments to Perform       : %d\n"
#define IOD_TotalExperiments "Number of Experiments to Perform       [%9d] : "
#define IOP_TotalExperiments IOS_TotalExperiments

#define IOS_TotalTrials	"Number of Trials per Experiment        : %d\n"
#define IOD_TotalTrials	"Number of Trials per Experiment        [%9d] : "
#define IOP_TotalTrials	IOS_TotalTrials

#define IOS_TotalGenerations "Number of Generations per Experiment   : %d\n"
#define IOD_TotalGenerations "Number of Generations per Experiment   [%9d] : "
#define IOP_TotalGenerations IOS_TotalGenerations

#define IOS_MinQuality	"Minimum of Tour Length to Abort        : %lf\n"
#define IOD_MinQuality	"Minimum of Tour Length to Abort        [%9.2f] : "
#define IOP_MinQuality	"Minimum of Tour Length to Abort        : %.2f\n"

#define IOS_Interval	"Report Interval, Evaluations           : %d\n"
#define IOD_Interval	"Report Interval, Evaluations           [%9d] : "
#define IOP_Interval	IOS_Interval

#define IOS_SaveSize	"Number of Individuals to Save          : %d\n"
#define IOD_SaveSize	"Number of Individuals to Save          [%9d] : "
#define IOP_SaveSize	IOS_SaveSize

#define IOS_MaxSpin	"Maximum No. of Gens. w/o Evaluation    : %d\n"
#define IOD_MaxSpin	"Maximum No. of Gens. w/o Evaluation    [%9d] : "
#define IOP_MaxSpin	IOS_MaxSpin

#define IOS_PgmFreq	"Interval for Tour Dumps (Gens)         : %d\n"
#define IOD_PgmFreq	"Interval for Tour Dumps (Gens)         [%9d] : "
#define IOP_PgmFreq	IOS_PgmFreq

#define IOS_DumpFreq	"Interval for Population Dumps (Gens)   : %d\n"
#define IOD_DumpFreq	"Interval for Population Dumps (Gens)   [%9d] : "
#define IOP_DumpFreq	IOS_DumpFreq

#define IOS_Options	"Internal Options                       : %s\n"
#define IOD_Options	"Internal Options                       [%9s] : "
#define IOP_Options	IOS_Options

#define IOS_RandomType	"Random Number Generator Type           : %c\n"
#define IOD_RandomType	"Random Number Generator Type           [%9c] : "
#define IOP_RandomType	IOS_RandomType

#define IOS_OrigSeed	"Seed for Random Number Generator       : %d\n"
#define IOD_OrigSeed	"Seed for Random Number Generator       [%9d] : "
#define IOP_OrigSeed	IOS_OrigSeed

#define IOS_GraphicsFlag "Save Tour for XTSP                     : %s\n"
#define IOD_GraphicsFlag "Save Tour for XTSP                     [%9s] : "
#define IOP_GraphicsFlag IOS_GraphicsFlag

#define IOS_DisplayFlag	"Show Results on Display                : %s\n"
#define IOD_DisplayFlag	"Show Results on Display                [%9s] : "
#define IOP_DisplayFlag	IOS_DisplayFlag

#define IOS_PopDisplay	"Population No. to Show on Display      : %d\n"
#define IOD_PopDisplay	"Population No. to Show on Display      [%9d] : "
#define IOP_PopDisplay	IOS_PopDisplay

#define IOS_OutFlag	"Write to Output Files                  : %s\n"
#define IOD_OutFlag	"Write to Output Files                  [%9s] : "
#define IOP_OutFlag	IOS_OutFlag

#define IOS_ReportFlag	"Running Report                         : %s\n"
#define IOD_ReportFlag	"Running Report                         [%9s] : "
#define IOP_ReportFlag	IOS_ReportFlag

#define IOD_Suffix      "Suffix of GA-Data Infile\n  [%46s] : "


static char *nystr[2] = { "No", "Yes" };	/* no/yes - string */


/*****************************************************************************/
/* Initialize parameters with predefined values                              */
/*****************************************************************************/
void init_values()
{
  trace("init_values() entered");

  strcpy(TspFileName, DEF_TspFileName);
  BreederSelect = DEF_BreederSelect;
  CrossRate = DEF_CrossRate;
  MutDim = DEF_MutDim;
  Elite = DEF_Elite;
  FitnessScale = DEF_FitnessScale;
  GapSize = DEF_GapSize;
  EliteHold = DEF_EliteHold;
  PopInit = DEF_PopInit;
  EliteSelect = DEF_EliteSelect;
  strcpy(Mutation, DEF_Mutation);
  NormNum = DEF_NormNum;
  strcpy(GA_Options, DEF_GA_Options);
  CrossTwoOff = FALSE;
  Normalize = FALSE;
  FilterTwins = FALSE;
  PopSize = DEF_PopSize;
  CrowFactor = DEF_CrowFactor;
  Replace = DEF_Replace;
  MateSelect = DEF_MateSelect;
  OptLocal = DEF_OptLocal;
  WindowSize = DEF_WindowSize;
  Crossover = DEF_Crossover;
  MutRate = DEF_MutRate;
  FitParA = DEF_FitParA;
  FitParB = DEF_FitParB;
  FitParC = DEF_FitParC;
  FitParD = DEF_FitParD;
  ChnLen = DEF_ChnLen;
  LowerIndex = DEF_LowerIndex;
  EtaMax = DEF_EtaMax;
  CtrlParam = DEF_CtrlParam;
  UpperIndex = DEF_UpperIndex;
  CrossInt = DEF_CrossInt;
  strcpy(TourFileName, DEF_TourFileName);
  PgmFreq = DEF_PgmFreq;
  GraphicsFlag = DEF_GraphicsFlag;
  DisplayFlag = DEF_DisplayFlag;
  TotalExperiments = DEF_TotalExperiments;
  DumpFreq = DEF_DumpFreq;
  TotalGenerations = DEF_TotalGenerations;
  strcpy(InFile, DEF_InFile);
  OutFlag = DEF_OutFlag;
  MinQuality = DEF_MinQuality;
  SaveSize = DEF_SaveSize;
  strcpy(Options, DEF_Options);
  AllFlag = FALSE;
  LastFlag = FALSE;
  NoTermFlag = FALSE;
  PrintPopFlag = FALSE;
  SchemaFlag = FALSE;
  TraceFlag = FALSE;
  VarFlag = FALSE;
  CommInt = DEF_CommInt;
  ParModel = DEF_ParModel;
  IndNum = DEF_IndNum;
  PopNum = DEF_PopNum;
  ProcNum = DEF_ProcNum;
  PollenDirection = DEF_PollenDirection;
  Topology = DEF_Topology;
  LowerWindForce = DEF_LowerWindForce;
  UpperWindForce = DEF_UpperWindForce;
  QueryFlag = DEF_QueryFlag;
  RandomType = DEF_RandomType;
  OrigSeed = DEF_OrigSeed;
  TotalTrials = DEF_TotalTrials;
  PopDisplay = DEF_PopDisplay;
  Interval = DEF_Interval;
  ReportFlag = DEF_ReportFlag;
  strcpy(Suffix, DEF_Suffix);
  MaxSpin = DEF_MaxSpin;

  strcpy(Msg, "");

  trace("init_values() completed");
}


/*****************************************************************************/
/* Get no/yes                                                                */
/*****************************************************************************/
char *no_yes(b)
  BOOLEAN b;		/* flag */
{
  return(nystr[(b) ? 1 : 0]);
}


/*****************************************************************************/
/* Check if string = yes                                                     */
/*****************************************************************************/
BOOLEAN is_yes(s)
  char *s;		/* string */
{ register int res;

  trace("is_yes() entered");

  res = ! (strcmp(s, no_yes(TRUE)));
  res |= ! (strcmp(s, "yes"));
  res |= ! (strcmp(s, "Y"));
  res |= ! (strcmp(s, "y"));

  trace("is_yes() completed");

  return((res) ? TRUE : FALSE);
}


/*****************************************************************************/
/* Write parameters in input file                                            */
/*****************************************************************************/
int write_data(fp)
  FILE *fp;		/* file pointer */
{ register int res;

  trace("write_data() entered");

  res = io_TspFileName(fp, WTDATA);

  trace("write_data() completed");

  return(res);
}


/*****************************************************************************/
/* Read parameters from input file                                           */
/*****************************************************************************/
int get_data(fp)
  FILE *fp;		/* file pointer */
{ register int res, i = 0;
  char s[4];

  trace("get_data() entered");

  if (fp == stdin)
  { init_values();
  }

  res = io_TspFileName(fp, RDDATA);

  if (CrossTwoOff)
  { s[i++] = OPT_CrossTwoOff;
  }
  if (Normalize)
  { s[i++] = OPT_Normalize;
  }
  if (FilterTwins)
  { s[i++] = OPT_FilterTwins;
  }
  s[i] = '\0';
  strcpy(GA_Options, s);

  trace("get_data() completed");

  return(res);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_TspFileName(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_TspFileName, TspFileName);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%s", TspFileName);
      }
    }
    else
    { fscanf(fp, IOS_TspFileName, TspFileName);
    }
  }
  else
  { fprintf(fp, IOP_TspFileName, TspFileName);
  }

  io_PopSize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PopSize(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PopSize, PopSize);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &PopSize);
      }
    }
    else
    { fscanf(fp, IOS_PopSize, &PopSize);
    }
  }
  else
  { fprintf(fp, IOP_PopSize, PopSize);
  }

  io_PopInit(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PopInit(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PopInit, PopInit);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &PopInit);
      }
    }
    else
    { fscanf(fp, IOS_PopInit, &PopInit);
    }
  }
  else
  { fprintf(fp, IOP_PopInit, PopInit);
  }

  if ((PopInit == POP_TOU) || (PopInit == POP_ALL))
  { io_TourFileName(fp, flag);
  }
  else
  { io_GapSize(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_TourFileName(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_TourFileName, TourFileName);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%s", TourFileName);
      }
    }
    else
    { fscanf(fp, IOS_TourFileName, TourFileName);
    }
  }
  else
  { fprintf(fp, IOP_TourFileName, TourFileName);
  }

  io_GapSize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_GapSize(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_GapSize, GapSize);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &GapSize);
      }
    }
    else
    { fscanf(fp, IOS_GapSize, &GapSize);
    }
  }
  else
  { fprintf(fp, IOP_GapSize, GapSize);
  }

  io_BreederSelect(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_BreederSelect(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_BreederSelect, BreederSelect);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &BreederSelect);
      }
    }
    else
    { fscanf(fp, IOS_BreederSelect, &BreederSelect);
    }
  }
  else
  { fprintf(fp, IOP_BreederSelect, BreederSelect);
  }

  switch (BreederSelect)
  { case BRS_INV:
    case BRS_GAU:
      io_WindowSize(fp, flag);
      break;
    case BRS_RND:
    case BRS_PRO:
      io_LowerIndex(fp, flag);
      break;
    case BRS_WHI:
      io_Whitley(fp, flag);
      break;
    case BRS_LIR:
    case BRS_ILR:
      io_EtaMax(fp, flag);
      break;
    case BRS_BOL:
      io_CtrlParam(fp, flag);
      break;
    default:
      io_WindowSize(fp, flag);
      break;
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Whitley(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Whitley, EtaMax);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &EtaMax);
      }
    }
    else
    { fscanf(fp, IOS_Whitley, &EtaMax);
    }
  }
  else
  { fprintf(fp, IOP_Whitley, EtaMax);
  }

  io_WindowSize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_EtaMax(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_EtaMax, EtaMax);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &EtaMax);
      }
    }
    else
    { fscanf(fp, IOS_EtaMax, &EtaMax);
    }
  }
  else
  { fprintf(fp, IOP_EtaMax, EtaMax);
  }

  io_LowerIndex(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CtrlParam(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CtrlParam, CtrlParam);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &CtrlParam);
      }
    }
    else
    { fscanf(fp, IOS_CtrlParam, &CtrlParam);
    }
  }
  else
  { fprintf(fp, IOP_CtrlParam, CtrlParam);
  }

  io_ChnLen(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_ChnLen(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_ChnLen, ChnLen);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &ChnLen);
      }
    }
    else
    { fscanf(fp, IOS_ChnLen, &ChnLen);
    }
  }
  else
  { fprintf(fp, IOP_ChnLen, ChnLen);
  }

  io_LowerIndex(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_LowerIndex(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_LowerIndex, LowerIndex);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &LowerIndex);
      }
    }
    else
    { fscanf(fp, IOS_LowerIndex, &LowerIndex);
    }
  }
  else
  { fprintf(fp, IOP_LowerIndex, LowerIndex);
  }

  io_UpperIndex(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_UpperIndex(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_UpperIndex, UpperIndex);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &UpperIndex);
      }
    }
    else
    { fscanf(fp, IOS_UpperIndex, &UpperIndex);
    }
  }
  else
  { fprintf(fp, IOP_UpperIndex, UpperIndex);
  }

  io_WindowSize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_WindowSize(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_WindowSize, WindowSize);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &WindowSize);
      }
    }
    else
    { fscanf(fp, IOS_WindowSize, &WindowSize);
    }
  }
  else
  { fprintf(fp, IOP_WindowSize, WindowSize);
  }

  io_MateSelect(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_MateSelect(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_MateSelect, MateSelect);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &MateSelect);
      }
    }
    else
    { fscanf(fp, IOS_MateSelect, &MateSelect);
    }
  }
  else
  { fprintf(fp, IOP_MateSelect, MateSelect);
  }

  io_Crossover(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Crossover(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Crossover, Crossover);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &Crossover);
      }
    }
    else
    { fscanf(fp, IOS_Crossover, &Crossover);
    }
  }
  else
  { fprintf(fp, IOP_Crossover, Crossover);
  }

  if (Crossover == CRO_NOP)
  { io_OptLocal(fp, flag);
  }
  else
  { io_CrossRate(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CrossRate(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CrossRate, CrossRate);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &CrossRate);
      }
    }
    else
    { fscanf(fp, IOS_CrossRate, &CrossRate);
    }
  }
  else
  { fprintf(fp, IOP_CrossRate, CrossRate);
  }

  io_CrossTwoOff(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CrossTwoOff(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CrossTwoOff, no_yes(CrossTwoOff));
      gets(s);
      if (strlen(s))
      { CrossTwoOff = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_CrossTwoOff, s);
      CrossTwoOff = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_CrossTwoOff, no_yes(CrossTwoOff));
  }

  if ((Crossover == CRO_INT) || (Crossover == CRO_RND))
  { io_CrossInt(fp, flag);
  }
  else
  { io_OptLocal(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CrossInt(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CrossInt, CrossInt);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &CrossInt);
      }
    }
    else
    { fscanf(fp, IOS_CrossInt, &CrossInt);
    }
  }
  else
  { fprintf(fp, IOP_CrossInt, CrossInt);
  }

  io_OptLocal(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_OptLocal(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_OptLocal, OptLocal);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &OptLocal);
      }
    }
    else
    { fscanf(fp, IOS_OptLocal, &OptLocal);
    }
  }
  else
  { fprintf(fp, IOP_OptLocal, OptLocal);
  }

  io_Mutation(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Mutation(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Mutation, Mutation);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%s", Mutation);
      }
    }
    else
    { fscanf(fp, IOS_Mutation, Mutation);
    }
  }
  else
  { fprintf(fp, IOP_Mutation, Mutation);
  }

  if (Mutation[0] == MUT_NOP)
  { io_Replace(fp, flag);
  }
  else
  { io_MutRate(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_MutRate(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_MutRate, MutRate);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &MutRate);
      }
    }
    else
    { fscanf(fp, IOS_MutRate, &MutRate);
    }
  }
  else
  { fprintf(fp, IOP_MutRate, MutRate);
  }

  io_MutDim(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_MutDim(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_MutDim, MutDim);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &MutDim);
      }
    }
    else
    { fscanf(fp, IOS_MutDim, &MutDim);
    }
  }
  else
  { fprintf(fp, IOP_MutDim, MutDim);
  }

  io_Replace(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Replace(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Replace, Replace);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &Replace);
      }
    }
    else
    { fscanf(fp, IOS_Replace, &Replace);
    }
  }
  else
  { fprintf(fp, IOP_Replace, Replace);
  }

  if (Replace == REP_CRW)
  { io_CrowFactor(fp, flag);
  }
  else
  { io_Elite(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CrowFactor(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CrowFactor, CrowFactor);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &CrowFactor);
      }
    }
    else
    { fscanf(fp, IOS_CrowFactor, &CrowFactor);
    }
  }
  else
  { fprintf(fp, IOP_CrowFactor, CrowFactor);
  }

  io_Elite(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Elite(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Elite, Elite);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &Elite);
      }
    }
    else
    { fscanf(fp, IOS_Elite, &Elite);
    }
  }
  else
  { fprintf(fp, IOP_Elite, Elite);
  }

  if (Elite == ELI_NOP)
  { io_FitnessScale(fp, flag);
  }
  else
  { io_EliteSelect(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_EliteSelect(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_EliteSelect, EliteSelect);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &EliteSelect);
      }
    }
    else
    { fscanf(fp, IOS_EliteSelect, &EliteSelect);
    }
  }
  else
  { fprintf(fp, IOP_EliteSelect, EliteSelect);
  }

  io_EliteHold(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_EliteHold(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_EliteHold, EliteHold);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &EliteHold);
      }
    }
    else
    { fscanf(fp, IOS_EliteHold, &EliteHold);
    }
  }
  else
  { fprintf(fp, IOP_EliteHold, EliteHold);
  }

  io_FitnessScale(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FitnessScale(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FitnessScale, FitnessScale);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &FitnessScale);
      }
    }
    else
    { fscanf(fp, IOS_FitnessScale, &FitnessScale);
    }
  }
  else
  { fprintf(fp, IOP_FitnessScale, FitnessScale);
  }

  if (FitnessScale == FIT_NOP)
  { io_FilterTwins(fp, flag);
  }
  else
  { io_FitParA(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FitParA(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FitParA, FitParA);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &FitParA);
      }
    }
    else
    { fscanf(fp, IOS_FitParA, &FitParA);
    }
  }
  else
  { fprintf(fp, IOP_FitParA, FitParA);
  }

  io_FitParB(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FitParB(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FitParB, FitParB);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &FitParB);
      }
    }
    else
    { fscanf(fp, IOS_FitParB, &FitParB);
    }
  }
  else
  { fprintf(fp, IOP_FitParB, FitParB);
  }

  io_FitParC(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FitParC(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FitParC, FitParC);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &FitParC);
      }
    }
    else
    { fscanf(fp, IOS_FitParC, &FitParC);
    }
  }
  else
  { fprintf(fp, IOP_FitParC, FitParC);
  }

  io_FitParD(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FitParD(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FitParD, FitParD);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &FitParD);
      }
    }
    else
    { fscanf(fp, IOS_FitParD, &FitParD);
    }
  }
  else
  { fprintf(fp, IOP_FitParD, FitParD);
  }

  io_FilterTwins(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_FilterTwins(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_FilterTwins, no_yes(FilterTwins));
      gets(s);
      if (strlen(s))
      { FilterTwins = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_FilterTwins, s);
      FilterTwins = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_FilterTwins, no_yes(FilterTwins));
  }

  io_Normalize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Normalize(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Normalize, no_yes(Normalize));
      gets(s);
      if (strlen(s))
      { Normalize = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_Normalize, s);
      Normalize = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_Normalize, no_yes(Normalize));
  }

  if (Normalize)
  { io_NormNum(fp, flag);
  }
  else
  {
#if (defined(PARIX) || defined(PARIX_HOST))
    io_ProcNum(fp, flag);
#else
    io_PopNum(fp, flag);
#endif
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_NormNum(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_NormNum, NormNum);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &NormNum);
      }
    }
    else
    { fscanf(fp, IOS_NormNum, &NormNum);
    }
  }
  else
  { fprintf(fp, IOP_NormNum, NormNum);
  }

#if (defined(PARIX) || defined(PARIX_HOST))
  io_ProcNum(fp, flag);
#else
  io_PopNum(fp, flag);
#endif

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_ProcNum(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_ProcNum, ProcNum);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &ProcNum);
      }
    }
    else
    { fscanf(fp, IOS_ProcNum, &ProcNum);
    }
  }
  else
  { fprintf(fp, IOP_ProcNum, ProcNum);
  }

  io_PopNum(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PopNum(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PopNum, PopNum);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &PopNum);
      }
    }
    else
    { fscanf(fp, IOS_PopNum, &PopNum);
    }
  }
  else
  { fprintf(fp, IOP_PopNum, PopNum);
  }

  if (PopNum > 1)
  { io_ParModel(fp, flag);
  }
  else
  { io_TotalExperiments(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_ParModel(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_ParModel, ParModel);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &ParModel);
      }
    }
    else
    { fscanf(fp, IOS_ParModel, &ParModel);
    }
  }
  else
  { fprintf(fp, IOP_ParModel, ParModel);
  }

  switch (ParModel)
  { case PAR_ISL:
      io_TotalExperiments(fp, flag);
      break;
    case PAR_TOK:
      io_IndNum(fp, flag);
      break;
    case PAR_POL:
      io_PollenDirection(fp, flag);
      break;
    case PAR_NEI:
      io_Topology(fp, flag);
      break;
    default:
      io_TotalExperiments(fp, flag);
      break;
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PollenDirection(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PollenDirection, PollenDirection);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &PollenDirection);
      }
    }
    else
    { fscanf(fp, IOS_PollenDirection, &PollenDirection);
    }
  }
  else
  { fprintf(fp, IOP_PollenDirection, PollenDirection);
  }

  io_LowerWindForce(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_LowerWindForce(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_LowerWindForce, LowerWindForce);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &LowerWindForce);
      }
    }
    else
    { fscanf(fp, IOS_LowerWindForce, &LowerWindForce);
    }
  }
  else
  { fprintf(fp, IOP_LowerWindForce, LowerWindForce);
  }

  io_UpperWindForce(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_UpperWindForce(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_UpperWindForce, UpperWindForce);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &UpperWindForce);
      }
    }
    else
    { fscanf(fp, IOS_UpperWindForce, &UpperWindForce);
    }
  }
  else
  { fprintf(fp, IOP_UpperWindForce, UpperWindForce);
  }

  io_IndNum(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Topology(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Topology, Topology);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &Topology);
      }
    }
    else
    { fscanf(fp, IOS_Topology, &Topology);
    }
  }
  else
  { fprintf(fp, IOP_Topology, Topology);
  }

  io_IndNum(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_IndNum(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_IndNum, IndNum);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &IndNum);
      }
    }
    else
    { fscanf(fp, IOS_IndNum, &IndNum);
    }
  }
  else
  { fprintf(fp, IOP_IndNum, IndNum);
  }

  io_CommInt(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_CommInt(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_CommInt, CommInt);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &CommInt);
      }
    }
    else
    { fscanf(fp, IOS_CommInt, &CommInt);
    }
  }
  else
  { fprintf(fp, IOP_CommInt, CommInt);
  }

  io_TotalExperiments(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_TotalExperiments(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_TotalExperiments, TotalExperiments);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &TotalExperiments);
      }
    }
    else
    { fscanf(fp, IOS_TotalExperiments, &TotalExperiments);
    }
  }
  else
  { fprintf(fp, IOP_TotalExperiments, TotalExperiments);
  }

  io_TotalTrials(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_TotalTrials(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_TotalTrials, TotalTrials);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &TotalTrials);
      }
    }
    else
    { fscanf(fp, IOS_TotalTrials, &TotalTrials);
    }
  }
  else
  { fprintf(fp, IOP_TotalTrials, TotalTrials);
  }

  if (TotalTrials == 0)
  { io_TotalGenerations(fp, flag);
  }
  else
  { io_MinQuality(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_TotalGenerations(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_TotalGenerations, TotalGenerations);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &TotalGenerations);
      }
    }
    else
    { fscanf(fp, IOS_TotalGenerations, &TotalGenerations);
    }
  }
  else
  { fprintf(fp, IOP_TotalGenerations, TotalGenerations);
  }

  io_MinQuality(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_MinQuality(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_MinQuality, MinQuality);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%lf", &MinQuality);
      }
    }
    else
    { fscanf(fp, IOS_MinQuality, &MinQuality);
    }
  }
  else
  { fprintf(fp, IOP_MinQuality, MinQuality);
  }

  io_Interval(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Interval(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Interval, Interval);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &Interval);
      }
    }
    else
    { fscanf(fp, IOS_Interval, &Interval);
    }
  }
  else
  { fprintf(fp, IOP_Interval, Interval);
  }

  io_SaveSize(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_SaveSize(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_SaveSize, SaveSize);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &SaveSize);
      }
    }
    else
    { fscanf(fp, IOS_SaveSize, &SaveSize);
    }
  }
  else
  { fprintf(fp, IOP_SaveSize, SaveSize);
  }

  io_MaxSpin(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_MaxSpin(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_MaxSpin, MaxSpin);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &MaxSpin);
      }
    }
    else
    { fscanf(fp, IOS_MaxSpin, &MaxSpin);
    }
  }
  else
  { fprintf(fp, IOP_MaxSpin, MaxSpin);
  }

  io_PgmFreq(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PgmFreq(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PgmFreq, PgmFreq);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &PgmFreq);
      }
    }
    else
    { fscanf(fp, IOS_PgmFreq, &PgmFreq);
    }
  }
  else
  { fprintf(fp, IOP_PgmFreq, PgmFreq);
  }

  io_DumpFreq(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_DumpFreq(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_DumpFreq, DumpFreq);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &DumpFreq);
      }
    }
    else
    { fscanf(fp, IOS_DumpFreq, &DumpFreq);
    }
  }
  else
  { fprintf(fp, IOP_DumpFreq, DumpFreq);
  }

  io_Options(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Options(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_Options, Options);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%s", Options);
      }
    }
    else
    { fscanf(fp, IOS_Options, Options);
    }
  }
  else
  { fprintf(fp, IOP_Options, Options);
  }

  io_RandomType(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_RandomType(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_RandomType, RandomType);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%c", &RandomType);
      }
    }
    else
    { fscanf(fp, IOS_RandomType, &RandomType);
    }
  }
  else
  { fprintf(fp, IOP_RandomType, RandomType);
  }

  io_OrigSeed(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_OrigSeed(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_OrigSeed, OrigSeed);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &OrigSeed);
      }
    }
    else
    { fscanf(fp, IOS_OrigSeed, &OrigSeed);
    }
  }
  else
  { fprintf(fp, IOP_OrigSeed, OrigSeed);
  }

  io_GraphicsFlag(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_GraphicsFlag(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_GraphicsFlag, no_yes(GraphicsFlag));
      gets(s);
      if (strlen(s))
      { GraphicsFlag = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_GraphicsFlag, s);
      GraphicsFlag = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_GraphicsFlag, no_yes(GraphicsFlag));
  }

  io_DisplayFlag(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_DisplayFlag(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_DisplayFlag, no_yes(DisplayFlag));
      gets(s);
      if (strlen(s))
      { DisplayFlag = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_DisplayFlag, s);
      DisplayFlag = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_DisplayFlag, no_yes(DisplayFlag));
  }

  if (DisplayFlag)
  { io_PopDisplay(fp, flag);
  }
  else
  { io_OutFlag(fp, flag);
  }

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_PopDisplay(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_PopDisplay, PopDisplay);
      gets(s);
      if (strlen(s))
      { sscanf(s, "%d", &PopDisplay);
      }
    }
    else
    { fscanf(fp, IOS_PopDisplay, &PopDisplay);
    }
  }
  else
  { fprintf(fp, IOP_PopDisplay, PopDisplay);
  }

  io_OutFlag(fp, flag);

  return(0);
}

/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_OutFlag(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_OutFlag, no_yes(OutFlag));
      gets(s);
      if (strlen(s))
      { OutFlag = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_OutFlag, s);
      OutFlag = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_OutFlag, no_yes(OutFlag));
  }

  io_ReportFlag(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_ReportFlag(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag)
  { if (fp == stdin)
    { printf(IOD_ReportFlag, no_yes(ReportFlag));
      gets(s);
      if (strlen(s))
      { ReportFlag = is_yes(s);
      }
    }
    else
    { fscanf(fp, IOS_ReportFlag, s);
      ReportFlag = is_yes(s);
    }
  }
  else
  { fprintf(fp, IOP_ReportFlag, no_yes(ReportFlag));
  }

  io_Suffix(fp, flag);

  return(0);
}


/*****************************************************************************/
/*                                                                           */
/*****************************************************************************/
int io_Suffix(fp, flag)
  FILE *fp;
  register int flag;
{ char s[MAX_STR];

  if (flag && (fp == stdin))
  { strcpy(Suffix, crt_file_name());
    printf(IOD_Suffix, Suffix);
    gets(s);
    if (strlen(s))
    { sscanf(s, "%s", Suffix);
    }
  }

  return(0);
}


/*****************************************************************************/
/* Get file name from path name                                              */
/*****************************************************************************/
char *get_file_name(s)
  char *s;		/* path name */
{ register int i, len;

  trace("get_file_name() entered");

  len = strlen(s);
  for (i = len; (i > 0) && (s[i - 1] != '/'); i--) ;

  trace("get_file_name() completed");

  return(&s[i]);
}


/*****************************************************************************/
/* Create file name for actual running                                       */
/*****************************************************************************/
char *crt_file_name()
{ char fname[MAX_STR], s[MAX_STR];

  trace("crt_file_name() entered");

  strcpy(fname, "t.");

  sprintf(s, "%s:", get_file_name(TspFileName));
  strcat(fname, s);

  sprintf(s, "%d", PopSize);
  strcat(fname, s); 

  sprintf(s, "%c", PopInit);
  strcat(fname, s);

  sprintf(s, "%g", GapSize);
  strcat(fname, s);

  sprintf(s, "%c", BreederSelect);
  strcat(fname, s);

  sprintf(s, "%c", MateSelect);
  strcat(fname, s);

  sprintf(s, "%c", Crossover);
  strcat(fname, s);

  sprintf(s, "%g", CrossRate);
  strcat(fname, s);

  sprintf(s, "%c", (CrossTwoOff) ? OPT_CrossTwoOff : '_');
  strcat(fname, s);

  sprintf(s, "%s", Mutation);
  strcat(fname, s);

  sprintf(s, "%g:", MutRate);
  strcat(fname, s);

  sprintf(s, "%g", MutDim);
  strcat(fname, s);

  sprintf(s, "%c", Replace);
  strcat(fname, s);

  sprintf(s, "%c", Elite);
  strcat(fname, s);

  sprintf(s, "%c", EliteSelect);
  strcat(fname, s);

  sprintf(s, "%d", EliteHold);
  strcat(fname, s);

  sprintf(s, "%c", FitnessScale);
  strcat(fname, s);

  sprintf(s, "%c", OptLocal);
  strcat(fname, s);

  sprintf(s, "%c", (FilterTwins) ? OPT_FilterTwins : '_');
  strcat(fname, s);

  sprintf(s, "%c", (Normalize) ? OPT_Normalize : '_');
  strcat(fname, s);

  trace("crt_file_name() completed");

  return(fname);
}


/*** end of file ***/
