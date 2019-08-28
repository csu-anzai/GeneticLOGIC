/* $Id: input.c,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : input.c                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "define.h"
#include "global.h"
#include "chrom.h"
#include "dbuff.h"
#include "dump.h"
#include "error.h"
#include "eval.h"
#include "fopen.h"
#include "getopt.h"
#include "init.h"
#include "interfac.h"
#include "other.h"
#include "parallel.h"
#include "readfile.h"
#include "trace.h"
#include "input.h"


/*****************************************************************************/
/* Set option flags                                                          */
/*****************************************************************************/
#ifdef PARIX
void set_flag(char c)
#else
void set_flag(c)
  char c;		/* option character */
#endif
{
  trace("set_flag() entered");

  switch (c)
  { case OPT_CrossTwoOff:
      CrossTwoOff = TRUE;
      CrossOff = 2;
      break;
    case OPT_Normalize:
      Normalize = TRUE;
      break;
    case OPT_FilterTwins:
      FilterTwins = TRUE;
      break;
    case OPT_AllFlag:
      AllFlag = TRUE;
      break;
    case OPT_LastFlag:
      LastFlag = TRUE;
      break;
    case OPT_NoTermFlag:
      NoTermFlag = TRUE;
      break;
    case OPT_PrintPopFlag:
      PrintPopFlag = TRUE;
      break;
    case OPT_SchemaFlag:
      SchemaFlag = TRUE;
      break;
    case OPT_TraceFlag:
      TraceFlag = TRUE;
      break;
    case OPT_VarFlag:
      VarFlag = TRUE;
      break;
    default:
      sprintf(Msg, "SetFlag: Wrong flag passed (%c)", c);
      critical_error(ERR_OPTION, Msg);
      break;
  }

  trace("set_flag() completed");
}


/*****************************************************************************/
/* Read parameters and initialize                                            */
/*****************************************************************************/
void input(argc, argv)
  int argc;		/* number of arguments */
  char **argv;		/* pointer to arguments */
{ register int i, o;
  FILE *fp;
  BOOLEAN inf = FALSE;

  trace("input() entered");

  init_values();
  strcpy(LogFile, "");

  my_opterr = 0;

  while ((o = get_options(argc, argv, OPT_STR)) != EOF)
  { switch ((char) o)
    { case OPT_TspFileName:
        sscanf(my_optarg, "%s", TspFileName);
        break;
      case OPT_BreederSelect:
        sscanf(my_optarg, "%c", &BreederSelect);
        break;
      case OPT_CrossRate:
        sscanf(my_optarg, "%lf", &CrossRate);
        break;
      case OPT_MutDim:
        sscanf(my_optarg, "%lf", &MutDim);
        break;
      case OPT_Elite:
        sscanf(my_optarg, "%c", &Elite);
        break;
      case OPT_FitnessScale:
        sscanf(my_optarg, "%c", &FitnessScale);
        break;
      case OPT_GapSize:
        sscanf(my_optarg, "%lf", &GapSize);
        break;
      case OPT_EliteHold:
        sscanf(my_optarg, "%d", &EliteHold);
        break;
      case OPT_PopInit:
        sscanf(my_optarg, "%c", &PopInit);
        break;
      case OPT_EliteSelect:
        sscanf(my_optarg, "%c", &EliteSelect);
        break;
      case OPT_Mutation:
        sscanf(my_optarg, "%s", Mutation);
        break;
      case OPT_NormNum:
        sscanf(my_optarg, "%d", &NormNum);
        break;
      case OPT_GA_Options:
        sscanf(my_optarg, "%s", GA_Options);
        break;
      case OPT_PopSize:
        sscanf(my_optarg, "%d", &PopSize);
        break;
      case OPT_CrowFactor:
        sscanf(my_optarg, "%d", &CrowFactor);
        break;
      case OPT_Replace:
        sscanf(my_optarg, "%c", &Replace);
        break;
      case OPT_MateSelect:
        sscanf(my_optarg, "%c", &MateSelect);
        break;
      case OPT_OptLocal:
        sscanf(my_optarg, "%c", &OptLocal);
        break;
      case OPT_WindowSize:
        sscanf(my_optarg, "%d", &WindowSize);
        break;
      case OPT_Crossover:
        sscanf(my_optarg, "%c", &Crossover);
        break;
      case OPT_MutRate:
        sscanf(my_optarg, "%lf", &MutRate);
        break;
      case OPT_Other:
        switch (my_optarg[0])
        { case OPT_FitParA:
            sscanf(&my_optarg[1], "%d", &FitParA);
            break;
          case OPT_FitParB:
            sscanf(&my_optarg[1], "%d", &FitParB);
            break;
          case OPT_FitParC:
            sscanf(&my_optarg[1], "%d", &FitParC);
            break;
          case OPT_FitParD:
            sscanf(&my_optarg[1], "%d", &FitParD);
            break;
          case OPT_ChnLen:
            sscanf(&my_optarg[1], "%d", &ChnLen);
            break;
          case OPT_LowerIndex:
            sscanf(&my_optarg[1], "%d", &LowerIndex);
            break;
          case OPT_EtaMax:
            sscanf(my_optarg, "%lf", &EtaMax);
            break;
          case OPT_CtrlParam:
            sscanf(my_optarg, "%lf", &CtrlParam);
            break;
          case OPT_UpperIndex:
            sscanf(&my_optarg[1], "%d", &UpperIndex);
            break;
          case OPT_CrossInt:
            sscanf(&my_optarg[1], "%d", &CrossInt);
            break;
          default:
            sprintf(Msg, "Input: Illegal option -%c%c\n", OPT_Other,
              my_optarg[0]);
            critical_error(ERR_OPTION, Msg);
            break;
        }
        break;
      case OPT_TourFileName:
        sscanf(my_optarg, "%s", TourFileName);
        break;
      case OPT_PgmFreq:
        sscanf(my_optarg, "%d", &PgmFreq);
        break;
      case OPT_GraphicsFlag:
        GraphicsFlag = TRUE;
        break;
      case OPT_DisplayFlag:
        DisplayFlag = TRUE;
        break;
      case OPT_TotalExperiments:
        sscanf(my_optarg, "%d", &TotalExperiments);
        break;
      case OPT_DumpFreq:
        sscanf(my_optarg, "%d", &DumpFreq);
        break;
      case OPT_TotalGenerations:
        sscanf(my_optarg, "%d", &TotalGenerations);
        break;
      case OPT_InFile:
        sscanf(my_optarg, "%s", InFile);
        if ((fp = file_open(InFile, "r", FALSE)) != NULL)
        { get_data(fp);
          fclose(fp);
        }
        else
        { sprintf(Msg, "Input: can't open '%s'", InFile);
          critical_error(ERR_FILE_OPEN, Msg);
        }
        sscanf(InFile, "in.%s", Suffix);
        inf = TRUE;
        break;
      case OPT_OutFlag:
        OutFlag = FALSE;
        break;
      case OPT_MinQuality:
        sscanf(my_optarg, "%lf", &MinQuality);
        break;
      case OPT_SaveSize:
        sscanf(my_optarg, "%d", &SaveSize);
        break;
      case OPT_Options:
        sscanf(my_optarg, "%s", Options);
        break;
      case OPT_Parallel:
        switch (my_optarg[0])
        { case OPT_CommInt:
            sscanf(&my_optarg[1], "%d", &CommInt);
            break;
          case OPT_ParModel:
            sscanf(&my_optarg[1], "%c", &ParModel);
            break;
          case OPT_IndNum:
            sscanf(&my_optarg[1], "%d", &IndNum);
            break;
          case OPT_PopNum:
            sscanf(&my_optarg[1], "%d", &PopNum);
            break;
          case OPT_ProcNum:
            sscanf(&my_optarg[1], "%d", &ProcNum);
            break;
          case OPT_PollenDirection:
            sscanf(&my_optarg[1], "%d", &PollenDirection);
            break;
          case OPT_Topology:
            sscanf(&my_optarg[1], "%c", &Topology);
            break;
          case OPT_LowerWindForce:
            sscanf(&my_optarg[1], "%d", &LowerWindForce);
            break;
          case OPT_UpperWindForce:
            sscanf(&my_optarg[1], "%d", &UpperWindForce);
            break;
          default:
            sprintf(Msg, "Input: Illegal option -%c%c\n", OPT_Parallel,
              my_optarg[0]);
            critical_error(ERR_OPTION, Msg);
            break;
        }
        break;
      case OPT_QueryFlag:
        QueryFlag = TRUE;
        break;
      case OPT_RandomType:
        sscanf(my_optarg, "%c", &RandomType);
        break;
      case OPT_OrigSeed:
        sscanf(my_optarg, "%d", &OrigSeed);
        break;
      case OPT_TotalTrials:
        sscanf(my_optarg, "%d", &TotalTrials);
        break;
      case OPT_PopDisplay:
	sscanf(my_optarg, "%d", &PopDisplay);
	break;
      case OPT_Interval:
        sscanf(my_optarg, "%d", &Interval);
        break;
      case OPT_ReportFlag:
        ReportFlag = FALSE;
        break;
      case OPT_Suffix:
        sscanf(my_optarg, "%s", Suffix);
        break;
      case OPT_MaxSpin:
        sscanf(my_optarg, "%d", &MaxSpin);
        break;
      case 'h':
      case '?':
        if (MyProcID == DISP_PROC)
        { printf("%s", USAGE);
        }
        exit(ERR_HELP);
        break;
      default:
        sprintf(Msg, "Input: illegal option -%c\n", o);
        critical_error(ERR_OPTION, Msg);
        break;
    }
  }

  for (i = 0; GA_Options[i] != '\0'; i++)
  { set_flag(GA_Options[i]);
  }
  for (i = 0; Options[i] != '\0'; i++)
  { set_flag(Options[i]);
  }

  if ((PopInit == POP_TOU) || (PopInit == POP_ALL))
  { read_TOUR_file(TourFileName);
  }

  read_TSP_file(TspFileName);

  if (((PopInit == POP_TOU) || (PopInit == POP_ALL)) &&
     (InitTourLen != TownNum))
  { sprintf(Msg, "Input: tour length (%d) and town number (%d) not equal",
      InitTourLen, TownNum);
    critical_error(ERR_TOUR_TOWN, Msg);
  }

  calc_all_lines();

  OrderLen = TownNum;

  if (QueryFlag)
  { check_consistency(FALSE);
    strcpy(Suffix, crt_file_name());
    if (MyProcID == DISP_PROC)
    { printf("%s\n", Suffix);
    }
    exit(ERR_QUERY);
  }

  init_population();

  if (! strlen(Suffix))
  { strcpy(Suffix, crt_file_name());
  }
  init_gen_files(Suffix);
  P = 0;

  check_consistency(TRUE);

  create_pgm();

  alloc_storage();

  Seed = OrigSeed;

  if (TraceFlag && (MyProcID == DISP_PROC))
  { write_data(stdout);
  }

  if (OutFlag || inf)
  { if ((fp = file_open(InFile, "w", FALSE)) != NULL)
    { write_data(fp);
      fclose(fp);
    }
    else
    { sprintf(Msg, "Input: can't open '%s'", InFile);
      critical_error(ERR_FILE_OPEN, Msg);
    }
  }
  
  if (OutFlag)
  { for (P = 0; P < MyPopNum; P++)
    { if ((fp = file_open(OutFile, "w", TRUE)) != NULL)
      { fclose(fp);
      }
      else
      { sprintf(Msg, "Input: can't open '%s'", get_fname(OutFile));
        critical_error(ERR_FILE_OPEN, Msg);
      }
    }
    P = 0;
  }

  trace("input() completed");
}


/*****************************************************************************/
/* Check consistency of parameters                                           */
/*****************************************************************************/
void check_consistency(warn)
  BOOLEAN warn;		/* warning flag */
{ FILE *fp;
  BOOLEAN mutok = TRUE;

  trace("check_consistency() entered");

  warn &= OutFlag && (MyProcID == DISP_PROC);

  if ((PopSize < MIN_PopSize) || (PopSize > MAX_PopSize))
  { sprintf(Msg, "ChkCons: Population size (%d) invalid", PopSize);
    critical_error(ERR_POPSIZE, Msg);
  }

  switch (PopInit)
  { case POP_RND:
    case POP_TOU:
    case POP_NEA:
    case POP_CHE:
    case POP_FAR:
    case POP_ALT:
    case POP_ALL:
      break;
    default:
      sprintf(Msg, "ChkCons: Population Init scheme (%c) unknown", PopInit);
      critical_error(ERR_POPINIT, Msg);
      break;
  }

  if ((GapSize < 0.0) || (GapSize > 1.0))
  { sprintf(Msg, "ChkCons: Gap size (%e) invalid", GapSize);
    critical_error(ERR_GAPSIZE, Msg);
  }

  switch (BreederSelect)
  { case BRS_INV:
    case BRS_GAU:
      break;
    case BRS_RND:
    case BRS_PRO:
    case BRS_WHI:
    case BRS_LIR:
    case BRS_ILR:
    case BRS_BOL:
      if (LowerIndex < 1)
      { sprintf(Msg, "ChkCons: Lower selection index (%d) invalid",
          LowerIndex);
        critical_error(ERR_LOWERINDEX, Msg);
      }

      if (UpperIndex < 1)
      { sprintf(Msg, "ChkCons: Upper selection index (%d) invalid",
          UpperIndex);
        critical_error(ERR_UPPERINDEX, Msg);
      }

      if (LowerIndex > UpperIndex)
      { sprintf(Msg, "ChkCons: Lower (%d) > Upper (%d) selection index",
          LowerIndex, UpperIndex);
        critical_error(ERR_GRINDEX, Msg);
      }

      if (LowerIndex > PopSize)
      { sprintf(Msg, "ChkCons: Warning - 'Lower index' correction (%d->%d)",
          LowerIndex, PopSize);
        if (warn)
        { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
          { fprintf(fp, "%s\n", Msg);
            fclose(fp);
          }
          else
          { fprintf(stderr, "%s\n", Msg);
          }
        }
        LowerIndex = PopSize;
      }

      if (UpperIndex > PopSize)
      { sprintf(Msg, "ChkCons: Warning - 'Upper index' correction (%d->%d)",
          UpperIndex, PopSize);
        if (warn)
        { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
          { fprintf(fp, "%s\n", Msg);
            fclose(fp);
          }
          else
          { fprintf(stderr, "%s\n", Msg);
          }
        }
        UpperIndex = PopSize;
      }
      break;
    default:
      sprintf(Msg, "ChkCons: Breeder selection scheme (%c) unknown",
        BreederSelect);
      critical_error(ERR_BRSELECT, Msg);
      break;
  }

  switch (BreederSelect)
  { case BRS_WHI:
      if (fabs(EtaMax - 1.0) < EPS)
      { sprintf(Msg, "ChkCons: Whitley's constant (%e) invalid", EtaMax);
        critical_error(ERR_ETAMAX, Msg);
      }
      if (UpperIndex != PopSize)
      { sprintf(Msg, "ChkCons: Upper index (%d) and popsize (%d) not equal",
          UpperIndex, PopSize);
        critical_error(ERR_WHITLEY, Msg);
      }
      if (LowerIndex != 1)
      { sprintf(Msg, "ChkCons: Lower index (%d) must be equal 1", LowerIndex);
        critical_error(ERR_WHITLEY, Msg);
      }
      break;
    case BRS_LIR:
    case BRS_ILR:
      if ((EtaMax < MIN_EtaMax) || (EtaMax > MAX_EtaMax))
      { sprintf(Msg, "ChkCons: Maximum expected value (%e) invalid", EtaMax);
        critical_error(ERR_ETAMAX, Msg);
      }
      break;
    case BRS_BOL:
      if ((CtrlParam < MIN_CtrlParam) || (CtrlParam > MAX_CtrlParam))
      { sprintf(Msg, "ChkCons: Temperature control parameter (%e) invalid",
          CtrlParam);
        critical_error(ERR_CTRLPARAM, Msg);
      }
      if (ChnLen < 1)
      { sprintf(Msg, "ChkCons: Temperature cooling interval (%d) invalid",
          ChnLen);
        critical_error(ERR_CHNLEN, Msg);
      }
      break;
    default:
      break;
  }

  if (WindowSize < 0)
  { sprintf(Msg, "ChkCons: Scaling window size (%d) invalid", WindowSize);
    critical_error(ERR_WINDOWSIZE, Msg);
  }

  switch (MateSelect)
  { case MAS_RND:
    case MAS_POS:
      break;
    default:
      sprintf(Msg, "ChkCons: Mates selection scheme (%c) unknown",
        MateSelect);
      critical_error(ERR_MASELECT, Msg);
      break;
  }

  switch (Crossover)
  { case CRO_NOP:
    case CRO_EDG:
    case CRO_PMX:
    case CRO_OX:
    case CRO_CX:
    case CRO_UNI:
    case CRO_NEA:
    case CRO_CHE:
    case CRO_FAR:
      break;
    case CRO_INT:
    case CRO_RND:
      if ((CrossInt < 1) || (CrossInt > OrderLen))
      { sprintf(Msg, "ChkCons: Crossover intervals (%d) invalid", CrossInt);
        critical_error(ERR_CROSSINT, Msg);
      }
      break;
    default:
      sprintf(Msg, "ChkCons: Crossover scheme (%c) unknown", Crossover);
      critical_error(ERR_CROSSOVER, Msg);
      break;
  }

  if ((CrossRate < 0.0) || (CrossRate > 1.0))
  { sprintf(Msg, "ChkCons: Crossover rate (%e) invalid", CrossRate);
    critical_error(ERR_CROSSRATE, Msg);
  }

  switch (OptLocal)
  { case OPL_NOP:
    case OPL_LO2:
    case OPL_OP2:
    case OPL_2QU:
    case OPL_ORA:
    case OPL_1OR:
    case OPL_2OR:
    case OPL_3OR:
      break;
    default:
      sprintf(Msg, "ChkCons: Local optimization scheme (%c) unknown",
        OptLocal);
      critical_error(ERR_OPTLOCAL, Msg);
      break;
  }

  switch (Mutation[0])
  { case MUT_NOP:
      if (strlen(Mutation) > 1)
      { mutok = FALSE;
      }
      break;
    case MUT_CON:
    case MUT_ADA:
      if (strlen(Mutation) != 2)
      { mutok = FALSE;
      }
      else
      { switch (Mutation[1])
        { case MUT_SWP:
          case MUT_MOV:
          case MUT_INV:
          case MUT_RND:
            break;
          case MUT_OPL:
          case MUT_ALL:
            AllLocalOpt = FALSE;
            break;
          default:
            mutok = FALSE;
            break;
        }
      }
      break;
    default:
      mutok = FALSE;
      break;
  }
  if (! mutok)
  { sprintf(Msg, "ChkCons: Mutation scheme (%s) unknown", Mutation);
    critical_error(ERR_MUTATION, Msg);
  }

  if ((MutRate < 0.0) || (MutRate > 1.0))
  { sprintf(Msg, "ChkCons: Mutation rate (%e) invalid", MutRate);
    critical_error(ERR_MUTRATE, Msg);
  }

  if (MutDim < 1.0)
  { sprintf(Msg, "ChkCons: Mutation step dimension (%e) invalid", MutDim);
    critical_error(ERR_MUTDIM, Msg);
  }

  if (MutDim > OrderLen)
  { sprintf(Msg, "ChkCons: Warning - 'Mutation step' correction (%ld->%d)",
      (long) MutDim, OrderLen);
    if (warn)
    { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
      { fprintf(fp, "%s\n", Msg);
        fclose(fp);
      }
      else
      { fprintf(stderr, "%s\n", Msg);
      }
    }
    MutDim = (double) OrderLen;
  }

  switch(Replace)
  { case REP_RND:
      break;
    case REP_CRW:
      if (CrowFactor < 1)
      { sprintf(Msg, "ChkCons: Crowding factor (%d) invalid", CrowFactor);
        critical_error(ERR_CROWFACTOR, Msg);
      }
      if (CrowFactor > PopSize)
      { sprintf(Msg, "ChkCons: Warning - 'Crowding factor' correction (%d->%d)",
          CrowFactor, PopSize);
        if (warn)
        { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
          { fprintf(fp, "%s\n", Msg);
            fclose(fp);
          }
          else
          { fprintf(stderr, "%s\n", Msg);
          }
        }
        CrowFactor = PopSize;
      }
      break;
    default:
      sprintf(Msg, "ChkCons: Replace scheme (%c) unknown", Replace);
      critical_error(ERR_REPLACE, Msg);
      break;
  }

  switch (Elite)
  { case ELI_NOP:
      break;
    case ELI_NOR:
    case ELI_MUT:
      switch (EliteSelect)
      { case ELS_RND:
        case ELS_WEA:
        case ELS_FIW:
          break;
        default:
          sprintf(Msg, "ChkCons: Elite selection scheme (%c) unknown",
            EliteSelect);
          critical_error(ERR_ELSELECT, Msg);
          break;
      }
      if (EliteHold < 1)
      { sprintf(Msg, "ChkCons: Elite number (%d) invalid", EliteHold);
        critical_error(ERR_ELITEHOLD, Msg);
      }
      if (EliteHold > PopSize)
      { sprintf(Msg, "ChkCons: Warning - 'Elite number' correction (%d->%d)",
          EliteHold, PopSize);
        if (warn)
        { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
          { fprintf(fp, "%s\n", Msg);
            fclose(fp);
          }
          else
          { fprintf(stderr, "%s\n", Msg);
          }
        }
        EliteHold = PopSize;
      }
      break;
    default:
      sprintf(Msg, "ChkCons: Elite mechanism (%c) unknown", Elite);
      critical_error(ERR_ELITE, Msg);
      break;
  }

  switch (FitnessScale)
  { case FIT_NOP:
      break;
    case FIT_LIN:
    case FIT_EXP:
      if (FitParB == 0)
      { sprintf(Msg, "ChkCons: Fitness parameter B (%d) invalid", FitParB);
        critical_error(ERR_FITB, Msg);
      }
      if (FitParD == 0)
      { sprintf(Msg, "ChkCons: Fitness parameter D (%d) invalid", FitParD);
        critical_error(ERR_FITD, Msg);
      }
      break;
    default:
      sprintf(Msg, "ChkCons: Fitness scaling mechanism (%c) unknown",
        FitnessScale);
      critical_error(ERR_FITSCALE, Msg);
      break;
  }

  if (Normalize)
  { if (NormNum < 1)
    { sprintf(Msg, "ChkCons: Normalization number (%d) invalid", NormNum);
      critical_error(ERR_NORMNUM, Msg);
    }
    if (NormNum > TownNum)
    { sprintf(Msg, "ChkCons: Warning - 'Normalize number' correction (%d->1)",
        NormNum);
      if (warn)
      { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
        { fprintf(fp, "%s\n", Msg);
          fclose(fp);
        }
        else
        { fprintf(stderr, "%s\n", Msg);
        }
      }
      NormNum = 1;
    }
  } 

  if (ProcNum < 1)
  { sprintf(Msg, "ChkCons: Number of processors (%d) invalid", ProcNum);
    critical_error(ERR_PROCNUM, Msg);
  }

  if (ProcNum != NProcsOrig)
  { sprintf(Msg, "ChkCons: Warning - 'Processor number' correction (%d->%d)",
      NProcsOrig, ProcNum);
    if (warn)
    { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
      { fprintf(fp, "%s\n", Msg);
        fclose(fp);
      }
      else
      { fprintf(stderr, "%s\n", Msg);
      }
    }
  }

  if ((PopNum < 1) || (PopNum > MAX_PopNum))
  { sprintf(Msg, "ChkCons: Number of populations (%d) invalid", PopNum);
    critical_error(ERR_POPNUM, Msg);
  }

  if (PopNum > 1)
  { switch (ParModel)
    { case PAR_ISL:
      case PAR_TOK:
      case PAR_POL:
      case PAR_NEI:
        break;
      default:
        sprintf(Msg, "ChkCons: Parallel model (%c) unknown", ParModel);
        critical_error(ERR_PARMODEL, Msg);
        break;
    }

    if (ParModel == PAR_ISL)
    { CommInt = 0;
    }
    else
    { if (CommInt < 0)
      { sprintf(Msg, "ChkCons: Communication interval (%d) invalid", CommInt);
        critical_error(ERR_COMMINT, Msg);
      }
      if (IndNum < 1)
      { sprintf(Msg, "ChkCons: Number of individuals to change (%d) invalid",
          IndNum);
        critical_error(ERR_INDNUM, Msg);
      }
      if (IndNum > PopSize)
      { sprintf(Msg, "ChkCons: Warning - 'Ind. Number' correction (%d->%d)",
          IndNum, PopSize);
        if (warn)
        { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
          { fprintf(fp, "%s\n", Msg);
            fclose(fp);
          }
          else
          { fprintf(stderr, "%s\n", Msg);
          }
        }
        IndNum = PopSize;
      }
    }

    switch (ParModel)
    { case PAR_ISL:
      case PAR_TOK:
        break;
      case PAR_POL:
        if ((PollenDirection < 0) || (PollenDirection > 359))
        { sprintf(Msg, "ChkCons: Spread direction of pollen (%d) invalid",
            PollenDirection);
          critical_error(ERR_POLLENDIR, Msg);
        }
        if ((LowerWindForce < 0) || (LowerWindForce > MAX_WINDFORCE))
        { sprintf(Msg, "ChkCons: Lower limit to wind force (%d) invalid",
            LowerWindForce);
          critical_error(ERR_LOWERWINDFORCE, Msg);
        }
        if ((UpperWindForce < 0) || (UpperWindForce > MAX_WINDFORCE))
        { sprintf(Msg, "ChkCons: Upper limit to wind force (%d) invalid",
            UpperWindForce);
          critical_error(ERR_UPPERWINDFORCE, Msg);
        }
        if (LowerWindForce > UpperWindForce)
        { sprintf(Msg, "ChkCons: Lower (%d) > Upper (%d) limit to wind force",
            LowerWindForce, UpperWindForce);
          critical_error(ERR_GRWINDFORCE, Msg);
        }
        break;
      case PAR_NEI:
        switch (Topology)
        { case TOP_RIN:
          case TOP_PIP:
          case TOP_GRI:
          case TOP_TOR:
          case TOP_HYP:
          case TOP_TRE:
            break;
          default:
            sprintf(Msg, "ChkCons: Topology type (%c) unknown", Topology);
            critical_error(ERR_TOPOLOGY, Msg);
            break;
        }
        break;
      default:
        break;
    }
  }

  if ((TotalExperiments < 1) || (TotalExperiments > MAX_TotalExperiments))
  { sprintf(Msg, "ChkCons: Total number of experiments (%d) invalid", 
      TotalExperiments);
    critical_error(ERR_TOTEXP, Msg);
  }

  if (TotalTrials == 0)
  { if ((TotalGenerations < 1) || (TotalGenerations > MAX_TotalGenerations))
    { sprintf(Msg, "ChkCons: Total number of generations (%d) invalid", 
        TotalGenerations);
      critical_error(ERR_TOTGEN, Msg);
    }
  }
  else
  { if ((TotalTrials < 1) || (TotalTrials > MAX_TotalTrials))
    { sprintf(Msg, "ChkCons: Total number of trials (%d) invalid", 
        TotalTrials);
      critical_error(ERR_TOTTRIALS, Msg);
    }
  }

  if ((MinQuality < MIN_MinQuality) || (MinQuality > MAX_MinQuality))
  { sprintf(Msg, "ChkCons: Minimum of tour length (%e) invalid", MinQuality);
    critical_error(ERR_MINQUAL, Msg);
  }

  if (Interval < 0)
  { sprintf(Msg, "ChkCons: Report interval (%d) invalid", Interval);
    critical_error(ERR_INTERVAL, Msg);
  }

  if (SaveSize < 0)
  { sprintf(Msg, "ChkCons: Save size (%d) invalid", SaveSize);
    critical_error(ERR_SAVESIZE, Msg);
  }
  if (SaveSize > PopSize)
  { sprintf(Msg, "ChkCons: Warning - 'Save size' correction (%d->%d)",
      SaveSize, PopSize);
    if (warn)
    { if ((fp = file_open(AllLogFile, "a", FALSE)) != NULL)
      { fprintf(fp, "%s\n", Msg);
        fclose(fp);
      }
      else
      { fprintf(stderr, "%s\n", Msg);
      }
    }
    SaveSize = PopSize;
  }

  if (MaxSpin < 0)
  { sprintf(Msg, "ChkCons: Maxspin (%d) invalid", MaxSpin);
    critical_error(ERR_MAXSPIN, Msg);
  }

  if (PgmFreq < 0)
  { sprintf(Msg, "ChkCons: Bitmap frequency (%d) invalid", PgmFreq);
    critical_error(ERR_PGMFREQ, Msg);
  }

  if (DumpFreq < 0)
  { sprintf(Msg, "ChkCons: Dump frequency (%d) invalid", DumpFreq);
    critical_error(ERR_DUMPFREQ, Msg);
  }

  if (DisplayFlag)
  { if ((PopDisplay < 1) || (PopDisplay > PopNum))
    { sprintf(Msg, "ChkCons: Population no. to show on display (%d) invalid",
	PopDisplay);
      critical_error(ERR_POPDISPLAY, Msg);
    }
  }

  switch (RandomType)
  { case RND_MAR:
    case RND_PRG:
      break;
    default:
      sprintf(Msg, "ChkCons: Random number generator type (%c) unknown",
        RandomType);
      critical_error(ERR_RNDTYPE, Msg);
      break;
  }

  trace("check_consistency() completed");
}


/*****************************************************************************/
/* Allocate global storage                                                   */
/*****************************************************************************/
void alloc_storage()
{ register int i;

  trace("alloc_storage() entered");

  OldPop = (POPULATION **) emalloc((unsigned long) MyPopNum *
    sizeof(POPULATION *), TRUE);
  NewPop = (POPULATION **) emalloc((unsigned long) MyPopNum *
    sizeof(POPULATION *), TRUE);
  BestSet = (BESTCHROM **) emalloc((unsigned long) MyPopNum *
    sizeof(BESTCHROM *), TRUE);
  Generation = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  Trials = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  DoneFlag = (BOOLEAN *) emalloc((unsigned long) MyPopNum * sizeof(BOOLEAN),
    TRUE);
  Online = (double *) emalloc((unsigned long) MyPopNum * sizeof(double), TRUE);
  Offline = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  Best = (double *) emalloc((unsigned long) MyPopNum * sizeof(double), TRUE);
  TotalOnline = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  TotalOffline = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  TotalBest = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  Spin = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  CurrDump = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  BestSize = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  Lost = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  Conv = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  Plateau = (int *) emalloc((unsigned long) MyPopNum * sizeof(int), TRUE);
  OnSum = (double *) emalloc((unsigned long) MyPopNum * sizeof(double), TRUE);
  OffSum = (double *) emalloc((unsigned long) MyPopNum * sizeof(double), TRUE);
  Worst = (double *) emalloc((unsigned long) MyPopNum * sizeof(double), TRUE);
  BestCurrPerf = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  AvgCurrPerf = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  WorstCurrPerf = (double *) emalloc((unsigned long) MyPopNum *
    sizeof(double), TRUE);
  AvgBestPerf = (double *) emalloc((unsigned long) MyPopNum * sizeof(double),
    TRUE);
  Win = (double **) emalloc((unsigned long) MyPopNum * sizeof(double *),
    TRUE);
  ValBuf = (BUFARRAY *) emalloc((unsigned long) MyPopNum * sizeof(BUFARRAY),
    TRUE);
  PfmBuf = (BUFFER **) emalloc((unsigned long) MyPopNum * sizeof(BUFFER *),
    TRUE);

  for (P = 0; P < MyPopNum; P++)
  { OldPop[P] = alloc_pop(PopSize);
    NewPop[P] = alloc_pop(PopSize);

    if (WindowSize)
    { Win[P] = (double *) emalloc((unsigned long) WindowSize * sizeof(double),
        TRUE);
    }

    if (SaveSize)
    { BestSet[P] = alloc_best(SaveSize);
    }

    for (i = 0; i < BUFCNT; i++)
    { ValBuf[P][i] = create_buffer(ValFile[i], OrderLen);
      change_fmt(ValBuf[P][i], 0, "%5.0f ");
    }
    PfmBuf[P] = create_buffer(OutFile, DATACOL);
  }
  P = 0;

  trace("alloc_storage() completed");
}


/*****************************************************************************/
/* Free global storage                                                       */
/*****************************************************************************/
void free_storage()
{ register int i;

  trace("free_storage() entered");

  for (P = 0; P < MyPopNum; P++)
  { delete_buffer(PfmBuf[P]);
    for (i = 0; i < BUFCNT; i++)
    { delete_buffer(ValBuf[P][i]);
    }
    free_best(BestSet[P]);
    free(Win[P]);
    free_pop(NewPop[P]);
    free_pop(OldPop[P]);
  }
  P = 0;

  free(PfmBuf);
  free(ValBuf);
  free(Win);
  free(AvgBestPerf);
  free(WorstCurrPerf);
  free(AvgCurrPerf);
  free(BestCurrPerf);
  free(Worst);
  free(OffSum);
  free(OnSum);
  free(Plateau);
  free(Conv);
  free(Lost);
  free(BestSize);
  free(CurrDump);
  free(Spin);
  free(TotalBest);
  free(TotalOffline);
  free(TotalOnline);
  free(Best);
  free(Offline);
  free(Online);
  free(DoneFlag);
  free(Trials);
  free(Generation);
  free(BestSet);
  free(NewPop);
  free(OldPop);

  trace("free_storage() completed");
}


/*** end of file ***/
