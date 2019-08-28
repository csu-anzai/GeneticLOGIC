/* $Id: interfac.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : interfac.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_INTERFAC_H
#define TSP_INTERFAC_H

#include <stdio.h>
#include "define.h"


#ifdef USE_PROTO
extern void init_values(void);
extern char *no_yes(BOOLEAN);
extern BOOLEAN is_yes(char *);
extern int write_data(FILE *);
extern int get_data(FILE *);
extern int io_TspFileName(FILE *, register int);
extern int io_PopSize(FILE *, register int);
extern int io_PopInit(FILE *, register int);
extern int io_TourFileName(FILE *, register int);
extern int io_GapSize(FILE *, register int);
extern int io_BreederSelect(FILE *, register int);
extern int io_Whitley(FILE *, register int);
extern int io_EtaMax(FILE *, register int);
extern int io_CtrlParam(FILE *, register int);
extern int io_ChnLen(FILE *, register int);
extern int io_LowerIndex(FILE *, register int);
extern int io_UpperIndex(FILE *, register int);
extern int io_WindowSize(FILE *, register int);
extern int io_MateSelect(FILE *, register int);
extern int io_Crossover(FILE *, register int);
extern int io_CrossRate(FILE *, register int);
extern int io_CrossTwoOff(FILE *, register int);
extern int io_CrossInt(FILE *, register int);
extern int io_OptLocal(FILE *, register int);
extern int io_Mutation(FILE *, register int);
extern int io_MutRate(FILE *, register int);
extern int io_MutDim(FILE *, register int);
extern int io_Replace(FILE *, register int);
extern int io_CrowFactor(FILE *, register int);
extern int io_Elite(FILE *, register int);
extern int io_EliteSelect(FILE *, register int);
extern int io_EliteHold(FILE *, register int);
extern int io_FitnessScale(FILE *, register int);
extern int io_FitParA(FILE *, register int);
extern int io_FitParB(FILE *, register int);
extern int io_FitParC(FILE *, register int);
extern int io_FitParD(FILE *, register int);
extern int io_FilterTwins(FILE *, register int);
extern int io_Normalize(FILE *, register int);
extern int io_NormNum(FILE *, register int);
extern int io_ProcNum(FILE *, register int);
extern int io_PopNum(FILE *, register int);
extern int io_ParModel(FILE *, register int);
extern int io_PollenDirection(FILE *, register int);
extern int io_LowerWindForce(FILE *, register int);
extern int io_UpperWindForce(FILE *, register int);
extern int io_Topology(FILE *, register int);
extern int io_IndNum(FILE *, register int);
extern int io_CommInt(FILE *, register int);
extern int io_TotalExperiments(FILE *, register int);
extern int io_TotalTrials(FILE *, register int);
extern int io_TotalGenerations(FILE *, register int);
extern int io_MinQuality(FILE *, register int);
extern int io_Interval(FILE *, register int);
extern int io_SaveSize(FILE *, register int);
extern int io_MaxSpin(FILE *, register int);
extern int io_PgmFreq(FILE *, register int);
extern int io_DumpFreq(FILE *, register int);
extern int io_Options(FILE *, register int);
extern int io_RandomType(FILE *, register int);
extern int io_OrigSeed(FILE *, register int);
extern int io_GraphicsFlag(FILE *, register int);
extern int io_DisplayFlag(FILE *, register int);
extern int io_PopDisplay(FILE *, register int);
extern int io_OutFlag(FILE *, register int);
extern int io_ReportFlag(FILE *, register int);
extern int io_Suffix(FILE *, register int);
extern char *get_file_name(char *);
extern char *crt_file_name(void);
#else
extern void init_values();
extern char *no_yes();
extern BOOLEAN is_yes();
extern int write_data();
extern int get_data();
extern int io_TspFileName();
extern int io_PopSize();
extern int io_PopInit();
extern int io_TourFileName();
extern int io_GapSize();
extern int io_BreederSelect();
extern int io_Whitley();
extern int io_EtaMax();
extern int io_CtrlParam();
extern int io_ChnLen();
extern int io_LowerIndex();
extern int io_UpperIndex();
extern int io_WindowSize();
extern int io_MateSelect();
extern int io_Crossover();
extern int io_CrossRate();
extern int io_CrossTwoOff();
extern int io_CrossInt();
extern int io_OptLocal();
extern int io_Mutation();
extern int io_MutRate();
extern int io_MutDim();
extern int io_Replace();
extern int io_CrowFactor();
extern int io_Elite();
extern int io_EliteSelect();
extern int io_EliteHold();
extern int io_FitnessScale();
extern int io_FitParA();
extern int io_FitParB();
extern int io_FitParC();
extern int io_FitParD();
extern int io_FilterTwins();
extern int io_Normalize();
extern int io_NormNum();
extern int io_ProcNum();
extern int io_PopNum();
extern int io_ParModel();
extern int io_PollenDirection();
extern int io_LowerWindForce();
extern int io_UpperWindForce();
extern int io_Topology();
extern int io_IndNum();
extern int io_CommInt();
extern int io_TotalExperiments();
extern int io_TotalTrials();
extern int io_TotalGenerations();
extern int io_MinQuality();
extern int io_Interval();
extern int io_SaveSize();
extern int io_MaxSpin();
extern int io_PgmFreq();
extern int io_DumpFreq();
extern int io_Options();
extern int io_RandomType();
extern int io_OrigSeed();
extern int io_GraphicsFlag();
extern int io_DisplayFlag();
extern int io_PopDisplay();
extern int io_OutFlag();
extern int io_ReportFlag();
extern int io_Suffix();
extern char *get_file_name();
extern char *crt_file_name();
#endif


#endif


/*** end of file ***/
