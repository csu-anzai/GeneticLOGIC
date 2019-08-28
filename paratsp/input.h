/* $Id: input.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : input.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_INPUT_H
#define TSP_INPUT_H

#include "define.h"


/* ParaTSP options */

#define OPT_STR		"A:B:C:D:E:F:G:H:I:L:M:N:O:P:Q:R:S:T:W:X:Y:Z#\
a:b:cde:f:g:hi:km:n:o:p#qr:s:t:u:v:wx:y:?"

#define OPT_TspFileName		'A'
#define OPT_BreederSelect	'B'
#define OPT_CrossRate		'C'
#define OPT_MutDim		'D'
#define OPT_Elite		'E'
#define OPT_FitnessScale	'F'
#define OPT_GapSize		'G'
#define OPT_EliteHold		'H'
#define OPT_PopInit		'I'
#define OPT_EliteSelect		'L'
#define OPT_Mutation		'M'
#define OPT_NormNum		'N'
#define OPT_GA_Options		'O'
#define OPT_CrossTwoOff			'C'
#define OPT_Normalize			'N'
#define OPT_FilterTwins			'T'
#define OPT_PopSize		'P'
#define OPT_CrowFactor		'Q'
#define OPT_Replace		'R'
#define OPT_MateSelect		'S'
#define OPT_OptLocal		'T'
#define OPT_WindowSize		'W'
#define OPT_Crossover		'X'
#define OPT_MutRate		'Y'
#define OPT_Other		'Z'
#define OPT_FitParA			'A'
#define OPT_FitParB			'B'
#define OPT_FitParC			'C'
#define OPT_FitParD			'D'
#define OPT_ChnLen			'I'
#define OPT_LowerIndex			'L'
#define OPT_EtaMax			'N'
#define OPT_CtrlParam			'T'
#define OPT_UpperIndex			'U'
#define OPT_CrossInt			'X'
#define OPT_TourFileName	'a'
#define OPT_PgmFreq		'b'
#define OPT_GraphicsFlag	'c'
#define OPT_DisplayFlag		'd'
#define OPT_TotalExperiments	'e'
#define OPT_DumpFreq		'f'
#define OPT_TotalGenerations	'g'
#define OPT_InFile		'i'
#define OPT_OutFlag		'k'
#define OPT_MinQuality		'm'
#define OPT_SaveSize		'n'
#define OPT_Options		'o'
#define OPT_AllFlag			'a'
#define OPT_LastFlag			'l'
#define OPT_NoTermFlag			'n'
#define OPT_PrintPopFlag		'p'
#define OPT_SchemaFlag			's'
#define OPT_TraceFlag			't'
#define OPT_VarFlag			'v'
#define OPT_Parallel		'p'
#define	OPT_CommInt			'i'
#define	OPT_ParModel			'm'
#define	OPT_IndNum			'n'
#define	OPT_PopNum			'p'
#define OPT_ProcNum			'r'
#define OPT_PollenDirection		's'
#define	OPT_Topology			't'
#define OPT_LowerWindForce		'x'
#define OPT_UpperWindForce		'y'
#define OPT_QueryFlag		'q'
#define OPT_RandomType		'r'
#define OPT_OrigSeed		's'
#define OPT_TotalTrials		't'
#define OPT_PopDisplay		'u'
#define OPT_Interval		'v'
#define OPT_ReportFlag		'w'
#define OPT_Suffix		'x'
#define OPT_MaxSpin		'y'

#define USAGE		"ParaTSP 1.0\t\t\t(c) 1994 by Holger Totzke\n\n\
Usage: paratsp [Options]\n\
GA-specific options:\n\
\t-A  tspfi\t\t# TSP input file\n\
\t-B  I,G,R,P,L,N,W,B\t# breeder selection scheme\n\
\t    I    \t\t# inverse roulette wheel\n\
\t    G    \t\t# gauss\n\
\t    R    \t\t# random\n\
\t    P    \t\t# proportional\n\
\t    L    \t\t# linear ranking\n\
\t    N    \t\t# inverse linear ranking\n\
\t    W    \t\t# Whitley's index calculation\n\
\t    B    \t\t# Boltzmann\n\
\t-C  xprob\t\t# crossover probability\n\
\t-D  mudim\t\t# mutation step dimension\n\
\t-E  N,M,_\t\t# elitist scheme\n\
\t    N    \t\t# normal\n\
\t    M    \t\t# mutate\n\
\t    _    \t\t# no elitism\n\
\t-F  L,E,_\t\t# fitness scaling type\n\
\t    L    \t\t# linear\n\
\t    E    \t\t# exponential\n\
\t    _    \t\t# no fitness scaling\n\
\t-G  gapsz\t\t# generation gap size\n\
\t-H  elhld\t\t# elite number (hold)\n\
\t-I  R,T,N,C,F,W,A\t# population initialization scheme\n\
\t    R    \t\t# random\n\
\t    T    \t\t# tour from input file\n\
\t    N    \t\t# nearest neighbour\n\
\t    C    \t\t# cheapest insertion\n\
\t    F    \t\t# farthest insertion\n\
\t    W    \t\t# random scheme without input (R,N,C,F)\n\
\t    A    \t\t# random scheme (R,T,N,C,F)\n\
\t-L  R,W,F\t\t# elite selection scheme (sacrifice select)\n\
\t    R    \t\t# random\n\
\t    W    \t\t# weakest\n\
\t    F    \t\t# first weaker\n\
\t-M  xy   \t\t# mutation scheme\n\
\t    x={C,A,_}\t\t# possible settings:\n\
\t    y={M,S,I,R,T,L}\t#\n\
\t    C    \t\t# constant mutation rates\n\
\t    A    \t\t# adapted mutation rates\n\
\t    _    \t\t# no mutation\n\
\t    M    \t\t# move\n\
\t    S    \t\t# swap\n\
\t    I    \t\t# invert\n\
\t    R    \t\t# random scheme without local opt. (M,S,I)\n\
\t    T    \t\t# local tour optimization as mutation\n\
\t    L    \t\t# random scheme with local opt. (M,S,I,T)\n\
\t-N  norno\t\t# normalization number (town index)\n\
\t-O  C,N,T\t\t# other GA-specific options\n\
\t    C    \t\t# crossover creates two childs\n\
\t    N    \t\t# normalization of tour\n\
\t    T    \t\t# filter twins\n\
\t-P  popsz\t\t# population size\n\
\t-Q  crowf\t\t# crowding factor\n\
\t-R  R,C  \t\t# replace scheme (dispersal)\n\
\t    R    \t\t# random\n\
\t    C    \t\t# crowding\n\
\t-S  R,P  \t\t# mates selection mechanism\n\
\t    R    \t\t# random\n\
\t    P    \t\t# positional index\n\
\t-T  L,T,Q,A,1,2,3,_\t# local tour optimization scheme\n\
\t    L    \t\t# Lin's 2-opt\n\
\t    T    \t\t# 2-opt\n\
\t    Q    \t\t# 2-quick\n\
\t    A    \t\t# sequence of 2-opt and or-opt\n\
\t    1    \t\t# or-opt(1)\n\
\t    2    \t\t# or-opt(2)\n\
\t    3    \t\t# or-opt(3)\n\
\t    _    \t\t# no local tour optimization\n\
\t-W  wdwsz\t\t# window size for scaling\n\
\t-X  E,I,P,O,C,U,N,H,F,\t# crossover scheme\n\
\t    R,_  \t\t#\n\
\t    E    \t\t# edge recombination (ERX)\n\
\t    I    \t\t# interval partially matched (IPMX)\n\
\t    P    \t\t# partially matched (PMX)\n\
\t    O    \t\t# order (OX)\n\
\t    C    \t\t# cycle (CX)\n\
\t    U    \t\t# uniform order based (UOBX)\n\
\t    N    \t\t# nearest neighbour\n\
\t    H    \t\t# cheapest insertion\n\
\t    F    \t\t# farthest insertion\n\
\t    R    \t\t# random (E,P,O,C,U,N,H,F)\n\
\t    _    \t\t# no crossover\n\
\t-Y  mprob\t\t# mutation probability\n\
\t-ZA fitpa\t\t# fitness scaling parameter A\n\
\t-ZB fitpb\t\t# fitness scaling parameter B\n\
\t-ZC fitpc\t\t# fitness scaling parameter C\n\
\t-ZD fitpd\t\t# fitness scaling parameter D\n\
\t-ZI chlen\t\t# cooling generations for boltzmann selection\n\
\t-ZL lwidx\t\t# lower index for breeder selection\n\
\t-ZN etamx\t\t# maximum expected value for linear ranking\n\
\t-ZT tmpct\t\t# temperature control for boltzmann selection\n\
\t-ZU upidx\t\t# upper index for breeder selection\n\
\t-ZX xpoin\t\t# number of crossover intervals in IPMX\n\
\n\
Parallel options:\n\
\t-pi comin\t\t# communication interval in generations\n\
\t-pm i,t,p,n\t\t# parallel model\n\
\t    i    \t\t# island\n\
\t    t    \t\t# token\n\
\t    p    \t\t# pollen\n\
\t    n    \t\t# neighbour\n\
\t-pn indno\t\t# number of individuals to change\n\
\t-pp popno\t\t# number of populations\n\
\t-pr nproc\t\t# number of processors\n\
\t-ps spdir\t\t# spread direction of pollen\n\
\t-pt r,p,g,o,h,t\t\t# type of communication topology\n\
\t    r    \t\t# ring\n\
\t    p    \t\t# pipe\n\
\t    g    \t\t# 2d-grid\n\
\t    o    \t\t# 2d-torus\n\
\t    h    \t\t# hypercube\n\
\t    t    \t\t# tree\n\
\t-px lwwin\t\t# lower limit to wind force\n\
\t-py upwin\t\t# upper limit to wind force\n\
\n\
General options:\n\
\t-a  tourf\t\t# tour input file\n\
\t-b  tdfrq\t\t# tour dump interval in generations\n\
\t-c       \t\t# save tour for XTSP\n\
\t-d       \t\t# show results on display\n\
\t-e  noexp\t\t# total number of experiments\n\
\t-f  dfreq\t\t# dump interval in generations\n\
\t-g  nogen\t\t# total number of generations\n\
\t-h       \t\t# this help information\n\
\t-?       \t\t#\n\
\t-i  infil\t\t# input file\n\
\t-k       \t\t# no output files\n\
\t-m  minqu\t\t# minimal tour length (quality) to abort\n\
\t-n  savno\t\t# number of best individuals to save\n\
\t-o  a,l,n,p,s,t,v\t# additional general options\n\
\t-q       \t\t# create directory name and exit\n\
\t-r  m,p  \t\t# random number generator type\n\
\t    m    \t\t# Marsaglia's random number generator\n\
\t    p    \t\t# rand() from programming language C\n\
\t-s  seed \t\t# seed for random number generator\n\
\t-t  notrl\t\t# total number of trials\n\
\t-u  popno\t\t# population no. to show on display\n\
\t-v  intvl\t\t# number of trials between data collections\n\
\t-w       \t\t# without report\n\
\t-x  suffx\t\t# file suffix\n\
\t-y  maxsp\t\t# number of generations wo. evaluations\n\
\n\
Additional options (following the general option -o):\n\
\t a       \t\t# evaluate all individuals\n\
\t l       \t\t# dump last generation\n\
\t n       \t\t# simplified termination\n\
\t p       \t\t# dump population\n\
\t s       \t\t# trace schema history\n\
\t t       \t\t# trace ParaTSP run\n\
\t v       \t\t# collect tour values (much data !)\n\
\n"


#ifdef USE_PROTO
extern void set_flag(char);
extern void input(int, char **);
extern void check_consistency(BOOLEAN);
extern void alloc_storage(void);
extern void free_storage(void);
#else
extern void set_flag();
extern void input();
extern void check_consistency();
extern void alloc_storage();
extern void free_storage();
#endif


#endif


/*** end of file ***/
