/* declare.h   9-9-92  Artificial Life simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * sccsid: @(#)declare.h	1.37	12/8/91
 */

#include "license.h"

#ifndef  LDECLAR_H
#define  LDECLAR_H

FILE *oufr;
HpInst soup;
I32s BitBucket; /* place to dump return values */
I32s AverageSize;	/* average size of cells in soup */
Pcells BottomReap; /* cell at bottom of reaper queue, last to die */
Pcells BottomDummy; /* dummy cell at bottom of reaper queue */
I32s BrkupCou; /* count of output files break.n */
I32s BrkupCum; /* cumulative count of bytes output to break.n */
I8s  Buff[120]; /* nice global text buff for all sorts of messages */
I32s pos; /* file pointer for position in break.X files */
I32s CelArSiz; /* size of each cells array */
I32s CellsSize;  /* total size of cells arrays of structures */
I32s CountFlaw;/* counter for flaw random number */
I32s CountMovMut;	/* keep track of time since last mov_mut */
I32s CountMutRate;	/* keep track of time since last mut */
I32s debug_switch;
Event DistNext;	/* time of next disturbance */
Event Disturb;	/* time of disturbance */
CellInd extr;     /* which cell to isolate */
I8s ExtrG[20];	/* last geno extracted */
I32s ExtractCount;	/* count of cells manually extracted */
I32s FirstOutDisk;	/* has OutDisk been called */
I32s fe_lines = 20;        /* approx hight in chars of screen, default 20 */
I32s fe_width = 60;        /* approx width in chars of screen, default 60 */
I32s FreeBlocks;	/* number of free blocks of memory */
double Generations;/*count of elapsed generations * (AvgPop/TimeBirth-Death)*/
I8s **GenInList;	/* pointers to soup_in genome names */
HistType *Hist = NULL;/* pointer to struct for histograms */
I32s HistSize = 0; /* Num of elements alloced in Hist */
float HistNStars = 0.0; /* ratio of counts to stars */
I8s *GenInBuf; /* buffer containing soup_in genome names */
I32s GFormat = -1; /* select genebank format style, -1 = first file = format */
indx_t  GIndx;     /* global index structure */
I8s  GoDown = 0;   /* flag to bring system down to defragment memory */
I32s HistPrint = 0; /* boolean to tell us weather to print hists, to log */
I8s IMode;	/* info display (plan,histo_size,histo_geno etc) */
I32s InstD[33]; /* data structure for freq dist. of instructions */
Event InstExe;	/* counter of instructions executed */
PInst is;/* structure for passing info between parse and execute */
I32s isolate;  /* isolate the genome of the cell extr */
Event LastDiv;	/* instructions executed at last divide */
I8s mes[10][80];/* array of strings for message passing to front end */
I32s MalLimit;  /* search limit for memory allocation */
I32s Max_hits;	/* cardinality of most populous size class */
I32s  Nop0 = 0;  /* instruct num corresponding to nop0 */
I32s  Nop1 = 1;  /* instruct num corresponding to nop1 */
I32s  NopS = 1;  /* Nop0 + Nop1 sum of Nops for template search */
I32s NumCelAr; /* number of cells arrays */
I32s NumGenDG; /* present # of permanent genotypes saved to disk */
I32s NumGenDM; /* present # of temporary genotypes swapped out to disk */
I32s NumGenRQ; /* present # of genotypes in RAM genequeue */
I32s NumGenotypes; /* # of genotypes of adult cells extant in soup */
I32s NumSizes; /* # of sizes of adult cells extant in soup */
I32s PhotonSize;	/* number of instructions in photon */
I32s RandIx1, RandIx2, RandIx3;	/* for trand() */
I32s RateFlaw; /* frequency of flaws */
I32s RateMovMut;	/* 1 / frequency of mutations per mov event */
I32s RateMut;  /* number of instructions per mutations */
I32s reaped;   /* 0 = reaper has not killed, 1 = reaper has killed */
I32s SigBlockSet; /* mask to block sig int in unix, for certain calls */
I32s siz_sl;   /* allocated size of *sl array */
I8s soup_fn[85];	/* place for soup_in filename */
I32s SoupBot;  /* index of FreeMem struct for bottom of soup memory */
I32s SoupTop;  /* index of FreeMem struct for top of soup memory */
I8s Swap=1;   /* DOS flag whether to gq_swap or not */
FILE *tfp_log = NULL; /* file pointer for log */
I8s  TC_Menu = 0;	/* flag unix sigint, to do a menu */
Pcells ThisSlice;/* index of cell that is currently active */
I32s TimeBirth;/* count of births in each million instruction */
I32s TimeDeath;/* count of deaths in each million instruction */
double TimePop;/* sum of ttime * NumCells for each million instructions */
Pcells TopDummy; /* dummy cell at top of reaper queue */
Pcells TopReap; /* index of cell at top of reaper queue, next to die */
I32s TotFlaw;  /* total number of flaws in this run */
I32s TotMemUse; /* total memory use by soup, cells, and genebank */
I32s TotMovMut;/* total number of move mutations in this run */
I32s TotMut;   /* total number of background mutations in this run */
Instruction PhotonInst[80];	/* instructional representation of photon */
I32s FreeMemCurrent;	/* current amount of free memory in soup */
I32s Search_limit;/* limit on how far address instructions will search */
I32s Put_limit; /* limit on intercellular communications distance */
Pcells Fp cells;  /* cells array */
Pmf FreeMem;   /* free memory array */
SList **sl;	/* list of unique size classes, number of gts */
double TrandArray[98];	/* for trand() */
LastOut lo;	/* last data output to disk */
GList *gq_bot;	/* bottom of gene queue */
GList *gq_top;	/* top of gene queue */
void (*slicer) ();

#ifdef ALCOMM
I16s           AL_run_flag; 
I16s           VPORT; 
#endif	/* ALCOMM */

I8u MSG_X = 1;
I8u MSG_Y = 1;
I8u ERR_X = 1;
I8u ERR_Y = 1;
I8u PLN_X = 1;
I8u PLN_Y = 1;
I8u HLP_X = 1;
I8u HLP_Y = 1;

#ifdef MICRO
I32s MC_step = -1L;
Pcells MicroSlice = 0;/* index of cell that is currently active */
#endif

#ifdef __TURBOC__
extern unsigned _stklen = 32767;
I16s FE_DV_Mode= 0;
#endif

#endif
