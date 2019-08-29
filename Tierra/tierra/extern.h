/* extern.h   9-9-92  Artificial Life simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * sccsid: @(#)extern.h	1.38	12/8/91
 */

#include "license.h"

#ifdef ALCOMM
#include <mlayer.h>
#endif /* ALCOMM */

#ifndef LEXTERN_H
#define LEXTERN_H

extern FILE *oufr;
extern HpInst soup;
extern I32s BitBucket; /* place to dump return values */
extern I32s alive;     /* run simulator alive * 1,000,000 instructions */
extern I32s AverageSize;	/* average size of cells in soup */
extern Pcells BottomDummy; /* dummy cell at bottom of reaper queue */
extern Pcells BottomReap;/* index of cell bottom reaper queue, last to die */
extern I32s BrkupCou;  /* count of output files break.n */
extern I32s BrkupCum;  /* cumulative count of bytes output to break.n */
extern I32s BrkupSiz;  /* size of output files break.n in K */
extern I8s  Buff[120]; /* nice global text buff for all sorts of messages */
extern I32s pos; /* file pointer for position in break.X files */
extern I32s CelArSiz; /* size of each cells array */
extern I32s CellsSize; /* number of allocated elements in cells array */
extern I32s CountFlaw; /* counter for flaw random number */
extern I32s CountMovMut;
extern I32s CountMutRate;
extern I32s CumGeneBnk;    /* Use cumulative gene files, or overwrite */
extern I32s debug;     /* output info to screen for debugging */
extern I32s debug_switch;
extern I32s DiskOut;   /* 0 = no output to disk, 1 = output to disk */
extern Event Disturb;	/* time of disturbance */
extern float DistFreq; /* freq. of disturbance, in multiples of recovery t */
extern Event DistNext;	/* time of next disturbance */
extern float DistProp; /* proportion of cells killed in each disturbance */
extern I32s DivSameSiz;/* 0 = off, 1 = daughter cell must be same size */
extern I32s DivSameGen;/* 0 = off, 1 = daught cell must be same genotype */
extern I32s DropDead;  /* millions instruction since last divide till dead */
extern CellInd extr;      /* which cell to isolate */
extern I8s ExtrG[20];   /* last geno extracted */
extern I32s ExtractCount;
extern I32s FirstOutDisk;
extern I32s fe_lines;  /* approx hight in chars of screen, default 20 */
extern I32s fe_width;  /* approx width in chars of screen, default 60 */
extern I32s FreeBlocks;/* number of free blocks of memory */
extern I32s GeneBnker; /* 0 = don't keep track genotyes, 1 = keep track */
extern double Generations; /* elapsed generations (AvgPop/TimeBirth-Death) */
extern I8s **GenInList;/* pointers to soup_in genome names */
extern I8s *GenInBuf;  /* buffer containing soup_in genome names */
extern float GenPerBkgMut;	/* generations per background mutation */
extern float GenPerFlaw;	/* generations per flaw */
extern float GenPerMovMut;	/* generations per move mutation */
extern I32s GFormat; /* select genebank format -1 = first file = format */
extern indx_t  GIndx;     /* global index structure */
extern I8s  GoDown; /* flag to bring system down to defragment memory */
extern I32s hangup; /* 0 = exit on error, 1 = hangup on error, for debugging */
extern HistType *Hist;       /* pointer to struct for histograms */
extern I32s HistSize; /* Num of elements alloced in Hist */
extern float HistNStars; /* ratio of counts to stars */
extern I32s HistPrint; /* boolean to tell us weather to print hists, to log */
extern I8s IMapFile[80];  /* inst map filename, default.map is assumed */
extern I8s IMode;      /* info display (plan,histo_size,histo_geno etc) */
extern I32s InstD[INSTNUM+1];/* data struct for freq dist. of instructions */
extern InstDef id[INSTNUM];
extern Event InstExe;	/* counter of instructions executed */
extern PInst is; /* struct for passing info between parse & execute */
extern I32s isolate;   /* isolate the genome of the cell extr */
extern Event LastDiv;	/* instructions executed at last divide */
extern I32s Log;         /* boolean for FE loging */
extern I32s MateSearchL;  /* used with HSEX, see soup_in.h */
extern float MateProb;  /* used with HSEX, see soup_in.h */
extern float MateXoverProp;  /* used with HSEX, see soup_in.h */
extern I32s MateSizeEp;  /* used with HSEX, see soup_in.h */
extern I32s Max_hits;  /* cardinality of most populous size class */
extern I32s MaxFreeBlocks; /* number allocated elements in FreeMem array */
extern float MaxMalMult;	/* multiple of cell size allowed for mal() */
extern I32s MemModeFree;  /* unix chmod for when mem is dealloced,  */
extern I32s MemModeProt;  /* chmod when mem is Alloced, NOT fully impl!*/
extern I8s mes[10][80]; /* array of strings for message passing to frontend */
extern I32s MalMode; /* 0= first fit, 1= better fit, 2= random preference,
  3= near mother's address, 4= near dx address, 5= near top of stack address*/
extern I32s MalReapTol; /* 1 = reap oldest creature within MalTol */
extern I32s MalTol; /* multiple of avgsize to search for free block */
extern I32s MalLimit;  /* search limit for memory allocation */
extern I32s MinCellSize;	/* minimum cell size */
extern I32s MinTemplSize;	/* minimum template size */
extern float MovPropThrDiv; /* min proportion of daught cell filled by mov */
extern I32s new_soup;  /* 0 = processed soup core, 1 = new soup core */
extern I32s  Nop0;  /* instruct num corresponding to nop0 */
extern I32s  Nop1;  /* instruct num corresponding to nop1 */
extern I32s  NopS;  /* sum of nops */
extern I32s NumCelAr; /* number of cells arrays */
extern I32s NumCells;  /* present number of cells in soup */
extern I32s NumGenDG; /* present # of permanent genotypes saved to disk */
extern I32s NumGenDM; /* present # of temporary genotypes swapped out to disk */
extern I32s NumGenRQ; /* present # of genotypes in RAM genequeue */
extern I32s NumGenotypes; /* # of genotypes of adult cells extant in soup */
extern I32s NumSizes; /* # of sizes of adult cells extant in soup */
extern I32s PhotonSize;/* number of instructions in photon */
extern I32s PhotonWidth; /* amount by which photons slide to determine fit */
extern I32s RamBankSiz;/* number of genotypes stored in RAM */
extern I32s RandIx1, RandIx2, RandIx3;	/* for tsrand() */
extern I32s RateFlaw;  /* frequency of flaws */
extern I32s RateMovMut;/* 1 / frequency of mutations per mov event */
extern I32s RateMut;   /* number of instructions per mutations */
extern I32s reaped;    /* 0 = reaper has not killed, 1 = reaper has killed */
extern float ReapRndProp; /* rnd prop of top of reaper Q to reap from */
extern I32s SaveFreq;  /* frequency of saving core_out, soup_out and list */
extern I32s SigBlockSet; /* mask to block sig int in unix, for certain calls */
extern I32s seed;      /* seed for random number generator */
extern I32s SizDepSlice;  /* 0 = slice constant, 1 = slice size dependent */
extern I32s siz_sl;    /* allocated size of *sl array */
extern I32s SliceSize; /* number of instructions executed in each slice */
extern I32s SliceStyle;/* select style of allocating slice size */
extern I8s soup_fn[85];/* place for soup_in filename */
extern I32s SoupBot;   /* index FreeMem struct for bottom of soup memory */
extern I32s SoupTop;   /* index of FreeMem struct for top of soup memory */
extern I8s Swap;   /* DOS flag whether to gq_swap or not */
extern FILE *tfp_log; /* file pointer for log */
extern I8s  TC_Menu;       /* flag unix sigint, to do a menu */
extern Pcells ThisSlice; /* cell that is currently active */
extern I32s TimeBirth; /* count of births in each million instruction */
extern I32s TimeDeath; /* count of deaths in each million instruction */
extern double TimePop; /* sum of ttime * NumCells for each million */
extern Pcells TopDummy; /* dummy cell at top of reaper queue */
extern Pcells TopReap;   /* cell at top reaper queue, next to die */
extern I32s TotFlaw;   /* total number of flaws in this run */
extern I32s TotMemUse; /* total memory use by soup, cells, and genebank */
extern I32s TotMovMut; /* total number of move mutations in this run */
extern I32s TotMut;    /* total number of background mutations in this run */
extern I32s WatchExe;  /* mark executed instructions in genome in genebank */
extern I32s WatchMov;  /* set mov bits in genome in genebank */
extern I32s WatchTem;  /* set template bits in genome in genebank */
extern I8s GenebankPath[80];	/* path for genebank */
extern I8s OutPath[80];   /* path for disk output */
extern I8s PhotonWord[80];	/* alphabetic representation of photon */
extern Instruction PhotonInst[80];/* instructional representation of photon */
extern I32s FreeMemCurrent;	/* current amount of free memory in soup */
extern float SearchLimit; /* limit on search, as multiple of average size */
extern I32s Search_limit; /* limit how far address instructions will search */
extern float PutLimit; /* distance for intercellular communication */
extern I32s Put_limit; /* limit on intercellular communications distance */
extern I32s SoupSize;  /* size of soup memory, measured in instructions */
extern Pcells Fp cells;  /* cells array */
extern SList **sl; /* list of size classes for genebanker */
extern Pmf FreeMem;    /* free memory array */
extern double PhotonPow;	/* power for photon match slice size */
extern double SlicePow;/* power for size dependent slice */
extern double TrandArray[98];	/* for trand() */
extern I32s  SavMinNum; /* minimum number of individuals to save genotype */
extern float SavThrMem; /* thresh memory prop. to save genotype */
extern float SavThrPop; /* thresh population prop. to save genotype */
extern float SlicFixFrac;	/* fixed fraction of slice size */
extern float SlicRanFrac;	/* random fraction of slice size */
extern GList *gq_bot;	/* bottom of gene queue */
extern GList *gq_top;	/* top of gene queue */
extern LastOut lo;	/* last data output to disk */
extern void (*slicer) ();

#ifdef ALCOMM
extern I16s           AL_run_flag;
extern I16s           VPORT;
#endif /* ALCOMM */

extern I8u MSG_X ;
extern I8u MSG_Y ;
extern I8u ERR_X ;
extern I8u ERR_Y ;
extern I8u PLN_X ;
extern I8u PLN_Y ;
extern I8u HLP_X ;
extern I8u HLP_Y ;

#ifdef MICRO
extern I32s MC_step;
extern Pcells MicroSlice; /* cell that is currently active */
#endif


#ifdef __TURBOC__
extern I16s FE_DV_Mode;
#endif

#endif /* LEXTERN_H */
