/* soup_in.h   9-9-92  Artificial Life simulator setup routines */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * sccsid: @(#)soup_in.h	1.5	7/21/92
 */

#include "license.h"

#ifndef SOUP_IN_H
#define SOUP_IN_H

/* observational parameters: */

I32s BrkupSiz = 1024;  /* size of output file in K: break.1, break.2 ... */
I32s CumGeneBnk = 0;    /* Use cumulative gene files, or overwrite */
I32s debug = 0;	       /* 0 = off, 1 = on, printf statements for debugging */
I32s DiskOut = 1;      /* output data to disk (1 = on, 0 = off) */
I32s GeneBnker = 1;    /* turn genebanker on and off */
I8s GenebankPath[80] = "gb/";	/* path for genebanker output */
I32s hangup = 0;    /* 0 = exit on error, 1 = hangup on error for debugging */
I32s Log = 0;    /* 0 = no log file, 1 = write tierra.log file to disk */
I32s MaxFreeBlocks = 600;/* initial # of structure for memory allocation */
I8s OutPath[80] = "td/";	/* path for data output */
I32s RamBankSiz = 30000;/* max num genotypes in ram, use with genebanker */
I32s SaveFreq = 100;   /* frequency of saving core_out, soup_out and list */
I32s  SavMinNum = 2;   /* minimum number of individuals to save genotype */
float SavThrMem = .015;/* threshold memory occupancy to save genotype */
float SavThrPop = .015;/* threshold population proportion to save genotype */
I32s WatchExe = 0;     /* mark executed instructions in genome in genebank */
I32s WatchMov = 0;     /* set mov bits in genome in genebank */
I32s WatchTem = 0;     /* set template bits in genome in genebank */

/* environmental variables: */

I32s alive = 500;      /* how many millions of instruction will we run */
float DistFreq = .3;    /* frequency of disturbance, factor of recovery time */
float DistProp = .2;   /* proportion of population affected by distrubance */
I32s DivSameSiz = 0;   /* produce offspring of same size, stop size change */
I32s DivSameGen = 0;  /* produce offspring of same genotype, stop evolution */
I32s DropDead = 5;     /* stop system if no reproduction in last x million */
float GenPerBkgMut = 16.; /* mut rate control by generations ("cosmic ray") */
float GenPerFlaw = 32.;/* flaw control by generations */
float GenPerMovMut = 8.;/* mutation rate control by generations (copy mut) */
I8s IMapFile[80] = "opcode.map"; /* map of instructions set into space */
I32s MalMode = 2; /* 0 = first fit, 1 = better fit, 2 = random preference,
3 = near mother's address, 4 = near dx address, 5 = near top of stack address*/
I32s MalReapTol = 1; /* 0 = reap by queue, 1 = reap oldest creat wthn MalTol */
I32s MalTol = 5; /* multiple of avgsize to search for free block */
float MateProb = .2;	/* probability of mating, this mal */
I32s MateSearchL = 0;	/* Search limit to find a mate, like SearchLimit */
I32s MateSizeEp = 1;	/* critters size +- val => allowed to mate with */
float MateXoverProp = 1.0; /* prop. of gene to consider for Xover point */
float MaxMalMult = 3;  /* multiple of cell size allowed for mal() */
I32s MinCellSize = 8;  /* minimum size for cells */
I32s MinTemplSize = 1; /* minimum size for templates */
float MovPropThrDiv = .7;/* min proportion of daughter cell filled by mov */
I32s MemModeFree = 0;  /* unix chmod for when mem is dealloced,  */
I32s MemModeProt = 2;  /* unix chmod for when mem is Alloced, NOT fully impl!*/
I32s new_soup = 1;     /* 1 = this a new soup, 0 = restarting an old run */
I32s NumCells = 1;    /* # of creatures and gaps used to inoculate new soup */
double PhotonPow = 1.5;/* power for photon match slice size */
I32s PhotonWidth = 8;  /* amount by which photons slide to find best fit */
I8s PhotonWord[80] = "chlorophill";	/* word used to define photon */
float PutLimit = 20.;   /* distance for intercellular communication */
float ReapRndProp = 0.0; /* rnd prop of top of reaper Q to reap from */
float SearchLimit = 5.;   /* distance for template search */
I32s seed = 0; /* seed for random number generator, 0 uses time to set seed */
I32s SizDepSlice = 1;  /* set slice size by size of creature */
double SlicePow = 1;  /* set power for slice size, use when SizDepSlice = 1 */
I32s SliceSize = 25;   /* slice size when SizDepSlice = 0 */
I32s SliceStyle = 2;   /* choose style of determining slice size */
float SlicFixFrac = 0; /* fixed fraction of slice size */
float SlicRanFrac = 2; /* random fraction of slice size */
I32s SoupSize = 60000L;  /* size of soup in instructions */

/* an edited version of the id[INSTNUM] array must be created and placed in
   the arginst.h file, for use by the assembler/disassembler of arg.c */

#if INST == 1

InstDef id[INSTNUM] = {
    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "not0", not0, pnot0},
    {0x03, "shl", shl, pshl},
    {0x04, "zero", movdd, pzero},
    {0x05, "ifz", ifz, pifz},
    {0x06, "sub_ab", math, psub_ab},
    {0x07, "sub_ac", math, psub_ac},
    {0x08, "inc_a", math, pinc_a},
    {0x09, "inc_b", math, pinc_b},
    {0x0a, "dec_c", math, pdec_c},
    {0x0b, "inc_c", math, pinc_c},
    {0x0c, "pushax", push, ppushax},
    {0x0d, "pushbx", push, ppushbx},
    {0x0e, "pushcx", push, ppushcx},
    {0x0f, "pushdx", push, ppushdx},
    {0x10, "popax", pop, ppopax},
    {0x11, "popbx", pop, ppopbx},
    {0x12, "popcx", pop, ppopcx},
    {0x13, "popdx", pop, ppopdx},
    {0x14, "jmp", adr, ptjmp},
    {0x15, "jmpb", adr, ptjmpb},
    {0x16, "call", tcall, ptcall},
    {0x17, "ret", pop, pret},
    {0x18, "movcd", movdd, pmovdc},
    {0x19, "movab", movdd, pmovba},
    {0x1a, "movii", movii, pmovii},
    {0x1b, "adr", adr, padr},
    {0x1c, "adrb", adr, padrb},
    {0x1d, "adrf", adr, padrf},
    {0x1e, "mal", malchm, pmal},
    {0x1f, "divide", divide, pdivide}
};

#endif /* INST == 1 */

#if INST == 2

InstDef id[INSTNUM] = {
    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "ax", regorder, pax},
    {0x03, "bx", regorder, pbx},
    {0x04, "cx", regorder, pcx},
    {0x05, "dx", regorder, pdx},
    {0x06, "movdd", movdd, pmovdd},
    {0x07, "movdi", movdi, pmovdi},
    {0x08, "movid", movid, pmovid},
    {0x09, "movii", movii, pmovii},
    {0x0a, "push", push, ppush},
    {0x0b, "pop", pop, ppop},
    {0x0c, "put", put, pput},
    {0x0d, "get", get, pget},
    {0x0e, "inc", math, pinc},
    {0x0f, "dec", math, pdec},
    {0x10, "add", math, padd},
    {0x11, "sub", math, psub},
    {0x12, "zero", movdd, pzero},
    {0x13, "shl", shl, pshl},
    {0x14, "not0", not0, pnot0},
    {0x15, "not", not, pnot},
    {0x16, "ifz", ifz, pifz},
    {0x17, "iffl", ifz, piffl},
    {0x18, "jmp", adr, ptjmp},
    {0x19, "jmpb", adr, ptjmpb},
    {0x1a, "call", tcall, ptcall},
    {0x1b, "adr", adr, padr},
    {0x1c, "adrb", adr, padrb},
    {0x1d, "adrf", adr, padrf},
    {0x1e, "mal", malchm, pmal},
    {0x1f, "divide", divide, pdivide}
};

#endif /* INST == 2 */

#if INST == 3

InstDef id[INSTNUM] = {
    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "rollu", rollu, pnop},
    {0x03, "rolld", rolld, pnop},
    {0x04, "enter", enter, pnop},
    {0x05, "exch", exch, pnop},
    {0x06, "movdi", movdi, pmovdi},
    {0x07, "movid", movid, pmovid},
    {0x08, "movii", movii, pmovii},
    {0x09, "push", push, ppush},
    {0x0a, "pop", pop3, ppop},
    {0x0b, "put", put, pput},
    {0x0c, "get", get, pget},
    {0x0d, "inc", math, pinc},
    {0x0e, "dec", math, pdec},
    {0x0f, "add", math3, padd},
    {0x10, "sub", math3, psub},
    {0x11, "zero", movdd3, pzero},
    {0x12, "shl", shl, pshl},
    {0x13, "not0", not0, pnot0},
    {0x14, "not", not, pnot},
    {0x15, "rand", movdd3, prand},
    {0x16, "ifz", ifz, pifz},
    {0x17, "iffl", ifz, piffl},
    {0x18, "jmp", adr, ptjmp},
    {0x19, "jmpb", adr, ptjmpb},
    {0x1a, "call", tcall, ptcall},
    {0x1b, "adr", adr3, padr},
    {0x1c, "adrb", adr3, padrb},
    {0x1d, "adrf", adr3, padrf},
    {0x1e, "mal", malchm3, pmal},
    {0x1f, "divide", divide, pdivide}
};

#endif /* INST == 3 */

#if INST == 4

InstDef id[INSTNUM] = {
    {0x00, "nop0", nop, pnop},
    {0x01, "nop1", nop, pnop},
    {0x02, "movdi", movdi, pmovdi},
    {0x03, "movid", movid, pmovid},
    {0x04, "movii", movii, pmovii},
    {0x05, "pushax", push, ppushax},
    {0x06, "pushbx", push, ppushbx},
    {0x07, "pushcx", push, ppushcx},
    {0x08, "pushdx", push, ppushdx},
    {0x09, "popax", pop, ppopax},
    {0x0a, "popbx", pop, ppopbx},
    {0x0b, "popcx", pop, ppopcx},
    {0x0c, "popdx", pop, ppopdx},
    {0x0d, "put", put, pput},
    {0x0e, "get", get, pget},
    {0x0f, "inc", math, pinc},
    {0x10, "dec", math, pdec},
    {0x11, "add", math, padd},
    {0x12, "sub", math, psub},
    {0x13, "zero", movdd, pzero},
    {0x14, "shl", shl, pshl},
    {0x15, "not0", not0, pnot0},
    {0x16, "ifz", ifz, pifz},
    {0x17, "iffl", ifz, piffl},
    {0x18, "jmp", adr, ptjmp},
    {0x19, "jmpb", adr, ptjmpb},
    {0x1a, "call", tcall, ptcall},
    {0x1b, "adr", adr, padr},
    {0x1c, "adrb", adr, padrb},
    {0x1d, "adrf", adr, padrf},
    {0x1e, "mal", malchm, pmal},
    {0x1f, "divide", divide, pdivide}
};

#endif /* INST == 4 */

#endif /* SOUP_IN_H */
