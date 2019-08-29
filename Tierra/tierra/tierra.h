/* tierra.h  9-9-92 */
/* type, structure and function definitions for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * tierra_h_sccsid: @(#)tierra.h	1.5    7/21/92
 */

#include "license.h"

#ifndef LTIERRA_H
#define LTIERRA_H

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include "configur.h"
#include "portable.h"
#ifndef ANSI
#include <varargs.h>
#else /* ANSI */
#include <stdarg.h>
#endif /* ANSI */

#ifdef ALCOMM
#include <mlayer.h>
#endif

#ifdef unix
#if FRONTEND == BASIC
#include <curses.h>
#endif /*FRONTEND == BASIC */
#define KEYHIT() ( TC_Menu )
#endif /* unix */

#ifdef __TURBOC__
#define KEYHIT() ( kbhit() )
#endif /* TURBOC */

#define ONE (I32u) 1

#define tulrand() ((I32u) (tdrand() * (double) ULONG_MAX))
/* returns random 32 bit positive signed integer */
#define tlrand() ((I32s) (tdrand() * (double) LONG_MAX))
/* returns random 32 bit positive signed integer */
#define tirand() ((I16s) (tdrand() * (double) INT_MAX))
/* returns random 16 bit positive signed integer */
#define tuirand() ((I16u) (tdrand() * (double) UINT_MAX))
/* returns random 16 bit unsigned integer */
#define tcrand() ((I8s) (tdrand() * (double) CHAR_MAX))
/* returns random 8 bit positive signed integer */
#define tucrand() ((I8u) (tdrand() * (double) UCHAR_MAX))
/* returns random 8 bit unsigned integer */

#define SetFlag(A)  (A->c.fl = 1)
#define IsBit(seed, bit) ((((I32u) seed) & (ONE << ((I32u) bit))) ? 1 : 0)
#define ad(A) ((A) >=0 ? ((A)%SoupSize) \
               : ((SoupSize-(-(A)%SoupSize))%SoupSize))
#define mo(A,B) ((A) >=0 ? ((A)%(B)) : (((B)-(-(A)%(B)))%(B)))

#define WRITE 1
#define NOWRITE 0
#define EXIT 1
#define NOEXIT 0
#define PLN_STATS 1
#define SIZ_HIST 2
#define SIZM_HIST 3
#define GEN_HIST 4

#if INSTBITNUM == 5

#ifdef __TURBOC__

typedef struct {
    unsigned int inst:5;
    unsigned int exec:1;  /* 0 = unprotected, 1 = protected */
    unsigned int write:1; /* 0 = unprotected, 1 = protected */
    unsigned int read:1;  /* 0 = unprotected, 1 = protected */
} Inst;

#else /* __TURBOC__ */
#ifdef __TRS6000__
typedef struct {
    unsigned int inst:5;
    unsigned int exec:1;  /* 0 = unprotected, 1 = protected */
    unsigned int write:1; /* 0 = unprotected, 1 = protected */
    unsigned int read:1;  /* 0 = unprotected, 1 = protected */
} Inst;

#else /* __TURBOC__ */

typedef struct {
    unsigned char inst:5;
    unsigned char exec:1;  /* 0 = unprotected, 1 = protected */
    unsigned char write:1; /* 0 = unprotected, 1 = protected */
    unsigned char read:1;  /* 0 = unprotected, 1 = protected */
} Inst;

#endif /* __TRS6000__ */
#endif /* __TURBOC__ */

#endif /* INSTBITNUM == 5 */


#if PLOIDY == 1

typedef Inst Instruction;

#else 

typedef Inst Instruction[PLOIDY];

#endif /* PLOIDY == 1 */

typedef Instruction Hp HpInst;
typedef Instruction Fp FpInst;
typedef Instruction Np NpInst;
typedef Inst Fp FpIns;

#if PLOIDY == 1
typedef I8s GenBits;
#else /* PLOIDY > 1 */
typedef I8s GenBits[PLOIDY];
#endif /* PLOIDY > 1 */

typedef GenBits Fp FpGenB;

typedef struct cell       Cell;
typedef struct cell Fp    Pcells;
typedef struct g_list     GList;
typedef struct g_list Fp  Pgl;

typedef I32s  Reg;   /* type for use in CPU registers */
typedef Reg   *Preg; /* pointer to register */

typedef struct { /* struct for passing arguments from parse to execute */

    Preg   sreg; /* pointer to source register */
    FpIns  sins; /* pointer to source instruction */
    I32s   sval; /* source value */
    I8s    stra; /* source track */

    Preg   dreg;  /* pointer to destination register */
    I32s   dval;  /* original destination value */
    FpIns  dins;  /* pointer to destination instruction */
    I8s    dtra;  /* destination track */
    I32s   dmod;  /* destination modulused positive this size */
    I32s   dran;  /* destination kept in signed range of this value */
    Pcells dcel;  /* destination cell */

    Preg   sreg2; /* pointer to 2nd source register */
    FpIns  sins2; /* pointer to 2nd source instruction */
    I32s   sval2; /* 2nd source value */
    I8s    stra2; /* 2nd source track */

    Preg   dreg2; /* pointer to 2nd destination register */
    I32s   dval2; /* original destination value */
    FpIns  dins2; /* pointer to 2nd destination instruction */
    I8s    dtra2; /* 2nd destination track */
    I32s   dmod2; /* 2nd dest modulused positive this size */
    I32s   dran2; /* 2nd dest kept in signed range of this value */

    I32s   sval3; /* 3rd source value */
    I32s   dval3; /* original destination value */

    Preg   dreg3; /* pointer to 3rd destination register */
    I32s   dmod3; /* 3rd dest modulused positive this size */
    I32s   dran3; /* 3rd dest kept in signed range of this value */

    I8s    mode;  /* mode of instruction */
    I8s    mode2; /* 2nd mode of instruction */
    I8s    mode3; /* 3rd mode of instruction */

    I8s    expr;  /* execute protection 0 = no protection, 1 = protected */
    I32s   oip;   /* address of instruction being executed: ce->c.ip */
    FpIns  eins;  /* pointer to instruction being executed */
    I8s    iip;   /* amount to increment instruction pointer */
    I8s    dib;   /* amount to decrement instruction bank */
    I16s   ts;    /* size of time slice, used to control central loop */
} PInst;

typedef struct { /* structure for instruction set definitions */
    I8s op;                      /* op code */
    I8s mn[9];                   /* assembler mnemonic */
    void (*execute) P_((Cell  *));  /* pointer to execute function */
    void (*parse) P_((Cell  *));    /* pointer to parse function */
} InstDef;

typedef struct {  /* structure for time measured in executed instructions */
    I32s m;     /* count of millions of instructions */
    I32s i;     /* count of instructions */
} Event;

typedef struct {
    I32s size;        /* size class (~species) */
    I8s label[4];     /* genotype label */
} Genotype;

typedef struct {
    I32s si;           /* size index */
    I16s gi;           /* genotype index */
} GlIndex;

typedef struct {  /* structure for metabolic data */
    I32s  inst;      /* count of instructions executed */
    I32s  flags;     /* count of flags (error conditions) set */
    I32s  mov_daught;/* count of number of instructions moved to daughter */
    I8s   BreedTrue; /* 0=not, 1 = this daughter genetically same as parent */
} Metabolism;

typedef struct { /* structure for indexing cells in the cells arrays */
    I16u  a; /* which array */
    I16u  i; /* which element of the array */
} CellInd;

typedef struct { /* structure for demographic data */
    Genotype gen;   /* size and genotype name of cell */
    I16s gi;         /* index to this genotype in the gl array */
    I32s hash;       /* hash value for this genotype */
    I32s fecundity;  /* count of number of daughter cells produced */
    I32s flags;      /* count of flags (error conditions) set */
    I32s mov_daught; /* count of number of instructions moved to daughter */
    I32s inst;       /* count of instructions executed */
    I32s mut;        /* 0 = no somatic mutations, >= 1 = somatic mutations */
    I32s flaw;       /* 0 = no flaws, >= 1 = flaws */
    Metabolism d1;   /* metabolic data for first daughter */
    Genotype parent; /* size and genotype name of parent */
    I16s ib;         /* instruction bank */
    CellInd  ne;     /* address of daughter cell */
    I8s is;        /* 1 = this cpu is active, in the slicer; 0 = not active */
    I8s tr;             /* which track currently being executed */
    I8s dm;        /* 0 = mother, 1 = daughter */
    I8s ploidy;    /* how many tracks */
    FpInst genome; /* pointer to genome itself */
#ifdef HSEX
    I32s mate_addr;   /* soup address of mate */
    I16s x_over_addr; /* # bytes into gen for Xover, - first half, + second */
#endif /* HSEX */
} Dem;

typedef struct { /* pointers to this, previous and next cells in queues */
    CellInd  this;   /* index of this cell */
    CellInd  n_time;   /* index to next cell in slicer queue */
    CellInd  p_time;   /* index to previous cell in slicer queue */
    CellInd  n_reap;   /* index to next cell in reaper queue */
    CellInd  p_reap;   /* index to previous cell in reaper queue */
} Que;

typedef struct {   /* structure for allocated memory block of cell */
    I32s p;     /* location of start of cell memory */
    I32s s;    /* size of cell memory */
} Mem;

#if INST == 1

typedef struct {        /* structure for registers of virtual cpu */
    Reg re[ALOC_REG];   /* array of registers */
    Reg sp;             /* stack pointer */
    Reg st[STACK_SIZE]; /* stack */
    Reg ip;             /* instruction pointer */
    I8s fl;             /* flag */
} Cpu;

#else /* INST == 1 */

typedef struct {         /* structure for registers of virtual cpu */
    Reg re[ALOC_REG];    /* array of registers */
    Reg sp;              /* stack pointer */
    Reg st[STACK_SIZE];  /* stack */
    Reg gb[GETBUFSIZ+3]; /* input buffer */
    Reg pb[PUTBUFSIZ+3]; /* output buffer */
    Reg ip;              /* instruction pointer */
    I8s fl;              /* flag */
} Cpu;

#endif /* INST == 1 */

struct cell {   /* structure for cell of organisms */
    Dem d;  /* fecundity and times and dates of birth and death */
    Que q;  /* pointers to previous and next cells in queues */
    Mem mm; /* main cell memory */
    Mem md; /* daughter cell memory */
    Cpu c;  /* virtual cpu */
    I8s ld; /* 0 = dead, 1 = alive */
} ;     /* sizeof(struct cell) = XX */

/* Structures for new soup allocator. CJS, July 1992. */

typedef struct {      /* Describes a free area of the soup */ 
  I32s    l,          /* Index of left son in cartesian tree; 
                         or index of 1st recycled node (>0);
                         or -(number of untouched nodes)   */
          r,          /* Index of rt son in cartesian tree */
          p,          /* Soup addr of this unoccupied area */
          s;          /* Size of the area (in instr slots) */
} MemFr ;

typedef MemFr Fp  Pmf;

struct g_list { /* structure for genotype list */
    I32s pop;   /* current number of adults of this genotype in soup */
    Genotype gen;   /* genotype of creature */
    Genotype parent;/* genotype of parent genotype (ancestor) */
    I32s hash;      /* hash number for genome identification */
    I32u bits;             /* see below */
    Metabolism d1;  /* metabolic data for first daughter */
    Metabolism d2;  /* metabolic data for second daughter */
    Event originI;  /* time of origin, in instruction time */
    I32s originC;      /* time of first origin of genotype, in clock time */
    float MaxPropPop;  /* max. propor. of NumCells reached by this gen. */
    float MaxPropInst; /* max. propor. of SoupSize reached by this gen. */
    Event mpp_time;    /* most recent time of MaxPropPop update */
    I8s ploidy;        /* how many tracks */
    I8s track;         /* which track are we on now ... */
    FpInst genome;     /* pointer to genome itself */
    FpGenB gbits;      /* pointer to genome bit field */
    struct g_list  *b;  /* next (below) in queue */
    struct g_list  *a;  /* previous (above) in queue */
} ;

typedef struct {    /* structure for size list */
    I32s num_c;    /* # adult creatures of this size in soup */
    I16s num_g;    /* # genotypes of this size extant in soup */
    I16s a_num;        /* allocated size of *g array */
    GList **g; /* array of GList structures */
} SList;

typedef struct {
  I16s  size ;
  I16s  lbl;
  I32s  count;
  I8u   nstar;
  } HistType;


/* definitions of bits:
    bit  0  permanent genotype name, saved in .gen file
    bit  1  swapped out to disk from the rambank, saved in .mem file
    bit  2  EXs = executes own instructions (self)
    bit  3  EXd = executes daughter's instructions
    bit  4  EXo = executes other cell's instructions
    bit  5  EXf = executes instructions in free memory
    bit  6  EXh = own instructions are executed by other creature (host)
    bit  7  TCs = matches template complement of self
    bit  8  TCd = matches template complement of daughter
    bit  9  TCo = matches template complement of other
    bit 10  TCf = matches template complement of free memory
    bit 11  TCh = own template complement is matched by other creature (host)
    bit 12  TPs = uses template pattern of self
    bit 13  TPd = uses template pattern of daughter
    bit 14  TPo = uses template pattern of other
    bit 15  TPf = uses template pattern of free memory
    bit 16  TPh = own template pattern is used by other creature (host)
    bit 17  MFs = moves instruction from self
    bit 18  MFd = moves instruction from daughter
    bit 19  MFo = moves instruction from other cell
    bit 20  MFf = moves instruction from free memory
    bit 21  MFh = own instructions are moved by other creature (host)
    bit 22  MTs = moves instruction to self
    bit 23  MTd = moves instruction to daughter
    bit 24  MTo = moves instruction to other cell
    bit 25  MTf = moves instruction to free memory
    bit 26  MTh = is written on by another creature (host)
    bit 27  MBs = executing other creatures code, moves inst from self
    bit 28  MBd = executing other creatures code, moves inst from daughter
    bit 29  MBo = executing other creatures code, moves inst from other cell
    bit 30  MBf = executing other creatures code, moves inst from free memory
    bit 31  MBh = other creature uses another cpu to move your instructions
*/

typedef struct {    /* record of last data output to disk */
    I32s  time;     /* elapsed time */
    I32s  ctime;    /* millions of instructions */
    I8s   bd;       /* b = birth, d = death */
    I32s  size;     /* size of creature */
    I8s   label[4]; /* genotype name of creature, e.g., aaa */
} LastOut;

typedef struct {
    I8s   magic[4];
    I32s  g_off;    /* ofsett where genomes begin */
    I16s  size;     /* size of genome */
    I16s  n;        /* number of genomes in bank */
    I16s  n_alloc;  /* allocated size of bank */
} head_t;

typedef struct {
    I8s   gen[3];
    I8s   pgen[3];
    I16s  psize;
    I32s  hash;
    I32u  bits;
    I32s  originC;
    I16s  mpp, mpi;
    Event mppT;		 /* last time of mpp update */
    I32s  ptr;           /* reserved for future versions */
    Event originI;
    Metabolism d1, d2;
    I8s   pt;           /* ploidy and track */
} indx_t;

#include "prototyp.h"

#endif /* LTIERRA_H */
