/* beagle.h  4-8-90  header file for the Beagle Explorer */
/*** Beagle Explorer:  Copyright (c) 1990  University of Delaware ***/

#ifndef BEAGLE_H
#define BEAGLE_H

#include <graphics.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <alloc.h>
#include <math.h>
#include <dos.h>                       /* supports the BIOS call */
#include <dir.h>
#include <dw.h>
#include <dwdata.h>
#include <dwmenu.h>
#include "gif/gif_lib.h"

#define INSTBITNUM 5
#define INSTNUM 32         /* INSTNUM = 2 ^ INSTBITNUM */
#define PLOIDY 1
#define INST 1 

#define Hp     huge  *
#define Fp     far   *
#define Np     near  *
typedef signed char      I8s;   /*  8 bit integer */
typedef unsigned char    I8u;   /*  8 bit integer */
typedef signed int       I16s;  /* 16 bit integer */
typedef unsigned int     I16u;  /* 16 bit integer */
typedef signed long      I32s;  /* 32 bit integer */
typedef unsigned long    I32u;  /* 32 bit integer */

#define ONE (I32u) 1

#define IsBit(seed, bit) ((((I32u) seed) & (ONE << ((I32u) bit))) ? 1 : 0)

#if INSTBITNUM == 5

typedef struct {
    unsigned int inst:5;
    unsigned int exec:1;
    unsigned int write:1;
    unsigned int read:1;
} Inst;

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

typedef struct g_list     GList;
typedef struct g_list Fp  Pgl;

typedef struct { /* structure for instruction set definitions */
    I8s op;         /* op code */
    I8s mn[9];      /* assembler mnemonic */
} ArgInstDef;

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

struct g_list { /* structure for genotype list */
    I32s pop;   /* current number of adults of this genotype in soup */
    Genotype gen;   /* genotype of creature */
    Genotype parent;/* genotype of parent genotype (ancestor) */
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

typedef struct {
    I8s   magic[4];
    I32s  g_off;
    I16s  size, n, n_alloc;
} head_t;

typedef struct {
    I8s   gen[3];
    I8s   pgen[3];
    I16s  psize;
    I32u  bits;
    I32s  originC;
    I16s  mpp, mpi;
    Event mppT;		 /* last time of mpp update */
    I32s  ptr;           /* reserved for future versions */
    Event originI;
    Metabolism d1, d2;
    I8s   pt;           /* ploidy and track */
} indx_t;

typedef unsigned int   Uint;
typedef unsigned long  Ulong;

struct last_out {
    Ulong  time;
    Ulong  ctime;
    char   bd;
    Uint   size;
    char   label[4];
    } ;

struct sbar {  /* coordinates of corners of bar */
    int  left;
    int  top;
    int  right;
    int  bottom;
    } ;

struct barst {   /* setup for bar screen */
    double nbr;  /* maximum number of bars */
    float  csz;  /* character size in pixels */
    float  bwd;  /* bar width in pixels */
    float  sbb;  /* space between bars in pixels */
    float  lbd;  /* left border in pixels */
    float  tbd;  /* top border in pixels */
    float  thd;  /* space above first bar in pixels */
    float  nsz;  /* number of characters in size part of left labels */
    float  nge;  /* number of characters in genotype part of left labels */
    float  nch;  /* number of characters in left labels (nsz + nge) */
    float  inb;  /* indent to bar, from numbers */
    float  bas;  /* indent to bar, from left border = lbd + n * csz + inb */
    } ;

struct tracest {
    int  csz;  /* character size in pixels */
    int  lbd;  /* left border in pixels */
    int  tbd;  /* top border in pixels */
    int  bti;  /* characters before time */
    int  tim;  /* characters in time */
    } ;

struct species {
    int   size;     /* size of this species */
    char  label[4]; /* genotype */
    int   num;      /* current number of individuals */
    int   max;      /* maximum number of individuals */
    int   xpm;      /* maximum x-pixel */
    int   xpo;      /* old x-pixel */
    long  min;      /* maximum number of instructions */
    } ;

struct rspecies {
    int   size;
    char  label[4];
    int   begin;    /* number at beginning of period */
    int   num;      /* number at end of period */
    int   min;      /* minimum number during run */
    int   max;      /* maximum number during run */
    } ;

struct use_template {
    int  locus;
    int  compl_locus;
    int  size;
    char  inst[7];
    char  template[15];
    } ;

struct geneprobe {
    int  fit;       /* fit of probe to genome */
    int  position;  /* position on genome of probe */
    int  diff;      /* size of insertion (+) or deletion (-) */
    int  idpos;     /* position on probe after which ins or del occurs */
    int  start;     /* position to start in case of overlap */
    int  stop;      /* position to stop in case of overlap */
    } ;

struct insdel {
    int  first;  /* index to geneprobe array of 1st part of probe */
    int  second; /* index to geneprobe array of 2nd part of probe */
    int  diff;   /* size of insertion (+) or deletion (-) */
    int  idpos;  /* position on probe after which ins or del occurs */
    int  fit;    /* best combined fit */
    } ;

struct gene_dat {
    struct tnode  *t;
    long          index;
    } ;

struct snode {
    long             max;  /* max pop value upon which tree is sorted */
    long             num;  /* number of genotypes of this max value */
    long             gsiz; /* allocated size of *gd array */
    struct gene_dat  *g;
    struct snode     *l;  /* left sub-tree */
    struct snode     *r;  /* right sub-tree */
    } ;

struct pop_dat {
    long  max; /* maximum population of this genotype */
    long  min; /* minimum population of this genotype */
    long  beg; /* beginning population of this genotype */
    long  end; /* current population of this genotype */
    } ;

struct tnode {
    long            size;  /* genome size */
    struct pop_dat  sd; /* pop_dat for this size class */
    int             gsize; /* allocated size of *g array */
    struct pop_dat  *g; /* array of pop_dat structures */
    struct tnode    *l;  /* left sub-tree */
    struct tnode    *r;  /* right sub-tree */
    } ;

struct xyrange { float xn; float xx; float yn; float yx; int n; int f; };
struct point   { float x; float y; };
struct polar	{ float r;	float th; } ;
struct rect	{ float dx;	float dy; } ;
struct gd { double d;	int bad; };
struct gf { float f;	int bad; };
struct gi { int i;	int bad; };
struct gl { long l;	int bad; };
struct gc { char c;	int bad; };
struct gs { char s[99];	int bad; };

#include "protob.h"

#endif /* BEAGLE_H */
