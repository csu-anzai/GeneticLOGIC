/*************************************************************/
/*                                                           */
/*  Copyright (c) 1986                                       */
/*  John J. Grefenstette                                     */
/*  Navy Center for Applied Research in AI                   */
/*  Naval Research Laboratory                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/

/*
 *	$Id: define.h,v 1.3 1992/06/19 16:55:21 baeck Exp $
 *	$Log: define.h,v $
 * Revision 1.3  1992/06/19  16:55:21  baeck
 * *** empty log message ***
 *
 * Revision 1.2  1992/06/19  16:43:36  baeck
 * Adaptive crossover removed
 *
 * Revision 1.1  1992/06/19  16:43:21  baeck
 * Initial revision
 *
 *
 *
 *      file:   define.h
 *
 *    author:   John J. Grefenstette
 *
 *   created:   1981
 *
 *   purpose:   global definitions for genesis
 *
 *  modified:	Thomas Baeck, 18 oct 90
 *			NSZ definition added to create a general
 *			definition for the length of filenames etc.
 *
 *		Thomas Baeck,  2 nov 90
 *			New random number generator added
 *
 *		Thomas Baeck,  5 dec 90
 *			struct FUNCTION added
 *
 *		Thomas Baeck, 27 dec 90
 *			macro for index calculation added
 *
 *		Thomas Baeck, 20 feb 91
 *			selection schemes
 *
 */
 
#include <stdio.h>
#include <math.h>
#include <malloc.h>

/*********************** SYSTEM DEPENDENCY ******************************/
 
#define INTSIZE 	32   		/* bits per unsigned int */
#define _PTHSEP 	"/"		/* separator for filenames */
#define _FILSEP 	"."		/* separator for filename, extension */
#define _CRTDIR 	"mkdir"		/* make directory command */
#define	C_SLASH		'/'		/* path component separator */
#define	S_SLASH		"/"

#define	C_DOT		'.'		/* extention separator */
#define	S_DOT		"."

#define	C_LBRACE	'{'		/* left  curly brace */
#define	S_LBRACE	"{"
#define	C_RBRACE	'}'		/* right curly brace */
#define	S_RBRACE	"}"

#define	C_UNDERSCORE	'_'		/* variant separator */
#define	S_UNDERSCORE	"_"

#define	C_COMMENT	'#'		/* comment character */
#define	S_COMMENT	"#"

#define	C_PERCENT	'%'		/* printf()-format introducer */
#define	S_PERCENT	"%"

#define	S_WHITESPACE	" \t\r\n\v\f"	/* list of white space characters */
#define	S_STRDEL	"\'\""		/* list of string delimiters */

#define	B_CHAR		8		/* #bits per char */
#define	B_INT		(B_CHAR*sizeof(int))
#define	B_LONG		(B_CHAR*sizeof(long))

#define	LARGE_VAL	1e+39		/* large floating point value */

 
/******************** END SYSTEM DEPENDENCY *****************************/

#define GENESIS	"GENEsYs 1.0\t(C) 1991 Thomas Baeck"
#define _GS	"GENEsYs 1.0"
#define LOGLEN	40		/* length of printed logfile strings */

#define	ROWS	100		/* rows in the data tables */
#define DTACOL	12		/* columns in the general data table */
#define BUFCNT	3		/* number of buffers */
#define _SGLLIN 0		/* flush buffer single line modus */
#define _MULLIN 1		/* flush buffer multiple line modus */
#define MOPT	3		/* maximum option number per objective fct */

	/* general state descriptors */

#define	_SINZ	0		/* initialization state */
#define _SRUN	1		/* running state */
#define _STRM	2		/* termination state */

	/* function table primitives */

#define VRBL	0		/* variable dimension */
#define STRC	1		/* strict, binding dimension setting */
#define EXCP	2		/* exception from the other cases */

#define REAL	0		/* individual is a string of real numbers */
#define PERM	1		/* set iff individual encodes a permutation */
#define BINY	2		/* individual is a binary string */

#define	DUMMY	"dummy"				/* dummy string		*/

	/* selection schemes */

#define PRP_SLT 'P'		/* proportional */
#define BZM_SLT 'B'		/* Boltzmann */
#define BRK_SLT 'R'		/* Baker's ranking */
#define IRK_SLT 'I'		/* inverse Baker's ranking */
#define WRK_SLT 'W'		/* Whitley's ranking */
#define MLR_SLT 'M'		/* (m,l) */
#define MLC_SLT 'C'		/* (m,l) with best group copying */

	/* mutation schemes */

#define STD_MTT 'S'		/* standard */
#define ADT_MTT 'A'		/* adaptive mutation, incl.mutation rate */
#define ADX_MTT 'X'		/* adaptive mutation, excluding mutation rate */
#define OPT_MTT 'O'		/* optimal mutation - ONLY FOR f12 !! */

	/* recombination schemes */

#define STD_REC 'S'		/* standard */
#define UFM_REC 'U'		/* uniform crossover */
#define DCT_REC 'D'		/* discrete recombination */
#define IMD_REC 'I'		/* intermediate recombination */
#define RID_REC 'R'		/* random intermediate recombination */
#define NOP_REC '_'		/* no recombination */


	/* general values which are valid for all objective functions	*/

#define EPS	1.0e-9			/* small value */

#define MAXMRATE 0.5			/* maximum mutation rate */

#define MAXARGS	500			/* maximum number of arguments */

#define LGTH	INTSIZE			/* maximum length of chromosome	*/

#define	NSZ	128			/* length of strings */ 

	/* default values for various global variables			*/

#define FUN_DEF 1			/* number of objective function */
#define DIM_DEF 30			/* dimension for f1		*/
#define EXP_DEF	1			/* number of experiments	*/
#define TRL_DEF 1000			/* number of trials		*/
#define PSZ_DEF	50			/* population size		*/
#define LGT_DEF 960			/* length of individuals	*/
#define CRT_DEF 0.6			/* crossover rate		*/
#define CPT_DEF 2			/* crossover points		*/
#define REC_DEF "S_"			/* recombination scheme		*/
#define MRT_DEF 0.001			/* mutation rate		*/
#define MBT_DEF 0			/* bits for mutation rate	*/
#define NBR_DEF 0 			/* number of mutation rates	*/
#define MTT_DEF STD_MTT			/* mutation scheme		*/
#define NOB_DEF 50			/* right extinctive bound	*/
#define RHO_DEF 1			/* left extinctive bound	*/
#define ETA_DEF 1.1			/* eta-max for ranking		*/
#define GAP_DEF 1.0			/* generation gap		*/
#define	SLT_DEF PRP_SLT			/* proportional selection	*/
#define TMP_DEF 0.99			/* temperature control		*/
#define CHN_DEF 5			/* markov chain length		*/
#define WSZ_DEF 5			/* windowsize			*/
#define RPI_DEF 100			/* report interval		*/
#define STR_DEF 1			/* structures saved		*/
#define MGE_DEF 2			/* max gens without evaluation	*/
#define DPI_DEF	0			/* dump interval		*/
#define PGM_DEF 0			/* frequency of bitmap output	*/
#define OPT_DEF "an"			/* options			*/
#define RDS_DEF 123456789		/* random number seed		*/

	/* each member of the population has this form 			*/

typedef struct {
   	char   *Gene;			/* genotype */
	double *ObjVar;			/* object variables */
	int    *PerMut;			/* discrete permutation (if needed) */
	char   *MttGen;			/* mutation rate genotype */
	double *MttRts;			/* mutation rates, phenotype */
	double	SltPrb;			/* selection probability */
    	double 	Perf;			/* fitness */
    	int 	Needs_evaluation;	/* evaluation flag */
} STRUCTURE;
 
   	/* the best structures are saved in the following record 	*/

typedef struct {
    	char   *Gene;			/* genotype */
	double *ObjVar;			/* object variables */
	int    *PerMut;			/* discrete permutation (if needed) */
	char   *MttGen;			/* mutation rate genotype */
	double *MttRts;			/* mutation rates, phenotype */
    	double 	Perf;			/* fitness */
    	int 	Gen;			/* generation */
    	int 	Trials;			/* number of trials */
} BESTSTRUCT;

	/* different objective functions are described by the 		*/
	/* following record						*/

typedef struct {
	int 	dim;			/* dimension of the function	*/
	int	DimObl;			/* dimension obligate flag	*/
	int	MrkFct;			/* function characteristic marker */
	double	umin,			/* lower and upper bounds for	*/
		umax;			/* each object variable		*/
	double 	(*f)();			/* pointer to the function	*/
	char   *fnm;			/* filename of the function	*/
	char   *descr;			/* textual description*/
	double  CstVal[MOPT];		/* constant external parameters */
	char   *CstDsc[MOPT];		/* constant description */
	struct {
		int 	OVal;			/* integer option value */
		char   *ODsc[MOPT];		/* option description */
		char   *OTtl;			/* option title */
	} FctOpt[MOPT];			/* integer options and descriptions */
} FUNCTION;


typedef struct {
	int 	BufSiz;			/* actual size of Buffer */
	int	BufCol;			/* number of columns */
	char	BufFil[NSZ];		/* Filename for data */
	char  **BufFmt;			/* Format for output data */
	double *BufVal[ROWS];		/* Buffer data */
} BUFFER;
 
	/* An allele has converged if all but a FEW 			*/
	/* structures have the same value at that position.		*/

#define FEW        	(Popsize/20)

	/* Macro for index calculation in the objective functions	*/

#define	INDEX(i)	((i) * ChrLen)
 
	/* print a debugging message        				*/

#define Trace(s)  if (Traceflag) \
        { printf(s); printf("\n"); fflush(stdout);}
 
 
	/****************************************************************/
 	/*								*/
	/*     Rand computes a pseudo-random                		*/
	/*     double value between 0 and 1, excluding 1.  Randint    	*/
	/*     gives an integer value between low and high inclusive.   */
 	/*								*/
	/****************************************************************/

 
#define MASK ~(~0<<(INTSIZE-1))
#define PRIME 65539
#define SCALE 0.4656612875e-9
 
#define Rand()  (( Seed = ( (Seed * PRIME) & MASK) ) * SCALE )
 
#define Randint(low,high) ( (int) (low + (high-low+1) * Rand()))


	/****************************************************************/
 	/*								*/
	/* The following definitions could be used for another random	*/
	/* number generator (see even init.c for further points which	*/
	/* have to be changed in this case).				*/
	/* drand48() stems from the standard C library, while 		*/
	/* rnd_ri() as well as 						*/
	/* rnd_01d() can be found in the file xrand.c			*/
 	/*								*/
	/* extern double drand48();					*/
 	/* extern double rnd_01d();					*/
 	/* extern long   rnd_ri();					*/
 	/*								*/
	/* #define Rand() 	drand48()				*/
 	/* #define Rand()	rnd_01d()				*/
 	/* #define Randint(low,high) 					*/
	/*	  ( (int) (low + (high-low+1) * Rand()))		*/
 	/*								*/
	/****************************************************************/
 
/** end of file **/
