/* $Id: error.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $  */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : error.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_ERROR_H
#define TSP_ERROR_H


/* error numbers */

#define ERR_NO			0
#define ERR_HELP		1
#define ERR_QUERY		2
#define ERR_OUT_OF_MEMORY	3
#define ERR_MAKE_DIR		4
#define ERR_FILE_OPEN		5
#define ERR_FILE_SYNTAX		6
#define ERR_FILE_TYPE		7
#define ERR_FILE_DIMENSION	8
#define ERR_FILE_INDEX		9
#define ERR_FILE_DIMINDEX	10
#define ERR_TOUR_TOWN		11
#define ERR_FILE_NAME		12
#define ERR_UNEXPECTED_EOF	13
#define ERR_OPTION		14
#define ERR_BRSELECT		15
#define ERR_CROSSRATE		16
#define ERR_MUTDIM		17
#define ERR_ELITE		18
#define ERR_FITSCALE		19
#define ERR_GAPSIZE		20
#define ERR_ELITEHOLD		21
#define ERR_POPINIT		22
#define ERR_ELSELECT		23
#define ERR_MUTATION		24
#define ERR_NORMNUM		25
#define ERR_POPSIZE		26
#define ERR_CROWFACTOR		27
#define ERR_REPLACE		28
#define ERR_MASELECT		29
#define ERR_OPTLOCAL		30
#define ERR_WINDOWSIZE		31
#define ERR_CROSSOVER		32
#define ERR_MUTRATE		33
#define ERR_FITB		34
#define ERR_FITD		35
#define ERR_CHNLEN		36
#define ERR_LOWERINDEX		37
#define ERR_ETAMAX		38
#define ERR_WHITLEY		39
#define ERR_CTRLPARAM		40
#define ERR_UPPERINDEX		41
#define ERR_CROSSINT		42
#define ERR_GRINDEX		43
#define ERR_COMMINT		44
#define ERR_PARMODEL		45
#define ERR_INDNUM		46
#define ERR_POPNUM		47
#define ERR_PROCNUM		48
#define ERR_POLLENDIR		49
#define ERR_TOPOLOGY		50
#define ERR_LOWERWINDFORCE	51
#define ERR_UPPERWINDFORCE	52
#define ERR_MAKELINK		53
#define ERR_GRWINDFORCE		54
#define ERR_PGMFREQ		55
#define ERR_TOTEXP		56
#define ERR_DUMPFREQ		57
#define ERR_TOTGEN		58
#define ERR_MINQUAL		59
#define ERR_SAVESIZE		60
#define ERR_RNDTYPE		61
#define ERR_TOTTRIALS		62
#define ERR_POPDISPLAY		63
#define ERR_INTERVAL		64
#define ERR_MAXSPIN		65
#define ERR_RANDOM		66
#define ERR_DIV_BY_ZERO		67
#define ERR_VARVAL		68
#define ERR_SCHEMA_INDEX	69
#define ERR_SYSCONF		70


#ifdef USE_PROTO
extern void critical_error(int, char *);
#else
extern void critical_error();
#endif


#endif


/*** end of file ***/
