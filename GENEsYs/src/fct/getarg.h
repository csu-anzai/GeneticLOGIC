/*
 *	getarg.h	Ho, 05.Oct.'90				SunOS 4.0.3
 *			Ho, 30.Oct.'90
 *			Ho, 10.Dec.'90
 *			Ho, 21.Dec.'90
 *
 *	get program arguments for running KORR
 *
 *
 *	$Log: getarg.h,v $
 *	Revision 1.3  1991/02/22  19:23:40  iwan
 *	enforced new module naming conventions
 *
 */

#ifndef	__GETARG__
#define	__GETARG__	1		/* prevent multiple definitions */


#ifndef	__ARGLST__
#include "arglst.h"
#endif

#ifndef	__FTABLE__
#include "ftable.h"
#endif

#include <sys/types.h>
#include <sys/timeb.h>			/* --> time_t */


/*	global ESCaPaDE data		*/

typedef	struct	EscArg			/* program parameters */
{
	ArgLst_t	*ea_AppOpt;	/* application option list */

	struct	FctD	*ea_FctD;	/* -> selected objective function */

	double		 ea_RngTim;	/* RNG initialization time */
	double		 ea_MaxCpu;	/* maximum CPU time */
	int		 ea_MaxGen;	/* maximum generations */

	char		*ea_DtaDir;	/* name of data directory */
	int		 ea_ClrDir;	/* clear flag for data directory */

	int		 ea_RunCnt;	/* number of independent runs */
	int		 ea_RunPty;	/* running priority */
	int		 ea_verbose;	/* verbose output flag */
	int		 ea_query;	/* query data-dir name */
} EscArg_t;


typedef	struct	EscDta			/* dynamic/dependent data */
{
	time_t		 ed_StrTim;	/* start time */
	time_t		 ed_EndTim;	/* end   time */

	FILE		*ed_PgmLog;	/* program log file */
	int		 ed_OldPty;	/* initial priority */
	int		 ed_RngCnt;	/* RNG randomization count */

	int		 ed_MaxGen;	/* number of generations of last run */
	int		 ed_RunCnt;	/* number of last run */
	double		 ed_RunCpu;	/* CPU time of last run */
	double		 ed_SumCpu;	/* CPU time of all runs */
	double		 ed_InzCpu;	/* CPU time of ESCaPaDE init */
	double		 ed_TrmCpu;	/* CPU time of ESCaPaDE term */

	int		 ed_xLen;	/* length of initial vector */
	double		*ed_xInit;	/* initial starting vector */
	double		 ed_fInit;	/* corresponding objective value */
} EscDta_t;

extern	EscArg_t	_mpEscArg;	/* ESCaPaDE program parameters */
extern	EscDta_t	_mpEscDta;	/* ESCaPaDE dynamic data */


/*	global variables		*/

extern	char		*_mpPrdNam;	/* product name */
extern	char		*_mpPrdVer;	/* product version */

extern	char		*_mpPgmNam;	/* program name */
extern	char		*_mpBasNam;	/* program base name */

extern	char		*_mpPgmLog;	/* program log file name */
extern	char		*_mpExtLog;	/* program log file extension */
extern	char		*_mpExtDmp;	/* dump file extension: loose data */
extern	char		*_mpExtGrp;	/* dump file extension: grouped data */

extern	char		*_mpPgmDft;	/* default argument file name */
extern	char		*_mpExtDft;	/* default argument file ext. */
extern	char		*_mpExtFct;	/* fct.related option file ext. */

extern	char		*_mpDftVar;	/* env.var refering to option file */
extern	char		*_mpOptDmp;	/* file logging actual pgm options */

extern	char		*_mpHlpOpt;	/* help option indicators */
extern	char		*_mpFctDft;	/* default function name */


#endif	/* __GETARG__ */
