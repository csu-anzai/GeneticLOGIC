/*		UTILITY for the CFS-C Classifier System

This file, UTILITY.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file implement general purpose utilities
that may or may not be included in the various
C compiler packages available on various machines.

String manipulation functions:

	IsInt ( string ) IsUInt ( string ) IsFloat ( string )
	GetUInt ( charptr, &var, default, endchars, &err ) | Get value of type indicated,
	GetInt	( charptr, &var, default, endchars, &err ) | Search start at charptr until get endchars or	\0
	GetFloat( charptr, &var, default, endchars, &err ) | Store it in &var (use default if none found or err).
	GetSInt ( etc )
	GetLInt ( etc )
		The above 5 functions all set &err TRUE if string up to endchars is illegal type.
		Also, they all return char pointer to character just beyond endchar found, or to \0 if that was found.
	CopyChar( destin, source, maxchar )
	SpacStrg( destin, source, blocksize ) - copy string and insert spaces.
	StpTok1 ( ... ) |				
	StpTok2 ( ... ) |- Two ways to strip a token from a buffer.
	Str2Char( source-string, dest-char-array, len )
	EndTok			test for end-of-token character.

Some math functions for C's that don't provide them:
	Round() 
	toupper() tolower() -- for some compilers
Some pseudo-Random number generators:
	Rand01 ( )					return sample from uniform 0..1 random distribution 
	InitRand()
	UNoise( MaxDelta )			return random noise around 0, in specified range.
See UTILITY.DEF for
	URandN	( upperbound )		return random draw from 0 to upperbound.
	URandF	( UpperFloat )		return float between 0 to UpperFloat

Date routines:
	GetDate Get it from system
	GetCFSCTime Get it from system

**/

#include	"compiler.h"
#include	<math.h>
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC || DECSTATION )
#include	<ctype.h>
#endif
#include	<stdio.h>	   /* used by URand01() for opening a test file */

#include	"utility.h" 
#include	"core.h"

VOID	SpacStrg(), Str2Char(), GetDate(), GetTime(), InitRand();
double	atof();


/******************************

IsInt	return TRUE if string is [blanks] <int> \n, otherwise return FALSE
IsUInt	return TRUE if string is [blanks] <unsigned int> \n, otherwise return FALSE
IsFloat -- return TRUE if string is [blanks] <float> \n, otherwise return FALSE 

******/
int IsInt( s )
	char	s[];
{
	register int i;
	
	for ( i=0; s[i] == ' ' || s[i] == '\t'; i++) ;	/* skip blanks and tabs */
	if ( s[i] == '+' || s[i] == '-' ) ++i;			/* skip a sign */	
	for ( ; s[i] >= '0' && s[i] <= '9'; i++)	 ;	/* skip digits */
	
	if ( s[i] == '\0' )
		return( TRUE );

	return( FALSE );

} /* IsInt */


int IsUInt( s )
	char	s[];
{
	register	int	i;
	
	for ( i=0; s[i] == ' ' || s[i] == '\t'; i++)	;	/* skip blanks and tabs */
	for ( ; s[i] >= '0' && s[i] <= '9'; i++)		;	/* skip digits */
	
	if ( s[i] == '\0' )
		return( TRUE );

	return( FALSE );

}	/* IsUInt */


int IsFloat( s )
	char s[];
{
	register int i;
	
	for ( i=0; s[i] == ' ' || s[i] == '\t'; i++ )	;	/* skip white space */
	if ( s[i] == '+' || s[i] == '-' ) ++i;				/* skip a sign */
	for ( ; s[i] >= '0' && s[i] <= '9'; i++ )		;	/* skip int part */
	if ( s[i] == '.' )	++i;						;	/* skip a decimal point */
	for ( ; s[i] >= '0' && s[i] <= '9'; i++ )		;	/* skip decimal part */

	if ( s[i] == '\0' )
		return( TRUE );
		
	 return( FALSE );
	
} /* IsFloat	*/


/******************************

GetUInt		|	Search buffer for value of type indicated, store it, and
GetInt		|	return pointer to spot in buffer beyond value extracted.
GetFloat	|
GetSInt		|
GetLInt	 |

	Start		Where to start search.
	Result		Where to store value found.
	Default		Store if value not found (or if error).
	EndChars	Stop search when one of these (or \0) encountered.
	Err			Store FALSE here if all is ok, TRUE if value found is of wrong type.

	Return		Pointer to char. just after EndChar found, or to \0 if that found.

******/

char *GetUInt ( Start, Result, Default, EndChars, Err )
	char			*Start;
	unsigned int	*Result, Default;
	char			EndChars[];
	int 			*Err;
{
	char	tbuff[20], *StpTok2();

	*Err = FALSE;								/* Assume all will be ok. */	

	Start = StpTok2( Start, tbuff, sizeof(tbuff), EndChars );	 /* Get the ascii form into tbuff */

	if ( tbuff[0] == '\0' ) 					/* use default */
		*Result = Default;
	else {
		if ( IsUInt( tbuff ) )					/* if the value found is ok... */
			*Result = (unsigned int) atoi( tbuff );	 /* use atoi to convert it */
		else {								/* else the value found is not ok, so */
			*Result = Default;					/* use the default */
			*Err	= TRUE;
		}
	}

	return( Start );							/* Start is at char after EndChar (or a \0 */

} /* GetUInt */


char *GetInt ( Start, Result, Default, EndChars, Err )
	char	*Start;
	int		*Result, Default;
	char	EndChars[];
	int		*Err;
{
	char	tbuff[20], *StpTok2();

	*Err = FALSE;								 /* Assume all will be ok. */	

	Start = StpTok2( Start, tbuff, sizeof(tbuff), EndChars );	 /* Get the ascii form into tbuff */

	if ( tbuff[0] == '\0' ) 					 /* use default */
		*Result = Default;
	else {
		if ( IsInt( tbuff ) )					 /* if the value found is ok... */
			*Result = atoi( tbuff );			 /* use atoi to convert it */
		else {								 /* else the value found is not ok, so */
			*Result = Default;					 /* use the default */
			*Err	= TRUE;
		}
	}

	return( Start );							 /* Start is at char after EndChar (or a \0 */

} /* GetInt */


char *GetFloat ( Start, Result, Default, EndChars, Err )
	char	*Start;
	float	*Result, Default;
	char	EndChars[];
	int		*Err;
{
	char	tbuff[20], *StpTok2();

	*Err = FALSE;								 /* Assume all will be ok. */	

	Start = StpTok2( Start, tbuff, sizeof(tbuff), EndChars );	 /* Get the ascii form into tbuff */

	if ( tbuff[0] == '\0' ) 					 /* use default */
		*Result = Default;
	else {
		if ( IsFloat( tbuff ) ) 				 /* if the value found is ok... */
			*Result = atof( tbuff );			 /* use atoi to convert it */
		else {								 /* else the value found is not ok, so */
			*Result = Default;					 /* use the default */
			*Err	= TRUE;
		}
	}

	return( Start );							 /* Start is at char after EndChar (or a \0 */

} /* GetFloat */


char *GetSInt ( Start, Result, Default, EndChars, Err )
	char	*Start;
	short	*Result, Default;
	char	 EndChars[];
	int 	*Err;
{
	char	tbuff[20], *StpTok2();

	*Err = FALSE;								/* Assume all will be ok. */	

	Start = StpTok2( Start, tbuff, sizeof(tbuff), EndChars );	 /* Get the ascii form into tbuff */

	if ( tbuff[0] == '\0' ) 					/* use default */
		*Result = Default;
	else {
		if ( IsInt( tbuff ) )					/* if the value found is ok... */ 
			*Result = (short)  atoi( tbuff );	/* use atoi to convert it */
		else {								/* else the value found is not ok, so */
			*Result = Default;					/* use the default */
			*Err	= TRUE;
		}
	}

	return( Start );							 /* Start is at char after EndChar (or a \0 */

} /* GetSInt */


char *GetLInt ( Start, Result, Default, EndChars, Err )
	char	*Start;
	long int *Result, Default;
	char	 EndChars[];
	int 	*Err;
{
	char	tbuff[20], *StpTok2();
	long int li, atol();

	*Err = FALSE;								/* Assume all will be ok. */	

	Start = StpTok2( Start, tbuff, sizeof(tbuff), EndChars );	 /* Get the ascii form into tbuff */

	if ( tbuff[0] == '\0' ) 					/* use default */
		*Result = Default;
	else {
		if ( IsInt( tbuff ) )  {				/* if the value found is ok... */ 
			*Result = (long int)  atol( tbuff );	/* use atol to convert it */
		}
		else {								/* else the value found is not ok, so */
			*Result = Default;					/* use the default */
			*Err	= TRUE;
		}
	}

	return( Start );							 /* Start is at char after EndChar (or a \0 */

} /* GetLInt */


/******************************

CopyChar	Copy characters from Source to Destination string.

Copy up to (1) a \0 character in Source or (2) MaxCount characters have been moved.
DO NOT COPY THE \0 if there is one.

*****/

int CopyChar ( Destination, Source, MaxCount )
	char	*Destination, *Source;
	int		MaxCount;
{
	register int	count;
	
	for ( count = 0; count < MaxCount; ++count, ++Source, ++Destination )
		if ( *Source == '\0' )
			break;
		else
			*Destination = *Source;

	return( count );

} /* CopyChar */


/******************************

SpacStng	Copy characters from Source string to Destination string, inserting
			a blank every BlockSize characters.

	NOTE: This copies STRNGSZ characters - so there better be room.
**/

VOID SpacStrg ( Destin, Source, BlckSize )
	char	*Destin, *Source;
	int		BlckSize;
{
	register int	count;
	
	for ( count = 0; count < STRNGSZ; ++count, ++Destin )
		if ( count % BlckSize == 0 )
			*Destin = ' ';
		else
			*Destin = *Source++;

} /* SpacStrg */


/******************************

StpTok1			Version of MICROSOFT/ LATTICE C library function "stptok".

	InBuff		Source char array buffer terminated by \0.
	OutBuff		Destination char array buffer.
	OBuffLen	sizeof(OutBuff) so Token retrieved can be up to OBuffLen-1
	ETChars		Pointer to array of end-of-token characters. Note that \0 is ALWAYS considered end-of-token.

	Returns		Pointer to character just after last character read into OutBuff (the end-token character).
			
This places characters from InBuff into OutBuff, until
an End-of-Token character is reached or OBuffLen-1 characters have been transferred.
Finally a '\0' is placed after the last character copied to OutBuff.

NOTES:	1. Unlike LATTICE C stptok, this DOES ignore leading blanks.
		2. If no end-of-token is found before OBuffLen-1, a message is printed,
			characters are read (but not copied) until an end-of-token is found.

******/

char *StpTok1 ( InBuff, OutBuff, OBuffLen, ETChars )
	char	InBuff[], OutBuff[];
	int		OBuffLen;
	char	ETChars[];
{
	char	*cptr 	= InBuff;			/* start at beginning of InBuff */
	int		OBuffCnt = 0;				/* 0 characters into OutBuff so far */

	while ( *cptr == ' ') ++cptr;		/* strip blanks */		

		/* Read and transfer until end-of-token character or no more room.
			Note OBuffCnt must be < OBuffLen-1 since the arrays count from 0 and we must also leave room for a \0.
		*/

	while ( !EndTok(*cptr,ETChars) && OBuffCnt < OBuffLen-1 ) {
		OutBuff[OBuffCnt++] = *cptr;
		++cptr;
	}

	OutBuff[OBuffCnt] = '\0';		/* Tack on final \0	*/

		/* If we stopped because of no more room in OutBuff (OBuffCnt=maxlen-1) and the
			that character in InBuff is not a end-of-token character, then its an error,
			so read in until end-token.
		*/

	if ( OBuffCnt == OBuffLen-1 && !EndTok(*cptr,ETChars) )	{
		WriteStd("\nWARNING (StpTok1): a 'token' was too long. Check Your command or input.");
		while ( !EndTok( *cptr, ETChars) )
			cptr++;
	}

	return( cptr ); 	/* Return pointer to the end-of-token char. in InBuff. */

} /* StpTok1 */


/******************************

StpTok2 	Like StpTok1

	EXCEPT	StpTok2() returns pointer to character just after EndChar,
			unless the EndChar is '\0', in which case it returns a pointer to it.
			
******/

char *StpTok2 ( InBuff, OutBuff, OBuffLen, ETChars )
	char	InBuff[], OutBuff[];
	int		OBuffLen;
	char	ETChars[];
{
	char	*start, *StpTok1();

	start = StpTok1( InBuff, OutBuff, OBuffLen, ETChars );	 /* StpTok1 returns pointer to the EndChar */

	if ( *start == '\0' )					 /* If start is now at end of input string... */	
		return( start );					 /* return pointer to \0 */
											 /* else start is at the EndChar found, so... */
	return( ++start );						 /* return pointer to next char */

} /* StpTok2 */


/******************************

Str2char	Copy from null-terminated string to character array (but don't copy terminal null). 

	Source	null terminated string.
	Destin	char (or int) array, at least Len long.
	Len		length of Destin, so don't copy more!

	If Source is shorter than Len, Destin beyond last non-null is undefined.

******/
		
VOID Str2Char ( Source, Destin, Len )
	char	Source[];
	char	Destin[];
	int		Len;
{
	register int i;

	for ( i = 0; i < Len && Source[i] != '\0'; ++i )
		Destin[i] = Source[i];	

} /* Str2Char */


/******************************

EndTok		Test character to see if it is one of a given string of 'end-of-token' characters.

	Return TRUE if it is, FALSE otherwise.

NOTE that \0 is always considered an end-of-token character.

******/

int EndTok ( Char, EndChars )
	char	Char, EndChars[];
{
	char *eptr = &EndChars[0];

	do
		if ( *eptr == Char )
			return( TRUE );
	while ( *eptr++ != '\0' );

	return( FALSE );

} /* EndTok */



/******************************

Round		Rounds float to nearest integer.

	x 		Value (float) to round.
			If x >= 0.5, then take ceil(x) to round up.
			else	just use (int) form, since C truncates on float -> int converstion.

NOTE: This only works for x's that fit in 'int' variables (nothing larger).

******/

int Round ( x )
	double	x;
{
	register	int roundx;

	if ( x >= 0 ) { 
		if ( (x - (int) x) >= 0.5 )
			roundx = ceil( x );
		else
			roundx = x;
	}
	else { /* x < 0 */
		if ( (x - (int) x) <= -0.5 )
			roundx = floor( x );
		else
			roundx = x;
	}

	return( roundx );

} /* Round */

#if PERKINEL		/* some compilers missing these, or do them wrong! */
char toupper ( Letter )
	char	Letter;
{

	if ( Letter >= 'a' && Letter <= 'z' )
		Letter -= 32;

	return( Letter );

} /* toupper */

char tolower ( Letter )
	char	Letter;
{

	if ( Letter >= 'A' && Letter <= 'Z' )
		Letter += 32;

	return( Letter );

} /* tolower */
#endif	/* PERKINEL */


short int	   TestRand = 0;   /* Must be available even if code below is not compiled */
#ifdef THIS_IS_IN_CFSRAND.C
    /*  The random number generators are now in cfsrand.c file */

/**************************************************************

'Random' number generation subroutines.

URand01 	Returns (float) pseudo-random sample from uniform distribution over 0..1.
InitRand	Initialize random number generator if needed (called by Init_CFS() in CFSC.C).

URandN	  Returns unsigned int in range 0..Max: Macro in UTILITY.H, using URand01
URandF	  Returns float in range 0...Max.: Macro in UTILITY.H, using URand01
UNoise	  Returns uniform random float 'noise' s.t. -MaxDelta <= noise <= +MaxDelta.

The basic generator is URand01(), so you can fill in your own for that one.
You can also fill in InitRand() if need be for your generator.

The method I use is described in CACM, 31, #10, p1195 (1988).

*********/

#define RANDa 16807
#define RANDm 2147483647
#define RANDq 127773
#define RANDr 2836

#if (INTSZ == 16)		/* if neither of these, errors will get someone's attention */
long int URndSd = 1;
#else
#if (INTSZ == 32)
int URndSd = 1;
#endif
#endif


#if TESTRAND
	/*  Set TestRand to 1 to collect first 64K random numbers into a file CFSCRAND.TST 
		This code is compiled only of TESTRAND #defined to be 1 at compile time.
	*/

unsigned int	TestRandCount = 0; 
FILE			*TestRandFILE = NULL;
FILE			*GetFILE();
char			TestRandFName[10];
short int	   TestRandFileOpen = 0;   /* set to 1 when open */
short int	   TestRandFileDone = 0;   /* set to 1 when collected sample */
#define TESTRANDMX  50000	   /* you should run long enough to reach this, to explicity close the file */
extern char	 *SaveSyFN;	  /* use as root for TestRand file name */
extern char	 *GOutBuff;
extern unsigned int CycleStp;
#endif


float URand01()	 /***** the source of all the other numbers ****/
{
#if (INTSZ == 16)
	long int lo, hi, test;
#else
	int  lo, hi, test;
#endif
	float  f;

	hi = URndSd / RANDq;
	lo = URndSd % RANDq;
	test = (RANDa * lo) - (RANDr * hi);

	if ( test > 0 )
		URndSd = test;
	else
		URndSd = test + RANDm;

	f = 1.0 * URndSd / RANDm;

#if TESTRAND
	if ( TestRand ) {
		if ( TestRandCount < TESTRANDMX ) {
			if ( !TestRandFileOpen ) {

				strncpy( TestRandFName, SaveSyFN, 8 );
				TestRandFName[0] = 'R';
				if ( strlen( TestRandFName ) != 8 )
					TestRandFName[8] = '\0';
				strcat( TestRandFName, ".TST" );
				sprintf( GOutBuff, "\n\nTestRand: Saving URndSd values to file '%s'.\n\n", TestRandFName );
				WriteStd( GOutBuff );

				if ( (TestRandFILE = GetFILE( TestRandFName, "", "w" )) == NULL ) {
					sprintf( GOutBuff, "\nERROR (URand01, TestRand): Couldn't open '%s'!\n", TestRandFName );
					WriteStd( GOutBuff );
					exit(0);
				}
				TestRandFileOpen = TRUE;
			}

#if (INTSZ == 16 )
			if ( TestRandCount % 20 == 0 )
				fprintf( TestRandFILE, "\n%f", f );  /* "\n%5.5lu", URndSd ); */
			else
				fprintf( TestRandFILE, " %f", f );   /* " %5.5lu", URndSd );  */
#else
			if ( TestRandCount % 20 == 0 )
				fprintf( TestRandFILE, "\n %f", f ); /* "\n%5.5u", URndSd ); */
			else
				fprintf( TestRandFILE, " %f", f );   /* " %5.5u", URndSd ); */
#endif

			++TestRandCount;
		}
		
		else {  /* hit the max, so close the file */
			if ( !TestRandFileDone ) {
				CloseFILE( TestRandFName, "", TestRandFILE );
				TestRandFileDone = TRUE;
				sprintf( GOutBuff, "\n\nTestRand: Done at step %u; %u numbers in file '%s'.\n\n",
					 CycleStp, TestRandCount, TestRandFName );
				WriteStd( GOutBuff );
			}
		}
	}
#endif /* TESTRAND */

	return( f );
 
}   /* end URand01 */


/**************************

InitRand	Initialize random number generator if needed.

	Called from Init_CFS() in CFSC.C .

*******/

VOID	InitRand (  )
{

	/* Not needed for URand01() as implemented here */

}   /* end InitRand */

float UNoise ( MaxDelta )
	float	MaxDelta;
{
	float noise, sign;

	if ( URand01() >= 0.5 ) 				/* get the sign */
		sign = -1.0;
	else
		sign = 1.0;
	noise = URand01();						/* another uniform in 0..1 */
	noise *= (sign * MaxDelta); 			/* scale MaxDelta and add sign */

	return( noise );						/* In -MaxDelta <= noise <= +MaxDelta */

} /* UNoise */
#endif


/**************************************************

GetDate 	Get system date
GetTime 	Get system time

	Buff	Pointer to buffer into which time/date is to be placed.

	Form	Integer indicating form of output (GetTime only).

These functions get the current date or time from the system.
**/

#if LATTICEC
#if ( __HIGHC__ || DECSTATION )
#include <time.h>
time_t bintime;
#else
#include "dos.h"
#endif
#define SVC_DATE 0x2a		/* get date */
#define SVC_TIME 0x2c		/* get time */
#else
#if MPWC
#include	<OSUtils.h>
struct	DateTimeRec	LocalDateTime;
#else
#if MACLSC
#undef TRUE
#undef FALSE
#include	<OSUtil.h>
DateTimeRec	LocalDateTime;
#else
#include <time.h>
#if CBELLMTS
time_t bintime;
#else
long bintime;
#endif
#endif
#endif
#endif

VOID GetDate ( Buff )
	char *Buff;
{

#if LATTICEC
#if __HIGHC__
#else
	union REGS regs;
	regs.h.ah = SVC_DATE;
	intdos(&regs,&regs);		
	sprintf( Buff, "%02d/%02d/%02d", regs.h.dh, regs.h.dl, regs.x.cx-1900 );
#endif
#endif

#if CI86
	struct tm	*tmstr;	
	tmstr = localtim( NULL );
	sprintf( Buff, "%02d/%02d/%02d", tmstr->tm_mon+1, tmstr->tm_mday, tmstr->tm_year );
#endif

#if APOLLOC
	struct	tm *tmstr;
	bintime = time( NULL );
	tmstr = localtime( &bintime );
	sprintf( Buff, "%02d/%02d/%02d", tmstr->tm_mon+1, tmstr->tm_mday, tmstr->tm_year );
#endif

#if ( VAX || PERKINEL || SUN3 )
	struct	tm *tmstr;
	long		time();
	time( &bintime );
	tmstr = localtime( &bintime );
	sprintf( Buff, "%02d/%02d/%02d", tmstr->tm_mon+1, tmstr->tm_mday, tmstr->tm_year );
#endif

#if ( CBELLMTS || __HIGHC__ || DECSTATION )
	struct	tm *tmstr;
	time_t	bintine;
	time( &bintime );
	tmstr = localtime( &bintime );
	sprintf( Buff, "%02d/%02d/%02d", tmstr->tm_mon+1, tmstr->tm_mday, tmstr->tm_year );
#endif

#if	MPWC	
	GetTime( &LocalDateTime );
	sprintf( Buff, "%02d/%02d/%02d", LocalDateTime.month, LocalDateTime.day, LocalDateTime.year );
#endif

} /* GetDate */


VOID GetCFSCTime( Buff, Form )
	char	*Buff;
	int		Form;
{

#if LATTICEC
#if __HIGHC__
#else
	union REGS regs;
	regs.h.ah = SVC_TIME;
	intdos(&regs,&regs);
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", regs.h.ch, regs.h.cl );
	else
		sprintf( Buff, "%02d:%02d:%02d", regs.h.ch, regs.h.cl, regs.h.dh );
#endif
#endif

#if CI86
	struct tm *tmstr;
	tmstr = localtim( NULL );
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", tmstr->tm_hour, tmstr->tm_min );
	else
		sprintf( Buff, "%02d:%02d:%02d", tmstr->tm_hour, tmstr->tm_min, tmstr->tm_sec );
#endif

#if APOLLOC
	struct	tm *tmstr;
	bintime = time( NULL );
	tmstr = localtime( &bintime );
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", tmstr->tm_hour, tmstr->tm_min );
	else
		sprintf( Buff, "%02d:%02d:%02d", tmstr->tm_hour, tmstr->tm_min, tmstr->tm_sec );
#endif

#if ( VAX || PERKINEL || SUN3 )
	struct	tm *tmstr;
	long		time();
	time( &bintime );
	tmstr = localtime( &bintime );
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", tmstr->tm_hour, tmstr->tm_min );
	else
		sprintf( Buff, "%02d:%02d:%02d", tmstr->tm_hour, tmstr->tm_min, tmstr->tm_sec );
#endif

#if ( CBELLMTS || __HIGHC__ || DECSTATION )
	struct	tm *tmstr;
	time( &bintime );
	tmstr = localtime( &bintime );
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", tmstr->tm_hour, tmstr->tm_min );
	else
		sprintf( Buff, "%02d:%02d:%02d", tmstr->tm_hour, tmstr->tm_min, tmstr->tm_sec );
#endif

#if	MPWC
	GetTime( &LocalDateTime );
	if ( Form == 0 )
		sprintf( Buff, "%02d:%02d", LocalDateTime.hour, LocalDateTime.minute );
	else
		sprintf( Buff, "%02d:%02d:%02d", LocalDateTime.hour, LocalDateTime.minute, LocalDateTime.second );
#endif

} /* GetCFSCTime */

