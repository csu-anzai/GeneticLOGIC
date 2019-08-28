/*		CFSIO for the CFS-C Classifier System

This file, CFSIO.C, is part of the CFS-C classifier system	(Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file are called to do (almost) all I/O.
The subroutines:

	GetFILE	( FileName, default, How )		return FILE pointer or NULL for error.	
	CloseFILE ( FileName, default, fileptr )	close the file (NOT stdin,stdout,stderr, or console.)

	ReadS( buffer, bufflen, source, readlen )	Read a line from 'source' into 'buffer'.

	WriteStd( String )							write contents of String via printf to stdout.
												If LogFlg is ON, also write it to the LogFILE device.

	WrtPrompt( String ) 						write prompt to appropriate field/line.

	WriteS( String, fileptr )					write contents of String to designated file/device.
												If LogFlg is ON, also write it to the LogFILE device.

	WriteGen( String )							Write to 'geneology' file
*/

#include	"compiler.h"
#if MPWC
#define __SEG__ CFSCREAD
#endif

#include	<stdio.h>

#if ( LATTICEC || CBELLMTS || SUN3	|| MPWC )
#include	<ctype.h>
#endif

#if LATTICEC
#if !__HIGHC__
#include	"dos.h"
extern short _OSERR;
#endif
#endif

#if MPWC
#include <Files.h>
#endif

#include	"utility.h" 
#include	"cfsio.def"

extern short int EchoFlg;

VOID WriteStd(), WrtPrompt(), WriteS();


/****************************

GetFILE		return FILE pointer, or NULL if file can't be opened and returned.

	FileName	name of file to open. NULL means use default. 
	Default		name of default file.
	How			how to open, "r", "w", etc.

******/

FILE *GetFILE ( FileName, Default, How )
	char		*FileName, *Default, *How;
{
	FILE		*fileptr;
	char		*fname = FileName;
#if MPWC
	short		vRefNum;
	long int	bytes;
	FInfo		fileinfo;
	int			err;
#endif

	while ( *fname == ' ' )		++fname;		/* Strip blanks. */

	if	( *fname == '\0' )					
		fname = Default;						/* Use the default supplied */	

	if ( strcmp( fname, SCREENNAME ) == 0 )
		fileptr = stdout;
	else if ( strcmp( fname, KEYBOARDNAME ) == 0 )
		fileptr = stdin;
	else 
	{	if ( (fileptr = fopen( fname, How )) == NULL )
		{	sprintf(GOutBuff,"\nWARNING (GetFILE): can't open file '%s'.\n", fname );
			WriteStd( GOutBuff );
			fileptr = NULL;
		}
#if THIS_MPWC
		else if ( How != READMODE )
		{	if ( (err = GetVInfo( 0, GInBuff, &vRefNum, &bytes )) != noErr )	/* 0 means default volume */
			{	sprintf( GOutBuff, "WARNING (GetFILE): GetVInfo err %d!", err );
				WriteStd( GOutBuff );
				return( fileptr );
			}
			if ( (err = GetFInfo( FileName, vRefNum, &fileinfo )) != noErr )
			{	sprintf( GOutBuff, "WARNING (GetFILE): GetFInfo err %d, VolName '%8.8s', FileName '%s', vRefNum %d!", err, GInBuff, FileName, vRefNum );
				WriteStd( GOutBuff );
				return( fileptr );
			}
			strncpy( fileinfo.fdType, "TEXT", 4 );
			if ( (err = SetFInfo( FileName, vRefNum, fileinfo )) != noErr )
			{	sprintf( GOutBuff, "WARNING (GetFILE): SetFInfo err %d, FileName '%s', vRefNum %d!", err, FileName, vRefNum );
				WriteStd( GOutBuff );
				return( fileptr );
			}
		}
#endif
	}

	return( fileptr );

} /* GetFILE */


/******************************

CloseFILE	close the file, unless its stdin, stdout, stderr, or the CONSOLE.

	Entry:	FileName - name of file to close.
			Default  - name of file if FileName is \0.
			FilePtr  - FILE pointer for file.
	Return:	OK if all ok, ERROR if an error.

******/

int CloseFILE ( FileName, Default, FilePtr )
	char	*FileName, *Default;
	FILE	*FilePtr;
{
	char	*fname = FileName;

	if ( *fname == '\0' )
		fname = Default;

		/* printf("\nClose fname '%s', FilePtr '%d'", fname, FilePtr ); */

	if ( FilePtr==stdin || FilePtr==stdout || FilePtr==stderr ) 
		return( OK );
	else if ( fclose( FilePtr ) == 0 )
		return( OK );
	else {
		sprintf( GOutBuff, "\nERR (CloseFILE)-couldn't close file '%s'.\n",fname);
		WriteStd( GOutBuff );
#if LATTICEC
#if !__HIGHC__
		sprintf( GOutBuff, "  (_OSERR is %d)\n", _OSERR );
		WriteStd( GOutBuff );
#endif
#endif
		return( ERROR );
	} 	

#if LATTICEC
	return( OK );
#endif
} /* CloseFILE */


/****************************

ReadS		Read a string from the designated input Source and puts it in
			the character array 'LineBuff'.

	ReadS reads in all characters until a newline (\n) is encountered.
	A /0 is placed after the last character. 

	Note that this also recognizes backspace, i.e., it backsup over characters
	already entered and writes over them.

	LineBuff	pointer to first character of array. 
	BufLen		the length of LineBuff, so ReadS places only the first BufLen-1 characters
				read into LineBuff, not including the newline, to leave room for the /0 .
	Source		FILE pointer to source to be read.
	RetLen		Set to number of charaters stored in LineBuff (not including the /0).
	
	Return:		TRUE if all is ok. 
				ERROR if source not open or unexpected EOF encountered.
				EOF if end-of-file encountered.
******/

int ReadS( LineBuff, BufLen, Source, RetLen )
	char	LineBuff[];
	int		BufLen;
	FILE	*Source;
	int		*RetLen;
{
	register int	InChar;			 /* This is int so EOF is detectable */
	register int	Count = 0;
	int				RetValue;

		/* Read in until get a newline or EOF, but only store characters as long as
			there is room (up to BufLen-2, to save room for the \0). 
		*/

	while ( (InChar=getc(Source)) != EOF && InChar != '\n' && InChar != CR )
		if ( InChar == BACKSPAC && Count > 0 )
			--Count;
		else if ( Count <= BufLen-2 )
			LineBuff[Count++] = InChar;

	LineBuff[Count] = '\0'; 				/* Append the \0 after the last character stored. */
	*RetLen = Count;						/* Return number stored (remember arrays from 0) */

		/* Now see why we stopped reading, and return appropriate value. */

	if	( ( InChar == '\n' || InChar == CR ) && Count <= BufLen-1 ) 
		RetValue = TRUE;
	else if ( InChar == EOF )
		RetValue = EOF;
	else { 
		sprintf( GOutBuff, "\nERR in ReadS: input line too long (>%d) for buffer?", Count);
		WriteStd( GOutBuff );
		RetValue = ERROR;
	} 

/*	 if ( LogFlg )	fprintf( LogFILE, "%s", LineBuff ); */

	return(RetValue);

} /* ReadS */

#if MPWC
#define __SEG__ CFSCWRITE
#endif


/****************************

WriteStd	Write String contents via printf to stdout.
			If LogFlg is ON, also write it to the LogFILE device.

	Strng	Pre-formatted and null terminated char string.

	NOTE:	GOutBuff is a Global Output Buffer as buffer for formatting prior to call to this.)

******/

VOID WriteStd ( Strng )
	char	*Strng;
{

	fprintf( StdOFILE, "%s", Strng );
#if ( CBELLMTS || MPWC )
	if ( !EchoFlg )
		fflush( StdOFILE );
#endif

	if ( LogFlg ) {
		fprintf( LogFILE, "%s", Strng );
	}

} /* WriteStd */


/****************************

WrtPrompt	Write prompt to appropriate field/line (Useful for special displays).

******/

VOID WrtPrompt ( Strng )
	char	*Strng;
{

	WriteStd( Strng );

} /* WrtPrompt */


/****************************

WriteS		Write String contents to specified file/device.

	Strng	 Pre-formatted and null terminated char string.

	FilePtr	Pre-opened file/device pointer.

	NOTES:	1. If LogFlg is ON, also write it to the LogFILE device.

			2. If FilePtr == StdOFile, call WriteStd to do the write and
				don't write to LogFILE here. This is necessary to ensure that all
				output that normally goes to 'stdout' goes through one subroutine, WriteStd(),
				which makes it route that output to one part of the screen,
				to a particular 'window', etc. (At least that's the theory...)
	
			3. GOutBuff is a Global Output Buffer as buffer for formatting prior to call to this.)
******/

VOID WriteS ( Strng, FilePtr )
	char	*Strng;
	FILE	*FilePtr;
{

	if ( FilePtr == StdOFILE )
		WriteStd( Strng );
	else {
		fprintf( FilePtr, "%s", Strng );

		if ( LogFlg ) {
			fprintf( LogFILE, "%s", Strng );
        }
	}

} /* WriteS */


/****************************

WriteGen	Write to 'geneology' file.

	Strng	Null terminated string to write.

******/

VOID WriteGen ( Strng )
	char	*Strng;
{

	fprintf( GenFILE, "%s", Strng );

} /* WriteGen */
