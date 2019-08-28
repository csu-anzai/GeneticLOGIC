/*		USERCMD for the CFS-C Classifier System

This file, USERCMD.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file prompt user for commands, check the command
name against list of valid command names, and return integer
command code associated with the command (or with illegal command):

	GUserCmd	prompt user for command, check it out, get parameters into buffer.
	GetCmdCd	test command name and return code for command (or ERROR)

This file also subroutines to process SET commands:

	Set 		entry point for SET command.
	SetVar		set a specified variable to a specified value.
	FindRTVN	find node for variable being set in CVTable (see USERCMD.DEF).
	SetMuPr 	Set MuPrTot variable, and recalculate poisson distribution.
	SetDMShare  Set DMShare variablie, recalculate DMDscnt value.
	SetLog		set logging.
	SetGen		set recording of geneology.
	SetADInt	set auto-display interval variable.
	SetMLL		set message-list limit (NmIMsgMx and NmDMsgMx) variables.
	SetDCfL 	Setup Display Classifier List--list of classifiers to display.
	InDsCfL	 see if cf id is in list of ids.

**/

#include "compiler.h"
#include <math.h>
#include <stdio.h>
#if ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include <ctype.h>
#endif
#include "utility.h"
#include "core.ext"
#include "cfsio.ext"
#include "usercmd.def"
#include "dsclearn.h"

VOID Set(), SetADInt(), SetLog(), SetGen(), SetDemoL(), SetMLL(), SetDCfL();
double atof();
char *StpTok2();	/* UTILITY.C version of a microsoft c library function */
char *GetInt(), *GetSInt(), *GetUInt(), *GetFloat(), *GetLInt();

struct RTVarNd *FindRTVN();

#if MPWC
#define __SEG__ CFSCREAD
#endif


/*******************************

GUserCmd	Get a user command and return the command-code associated with that command.

Prompt user for a command, look it up in the command tables, and if it
is a valid command, return the associated code.
If the command is illegal, reprompt the user.

******/

int GUserCmd ( CmdPars )
	char	CmdPars[];
{
	int		CmdCode, CharCnt, ReadRet, LineLen;
	char	*lptr,								 /* pointer into LineBuff */
			Command[CMDNAMSZ+1]; 				 /* room for command name and \0 */

	WrtPrompt("C-? ");
	ReadRet = ReadS( GInBuff, CMDLINSZ, stdin, &LineLen );

	if ( EchoFlg )	{
		WriteStd( GInBuff );	
		WriteStd( "\n" );
	}

	if ( LineLen == 0 )
		CmdCode = NULL_CC;

	else {
		for ( lptr = GInBuff; *lptr != '\0'; ++lptr )
			if ( strncmp( lptr, "C-?", 3 ) == 0 )	/* strip prompt if use line-rentry to enter it in MPWC */
				break;
				
		if ( strncmp( lptr, "C-?", 3 ) == 0 )
			lptr += 3;
		else
			lptr = GInBuff;
			
		for ( ; *lptr == ' '; ++lptr )	;		/* skip leading blanks */
			
		for ( CharCnt = 0; *lptr != COMCHAR && *lptr != ' ' && *lptr != '\0' && CharCnt<=CMDNAMSZ; ++lptr, ++CharCnt )
				Command[CharCnt] = isupper( *lptr ) ? tolower( *lptr ) : *lptr;
		Command[CharCnt] = '\0';

			/* Get CmdCode, or null line command code or error command code. */

		if	( CharCnt == 0 )
			CmdCode = NULL_CC;

		else if ( (CmdCode = GetCmdCd( Command )) == ERROR )
			CmdCode = ERROR_CC;

		else {
			while ( *lptr == ' ')	++lptr; 			 /* skip blanks before parameter */
		
			for ( CharCnt = 0; *lptr != COMCHAR && *lptr != '\0' && CharCnt <= CMDPARSZ; ++CharCnt, ++lptr )
					CmdPars[CharCnt] = isupper( *lptr ) ? tolower( *lptr ) : *lptr;
			CmdPars[CharCnt] = '\0';
		}
	}

	return( CmdCode );

} /* GUserCmd */


/*******************************

GetCmdCd	Get CmdCode for a specified command.

	Command	String form of command entered by user.

	Return:	CmdCode for specified command, or ERROR if command not found.
**/

int GetCmdCd ( Command )
	char	Command[];
{
	int		ChNum;
	int		NodeNum = 0;
	char	*CharPtr;
	struct CmdNode	*cnptr = Commands;

	while ( NodeNum <= CCODEMX )	{
		ChNum = 0;										/* For first char. in test String. */
		CharPtr = cnptr->CmdStrng;			/* Point to first char. of string in table. */

		while ( *CharPtr == Command[ChNum] && Command[ChNum] != MINTABCH &&
					Command[ChNum] != '\0' && *CharPtr != '\0' ) {
			++CharPtr;
			++ChNum;
		}

			/* Why did we stop examining that command? */

		 if ( *CharPtr == MINTABCH || (Command[ChNum] == '\0' && *CharPtr == '\0') )
			break;
		else {
			++cnptr;	/* Didn't get it, so try the next entry in table. */
			++NodeNum;
		}

	}

	if ( NodeNum > CCODEMX )
		return( ERROR ) ;

	return( cnptr->CmdCode );
	
} /*	GetCmdCd */


/*******************************

Set 		Set one or more system variables, test-flags, etc.

	CmdPars	Null terminated string of the form:
				varname=value [ varname=value]*
			where each 'varname' is a SET-able system variable (see CVTable in USERCMD.DEF)
			and each associated 'value' is the value to assign to that variable.

Set() just picks off the 'varname=value' items and passes them to SetVar().

******/

VOID Set( CmdPars )
	char	CmdPars[];
{
	char	*lineptr = CmdPars;
	char	Par[PARNAMSZ+PARVALSZ+1];	/* room for par=val and \0 */

	while ( TRUE ) {
		lineptr = StpTok2( lineptr, Par, sizeof(Par), " ");

			/* If Par is NULL string, there are no more parameters so we are done parsing them.
				Otherwise, call SetVar to test and perhaps store the value.
			*/

		if ( Par[0] == '\0' )
			break;

		else
			SetVar( Par, 's' ); 				 /* 's' means this call from user-entered SET command */

	}

} /* Set */


/*******************************

SetVar			Set system variable.

	InLine		Null terminate string of the form:
					varname=value
				where 'varname' is the name of variable to be assigned a new value, and
					'value' is the new value.
				'varname' must be a CVName in the CVTable of settable variables (see USERCMD.DEF).
				The equal-sign may be replaced by one or more blanks.

	CallType	The type of calling function:
				's'	From user-entered SET command.
				'l'	From ReadVars() from LoadSys() -- respect Load flag.

	Return		OK		if value assigned.
				ERROR	if value not assigned (eg, no such variable, illegal type, out of range, etc.).

******/

int SetVar ( InLine, CallType )
	char	*InLine, CallType;
{
	char			varname[10], *valstart, *cp;
	struct RTVarNd	*rtv;	   /* point to node for variable being set */
	unsigned int	ui;		 /* scratch vars */
	int 			i, err;
	long int		li;
	float			f;
	
	valstart = StpTok2( InLine, varname, sizeof(varname), "= " );	/* get the variable name into varname */	
	while ( *valstart == ' ' || *valstart == '=' ) 				/* get valstart past the = and blanks, if any */
		++valstart;
	
	if ( (rtv = FindRTVN( varname )) == NULL ) {
		sprintf( GOutBuff, "\nVariable '%s' not found (InLine '%s').\n", varname, InLine );
		WriteStd( GOutBuff );
	}
	else {
		if ( strcmp( rtv->Name, "muprtot" ) == 0 )			/* first look for special variables */
			SetMuPr ( valstart );

		else if ( strcmp( rtv->Name, "dmshare" ) == 0 )
			SetDMSh ( valstart );
			
		else if ( strcmp( rtv->Name, "logfname" ) == 0 )
			SetLog ( valstart );
			
		else if ( strcmp( rtv->Name, "genfname" ) == 0 )
			SetGen ( valstart );

		else if ( strcmp( rtv->Name, "dscflst" ) == 0 )
			SetDCfL( valstart );

		else if ( strcmp( rtv->Name, "nmimsgmx" ) == 0 || strcmp( rtv->Name, "nmdmsgmx" ) == 0 )
			SetMLL ( rtv, valstart );
			
		else if ( strcmp( rtv->Name, "adcfint" ) == 0 || strcmp( rtv->Name, "adcmint" ) == 0 ||
				strcmp( rtv->Name, "addmint" ) == 0 || strcmp( rtv->Name, "adenvint" ) == 0 ||
				strcmp( rtv->Name, "advarint" ) == 0 || strcmp( rtv->Name, "adcycint" ) == 0 )
			SetADInt ( rtv, valstart );
			
		else if ( strcmp( rtv->Name, "urndsd" ) == 0 ) {
#if (INTSZ == 16)   /* then URndSd is declared as long int in UTILITY.C */
			cp = GetLInt( valstart, &li, 0L, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal long int value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else {
				*((long int *) rtv->Addr) = li;
			}
#else   
#if (INTSZ == 32)   /* else if its declared as int */
			cp = GetInt( valstart, &i, 0, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal int value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else
				*((int *) rtv->Addr) = i;
#else			   /* else its a mistake--should be INTSZ 16 or 32 */
			sprintf( GOutBuff, "\n\n***SetVar (USERCMD.C): URndSd not settable!!!\n\n" );
			WriteS( GOutBuff, fileptr );	/* to the log or save file, if on */
			printf( GOutBuff ); /* to the screen, so user sees it */
#endif		
#endif	
		}

		else if ( rtv->Type == 's' ) {
			cp = GetSInt( valstart, &i, 0, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal short value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else
				*((short *) rtv->Addr) = i;
		}
		
		else if ( rtv->Type == 'i' ) {
			cp = GetInt( valstart, &i, 0, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal int value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else
				*((int *) rtv->Addr) = i;
		}
		
		else if ( rtv->Type == 'u' ) {
			cp = GetUInt( valstart, &ui, 0, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal unsigned int value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else
				*((unsigned int *) rtv->Addr) = ui;
		}
		
		else if ( rtv->Type == 'f' ) {
			cp = GetFloat( valstart, &f, (float) 0.0, " ", &err );
			if ( err ) {
				sprintf( GOutBuff, "\nIllegal float value '%s'.\n", valstart );
				WriteStd( GOutBuff );
			}
			else
				*((float *) rtv->Addr) = f;
		}
		
		else if ( rtv->Type == 'c' ) {
			if ( strlen( valstart ) >= FNAMESZ ) {
				sprintf( GOutBuff, "\nchar value '%s' too long.\n", valstart );
				WriteStd( GOutBuff );
			}
			else {
				sprintf( GOutBuff, "\nSorry, can't set char variable '%s' now.\n", rtv->Name );
				WriteStd( GOutBuff );
			}
		}
		
		else {
			sprintf( GOutBuff, "\n**ERR SetVar: illegal Type (%c) in RTVarNd for '%s'!\n",
					rtv->Type, varname );
			WriteStd( GOutBuff );
		}
	}

	return( OK );

} /* SetVar */


/*******************************

FindRTVN		Find RTVarNd with Name or Alias equal to specified variable.

	Var 		char pointer to variable name (lowercase, null terminated).
	
	Return		Pointer to RTVarNd if found, else NULL.

******/

struct RTVarNd	*FindRTVN ( Var )
	char	*Var;
{
	struct RTVarNd *rtv;
	unsigned int	cnt;

	for ( rtv = RTVars, cnt = 0; cnt < RTVarsSz; ++rtv, ++cnt )
	{  	if ( strcmp( Var, rtv->Name ) == 0 ) {
			break;
        }
        else if ( rtv->Alias != '\0' ) {
            if ( strcmp( Var, rtv->Alias ) == 0 ) {
	    		break;
            }
        }
	} /* endfor */

	if ( cnt == RTVarsSz )	/* looked at them all and didn't find it! */
		rtv = NULL;

	return ( rtv );

} /* FindRTVN */


/*********************************

SetMuPr 	Set MuPrTot variable, and recalculate poisson distribution.

AValue		Ascii form of new value for MuPrTot.

*******/

int SetMuPr ( AValue )
	char	*AValue;
{
	int 			err;
	register int	i, j;
	register float	efactor, temp;
	float			value;
	extern float	MuPrTab[MUTABSZ];

	GetFloat( AValue, &value, (float) 0.0, " ", &err );
	
	if ( err ) {
		WriteStd( "\nMuPrTot must be a float!\n" );
		return( ERROR );
	}
	
	MuPrTot = value;

	if ( MuPrTot == (float) 0.0 )
		for ( i = 0; i < MUTABSZ; ++i )
			MuPrTab[i] = 1.0;

	else {
		efactor = 0.0 - value;
		efactor = pow( 2.7183, efactor );

		for ( i = 0; i < MUTABSZ; ++i ) {
			temp = i;
			MuPrTab[i] = efactor * pow( MuPrTot, temp );	/* get e to the lambda */

			if ( i > 1 )
				for ( j = i; j > 1; --j )					/* divide by i! */
					MuPrTab[i] /= 1.0 * j;
					
			if ( i >= 1 )
				MuPrTab[i] += MuPrTab[i-1]; 				/* make it cummulative */
		}
	}

	sprintf( GOutBuff, "\nFor MuPrTot %.3f, the Poisson distr. is:\n  i  Prob\n", MuPrTot );
	WriteStd( GOutBuff );
	
	for ( i = 0; i < 5; ++i ) {
		sprintf( GOutBuff, "  %d  %.4f\n", i, MuPrTab[i] ); 
		WriteStd( GOutBuff );
	}

	return( OK );

} /* SetMuPr */


/*********************************

SetDMSh 	Set DMShare variable, and recalculate DMDscnt value.

Value		Ascii form of new value for DMShare.

******/

int SetDMSh ( AValue )
	char	*AValue;
{
	int 	err;
	float	value;
	
	GetFloat( AValue, &value, (float) 0.0, " ", &err );
	
	if ( err ) {
		WriteStd( "\nDMShare must be a float!\n" );
		return( ERROR );
	}
	
	DMShare = value;

	DMDscnt = value - 1;

	sprintf( GOutBuff, "\nFor DMShare %.3f, DMDscnt is %.3f.\n", DMShare, DMDscnt );
	WriteStd( GOutBuff );

	return( OK );

} /* SetDMSh */


/*********************************

SetLog		Control echo-ing of all I/0 to/from user to a 'log' file.

AValue		'value' part of 'set log=value' comamnd. For Value:
				OFF 	then Set LogFlg = OFF (0).
				ON		then try to open LogFName. If ok, Set LogFlg = ON (1).
				file	try to open file 'file'. If ok, set LogFlg on and store that name in LogFName.

******/

VOID SetLog ( AValue )
	char			*AValue;
{
	char			filename[FNAMESZ];
	extern	short	LogFlg;
	extern	char	*LogFName;
	extern	FILE	*LogFILE;

	if ( strcmp( AValue, "off" ) == 0 ) {
		if ( !LogFlg )
			WriteStd("\nOutput not being logged.\n\n");
		else {
			LogFlg = OFF;
			if ( CloseFILE( LogFName, LogFName, LogFILE ) == FCLOSERR ) 
				WriteStd( "\nSetLog: *** BUG ***" );
			else {
				sprintf(GOutBuff,"\nLogging on '%s' terminated.\n\n", LogFName);
				WriteStd( GOutBuff );
			}
		}
	}

	else if ( LogFlg )	{
		sprintf(GOutBuff,"\nOutput already being logged on '%s'.\n", LogFName);
		WriteStd( GOutBuff );
	}
		
	else {
		if ( strcmp( AValue, "on" ) == 0 )
			strcpy( filename, LogFName );
		else
			strcpy( filename, AValue );

		if ( (LogFILE = GetFILE( filename, "", WRITEMODE) ) == NULL ) {
			sprintf(GOutBuff,"\nCan't open '%s' for logging.\n\n", filename );
			WriteStd( GOutBuff );
		}	
		else {
			LogFlg = ON;
			strcpy( LogFName, filename );
			sprintf(GOutBuff,"\nLogging output on '%s'.\n\n", LogFName );
			WriteStd( GOutBuff );
		}
	}

} /* SetLog */


/*********************************

SetGen		Control writing of 'geneology' information to a file.

AValue		'value' part of 'set gen=value' comamnd. For Value:
				OFF 	then Set GenFlg = OFF (0).
				ON		then try to open GenFName. If ok, Set GenFlg = ON (1).
				file	try to open file 'file'. If ok, set GenFlg on and store that name in GenFName.
******/

VOID SetGen ( AValue ) 
	char	*AValue;
{
	char			filename[FNAMESZ];
	extern	short	GenFlg;
	extern	char	*GenFName;
	extern	FILE	*GenFILE;

	if ( strcmp( AValue, "off" ) == 0 ) {
		if ( !GenFlg )
			WriteStd("\nGeneology not being saved.\n");
		else {
			GenFlg = OFF;
			CloseFILE( GenFName, GenFName, GenFILE );
			sprintf(GOutBuff,"\nGeneology record on '%s' terminated.\n", GenFName);
			WriteStd( GOutBuff );
		}
	}

	else if ( GenFlg )	{
		sprintf(GOutBuff,"\nGeneology already being written on '%s'.\n", GenFName);
		WriteStd( GOutBuff );
	}
		
	else {
		if ( strcmp( AValue, "on" ) == 0 )
			strcpy( filename, GenFName );
		 else
			strcpy( filename, AValue );

		if ( (GenFILE = GetFILE( filename, "", WRITEMODE) ) == NULL ) {
			sprintf(GOutBuff,"\nCan't open for geneology.\n", filename );
			WriteStd( GOutBuff );
		}	
		else {
			GenFlg = ON;
			strcpy( GenFName, filename );
			sprintf(GOutBuff,"\nGeneology to be written on '%s'.\n", GenFName );
			WriteStd( GOutBuff );
		}
	}

} /* SetGen */


/*********************************

SetADInt	Setting an ADxxxInt (auto-display interval) variable.

NodePtr 	Pointer to RTVars entry for the variable.

AValue		Ascii form of value to store.

If the variable is being autodisplayed now (stored value is not ADOFF), and
the value to store is 0 or ADOFF, decrement AutoDspl--one fewer item is being displayed.
If the value stored is ADOFF and the new value is not that or 0, increment AutoDspl.

Also, if the new value is 0 (turn off the auto-display), store ADOFF instead.
(0 won't work with the mod (%) test in ADisplay.)

******/

VOID SetADInt ( NodePtr, AValue )
	struct	RTVarNd 	*NodePtr;
	char				*AValue;
{
	int 				err;
	unsigned int		value;
	extern unsigned int AutoDspl;

	GetUInt( AValue, &value, 0, " ", &err );

	if ( err ) {
		WriteStd( "\nMust be an unsigned int.\n" );
		return;
	}

	if ( *((unsigned int *) NodePtr->Addr) == ADOFF && ( value != ADOFF || value != 0 ) )
		++AutoDspl;
	else if ( *((unsigned int *) NodePtr->Addr) != ADOFF && ( value == ADOFF || value == 0 ) )
		--AutoDspl;

	if ( value == 0 )
		value = ADOFF;

	*((unsigned int *) NodePtr->Addr) = value;
	
} /* SetADInt */


/*********************************

SetMLL		Set message-list limits:
				NmIMsgMx - Maximum number of new (internal) messages.
				NmDMsgMx - Share of message-list reserved for detector messages.

NodePtr 	Pointer to CVUTable entry for the NmIMsgMx or NmDMsgMx variable.

NewValue	Value to store.

A share of the message-list (MSGLSTSZ messages max.) is reserved for detector messages,
namely DmDMsgMx messages. The rest of the list can be used for new messages generated
'internally', i.e., by classifiers. So, at all times:
NmIMsgMx + NmDMsgMx = MSGLSTSZ	
Thus changing one limit affects the other.

******/

VOID SetMLL ( NodePtr, AValue )
	struct	RTVarNd 	*NodePtr;
	char				*AValue;
{
	int 				err;
	unsigned int		value;
	extern unsigned int NmIMsgMx, NmDMsgMx;
	
	GetUInt( AValue, &value, 0, " ", &err );

	if ( err ) {
		WriteStd( "\nMust be an unsigned int.\n" );
		return;
	}

	if ( value > MSGLSTSZ ) {
		WriteStd("\nCan't set > msglist size.\n");
		return;
	}

	if ( strcmp( NodePtr->Name, "nmimsgmx" ) == 0 ) {
		NmIMsgMx = value;
		NmDMsgMx = MSGLSTSZ - NmIMsgMx;
	}
	else {
		NmDMsgMx = value;
		NmIMsgMx = MSGLSTSZ - NmDMsgMx;
	}

	sprintf(GOutBuff, "\nMax number of msgs is:  %u", MSGLSTSZ );
	WriteStd( GOutBuff );
	sprintf(GOutBuff, "\nReserved for detectors: %u", NmDMsgMx );
	WriteStd( GOutBuff );
	sprintf(GOutBuff, "\nMax internal messages:  %u\n", NmIMsgMx );
	WriteStd( GOutBuff );

}  /* SetMLL */


/******************************

SetDCfL 	Setup Display Classifier List--list of classifiers to display.

Cflist		List of classifiers that are to be displayed:	<id1>,<id2>, ...
			or, it is a letter that specifies a special subset designators:
				a - all with Strength > Average
				b - all bidders on most recent step
				w - all winners on most recent step
				p - all cfs that "post" new messages (after effector resolution)

Store the Id values, sorted from low id to high, in the array DCfLIds[]
(Max number is DSCFLMX). Store USINTMAX in entry to signal end of list.
This list will be used whenever classifiers are displayed--only
classifiers on this list will be displayed until the DsCfLFlg is set to '0' (off).

*********/

VOID SetDCfL ( Cflist )
	char	Cflist[];
{
	int 	err, i, id, idindex, numids;
	char	*nxtchar, *GetUInt();

	for ( idindex = 0; idindex <= DSCFLMX; ++idindex )
		DsCfLIds[idindex] = INTMAX;

	if ( strcmp( Cflist, "off" ) == 0 || *Cflist == '0' ) /* turn it off */
		DsCfLst = '0';
	else if ( *Cflist == 'a' )
		DsCfLst = 'a';
	else if ( *Cflist == 'b' )
		DsCfLst = 'b';
	else if ( *Cflist == 'w' )
		DsCfLst = 'w';
	else if ( *Cflist == 'p' )
		DsCfLst = 'p';
	else {
		DsCfLst = '1';
		for ( numids = 0, nxtchar = Cflist; numids <= DSCFLMX;	) {
			if ( *nxtchar == '\0' ) 
				break;
			nxtchar = GetUInt( nxtchar, &id, 9999, ",", &err );
			if ( err )
				WriteStd( "\nWarning: illegal Cf_Id in list ignored.\n" );
			else {
				for ( idindex = 0; DsCfLIds[idindex] < id; ++idindex )	 /* idindex points to place to put it */
					;
				for ( i = numids; i > idindex; --i )	/* move the rest down one */
					DsCfLIds[i] = DsCfLIds[i-1];
				DsCfLIds[idindex] = id; 				/* store the latest one */
				++numids;
			}
		}
	}

} /* SetDCfL */


/******************************

InDsCfL 	See if Cd_Id is in list of ones to be displayed, DsCfLIds[], which
			is constructed by the SET DSCFLST command (see SetDsCfL() above).

	Id		Id to look for.

	Return	TRUE if its in the list, else false.

*******/

int InDsCfL ( Id )
	unsigned int Id;
{
	register int	i;

	for ( i = 0; DsCfLIds[i] != INTMAX && i <= DSCFLMX; ++i )	/* INTMAX means end of list */
		if ( DsCfLIds[i] > Id )
			break;												/* list is sorted low to high */
		else if ( DsCfLIds[i] == Id )
			return( TRUE );

	return( FALSE );

} /* InDsCfL */

