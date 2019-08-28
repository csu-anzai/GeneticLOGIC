/*		CFMSGIO for the CFS-C Classifier System.

This file, CFMSGIO.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

This file contains subroutines to read-in and display the classifier-list and the message-list from 
user-specified files (or from default files if no files are specified by the user), devices, etc.
These functions do ALL their I/O by calling functions defined in the system I/O source file, CFSIO.C,
i.e., the ReadS(), WriteStd(), and WriteS() functions.

The subroutines in this file:

	LoadMsgs	 open file, load messages from it, close file (if closeable).
	ReadMsgs	load messages from opened file.
	StoreMsg	store message from buffer into a given MsgNode structure.
	GtNxtMNd	get pointer to next available MsgNode in specified list, increment appropriate counts.
	EmptyML 	empty the specified message list.

	DsplMsgs	open file/device, write current messages to it, close file/device (if closeable).
	WrtMsgs 	write current messages to an opened file/device.
	PutMNd		write MsgNode to buffer in specified format.

	LoadCfs 	open file, load classifiers from it, close file (if closeable).
	ReadCfs 	read classifiers from open file and call StoreCf for each one.
	StoreCf 	store classifier as specified in buffer in a given CfNode.
	AddCf		Add classifier to classifier list.

	DsplCfs 	Display classifiers on some file/device
	WrtCfs		write to some opened file/device.
	PutCNd		write CfNode contents to specified buffer in specified format.
**/

#include	"compiler.h"
#include	<math.h>
#include	<stdio.h>
#include	"utility.h"
#include	"cfsio.ext"
#include	"core.ext"
#include	"cfops.ext"

extern char *StpTok2();
extern float UNoise();

	/* Some variables used by WriteCf and PutCNd to accumulate totals while displaying classifiers. */

static float WCtotstr, WCtotchng, WCtotbr;	
static int	WCtotbid, WCtotmat, WCtotprd, WCtotpst, WCtoteff, WCtotpos, WCtotneg, WCtotofs;

char	StCfEMsg[17] = "\nERR (StoreCf),";	/* Part of error message used in StoreCf() */

VOID	LoadMsgs(), ReadMsgs(), EmptyML(), WrtMsgs(), PutMNd(), 
	LoadCfs(), ReadCfs(), PutCNd(), WrtCfs();

#if MPWC
#define __SEG__ CFSCREAD
#endif

/****************************

LoadMsgs		Read messages into the system from a file/device.

	LoadMsgs opens the specified file, calls ReadMsgs to do the work, and closes the file.
			
	FDName:		Name of 'file' from which to read messages
				("" means read from current default file MsgInFN (see CFSIO.DEF).
	How:		How to add messages to the system:
				'r' - replace the current messages with the ones read in.
				'a' - append the messages read in to the current ones.

	List: 		Messages list to which messages are to be added:
				'c' - current message list.
				'o' - old message list.
				'd' - current message list, but add as messages from detectors.

******/

VOID LoadMsgs ( FDName, How, List )
	char *FDName, How, List;
{
	FILE	*fptr;

	if ( List != 'c' && List != 'o' ) {
		sprintf(GOutBuff,"\n\nERR (LoadMsgs): illegal List '%c'.\n", List );
		WriteStd( GOutBuff );
	}
	else if ( (fptr = GetFILE( FDName, MsgInFN, "r" )) == NULL )
		WriteStd("--No messages Loaded.\n");
	else {
		ReadMsgs( fptr, How, List );
		CloseFILE( FDName, MsgInFN, fptr );
	}

} /* LoadMsgs	 */


/****************************

ReadMsgs	Read messages from opened file/device into specified list.

	ReadMsgs parameters are as for LoadMsgs, except FilePtr is FILE pointer to open file. 
	The file/device should consist of lines of input-messages (or comments) followed
	by a line with just the string "ENDMSGS" to indicate the end of the message to add.

	See StoreMsg for the format of each input-message line.

******/

VOID ReadMsgs ( FilePtr, How, List )
	FILE	*FilePtr;
	char	How, List;
{
	int		loadcnt = 0;
	int		retlen;			
	char	*rchar, *wchar;

	if ( How == 'r' )		EmptyML( List );		/* If How is 'r' (Replace), empty list. */

		/* Read messages from input, append the OK ones. Stop on end-of-file or too many messages. */
		
	while ( (ReadS( GInBuff, GIBUFFSZ-1, FilePtr, &retlen) != EOF) && retlen > 0 && NmCMsgs <= NmIMsgMx ) {
		if ( *GInBuff == COMCHAR )										/* ignore comments */
			continue;

		else if ( strcmp( GInBuff, "ENDMSGS" ) == 0 )					/* thats all, folks */
			break;

		else {
			for ( wchar = rchar = GInBuff; *rchar != '\0'; ++rchar )	/* strip out all blanks now */
				if ( *rchar != ' ' )	*wchar++ = *rchar;
			*wchar = '\0';
			if ( StoreMsg( GInBuff, List ) != ERROR )					/* try to store message. */
				++loadcnt;												/* increment count if no error */
		}

	}

	if ( strcmp( GInBuff, "ENDMSGS" ) != 0 )							/* Look for end-of-data marker */
		while ( ReadS( GInBuff, GIBUFFSZ-1, FilePtr, &retlen) != EOF )
			if ( strcmp( GInBuff, "ENDMSGS" ) == 0 ) break;

	sprintf( GOutBuff, "\nLoaded %u Msgs.\n", loadcnt );
	WriteStd( GOutBuff );
	
} /* ReadMsgs	 */

/****************************

StoreMsg		Read in and store message.

	LineBuff	String of the form:
					message-string, id, intensity, producer-id, input1, input2
				where
					message-string	string of 0's and 1's
					id				Msg_Id to use for message (default is next id available).
					intensity 		message intensity (default is MsgIntDf system variable)
						(NOTE: the following 3 item come in after a SAVE command, and the
							id's are resolved to pointers in the startup process in CFSC.C)
					producer-id		Cf_Id of producer (default is 0 for none )
					input1			Msg_Id of first message used by producer (default is 0 for none)
					input2			Msg_Id of second message used by prodcuer (default is 0 for none)
	List		List to which message it to be added:
					'c' - CurMsgs 
					'n' - NewMsgs (from a classifier)
					'o' - OldMsgs
					'd' - CurMsgs, with DetMsg flag set TRUE.

	Return		OK	if message stored successfully.
				ERROR if something goes wrong and message not stored.

******/

int StoreMsg( LineBuff, List )
	char	LineBuff[], List;
{
	unsigned int	 id, producer, input1, input2;
	float			 intensit, UNoise();	
	int 			 err;
	char			*lbufptr, tbuff[MSGBUFSZ], *GetUInt(), *GetFloat(), *GetSInt(); 
	struct MsgNode	*newmsg, *GtNxtMNd();
	extern int			LastCMsg;
	extern unsigned int NmCMsgs, NmOMsgs;
		
		/* Read in the message string and check the characters. */

	lbufptr = LineBuff;
	lbufptr = StpTok2( lbufptr, &tbuff[0], STRNGSZ+1, ", ");
	if ( !IsMessage( tbuff ) )	{
		sprintf(GOutBuff,"\nStoreMsg: Bad msg in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
		return(ERROR);
	}

		/* Get the rest of the message values (or defaults) */

	if ( List == 'c' || List == 'n' )					/* Default for id is next id not used */
		id = NmCMsgs + 1;								/* be sure its big enough to be unique */
	else if ( List == 'd' )
		id = MSGLSTSZ + NmDetMsg + 1;					/* detector msgs. counted from MSGLSTSZ + 1 so that */
														/* on a LoadSys() there's no chance of duplicate ids */
	else
		id = NmOMsgs + 1;

	lbufptr = GetUInt( lbufptr, &id, id, ",", &err );		
	if ( err == TRUE )	{
		sprintf(GOutBuff,"\nStoreMsg: Bad Msg_Id in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
	}

	intensit = MsgIntDf + UNoise( ( MsgIntDf / 20.0 ) );		/* Add some noise to default */
	lbufptr = GetFloat( lbufptr, &intensit, intensit, ",", &err );		
	if ( err == TRUE )	{
		sprintf(GOutBuff,"\nStoreMsg: Bad Intensity in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &producer, 0, ",", &err );		
	if ( err == TRUE ) {
		sprintf(GOutBuff,"\nStoreMsg: Bad Producer Id in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &input1, 0, ",", &err );		
	if ( err == TRUE )	{
		sprintf(GOutBuff,"StoreMsg: Bad Input1-MsgId in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &input2, 0, ",", &err );		
	if ( err == TRUE ) {
		sprintf(GOutBuff,"\nStoreMsg: Bad Input2-MsgId in '%s'\n", LineBuff );
		WriteStd( GOutBuff );
	}

		/* Got all the values, so get the next message node */

	if ( (newmsg = GtNxtMNd( List )) == NULL )	{
		sprintf(GOutBuff,"\nStoreMsg: Msglist '%c' is full!", List );
		WriteStd( GOutBuff );
		return( ERROR );
	}

	else {												 /* We've got the node, so store everything. */
		newmsg->Msg_Id = id;								
		AMsgtoB( tbuff, newmsg->Message);
		newmsg->Intensit = intensit;
		newmsg->IsMsg = TRUE;								 /* This is a message (for now). */
		if ( List == 'd' )
			newmsg->FromDet = TRUE; 						 /* from detectors */
		else
			newmsg->FromDet = FALSE;						 /* not from detectors */
		if	( producer == 0 )
			newmsg->Producer = NULL;						 /* no producer */
		else
			newmsg->Producer = (struct CfNode *) producer;	/* store id of producer */
			/* NOTE: If you get errors for these casts, that's probably ok;
					 these id's are resolved at startup time by code in CFSC.C,
					 since these id's will be non-zero only after a SAVE
			*/
		if ( input1 == 0 )
			newmsg->MtchMsg1 = NULL;						 /* no input message 1 */
		else
			newmsg->MtchMsg1 = (struct MsgNode *) input1;	 /* store id of message */
		if ( input2 == 0 )
			newmsg->MtchMsg2 = NULL;						 /* no input message 2 */
		else
			newmsg->MtchMsg2 = (struct MsgNode *) input2;	 /* store id of message */
	}

	return( OK );	/* all ok if you get here */

} /* StoreMsg */

/****************************

EmptyML		Empty the specified message list. 
			(Its already empty if the next message is the first message.)

	List:	'c' Empty current message list, CurMsgs.
			'o' Empty old message list, OldMsgs.

******/

VOID EmptyML ( List )
	char	List;
{

	if ( List == 'c' ) {
		if ( NextCMsg != CurMsgs ) { 
			NextCMsg	= CurMsgs; 		/* Just set NextCMsg pointer to start of array, and */
			NmCMsgs		= NmCMsgIn = 0;	/* set count to 0. */	
			LastCMsg = -1;				/* signals empty list. */
		} 	
		NmDetMsg = 0;					/* No detector messages now */
	}
	else if ( List == 'o' ) {
		if ( NextOMsg != OldMsgs ) { 
			NextOMsg	= OldMsgs; 		/* Just set NextOMsg pointer to start of array, and */
			NmOMsgs		= NmOMsgIn = 0;	/* set count to 0. */	
			LastOMsg	= -1;			/* signals empty list. */
		} 	
	}
	else {
		sprintf(GOutBuff,"\nEmptyML: Bad List '%c'.\n", List );
		WriteStd( GOutBuff );
	}

} /* EmptyML */

/****************************

GtNxtMNd		Get pointer to next unused node in specified message List.

	List	'c' (CurMsgs)	'o' (OldMsgs) 
			'd' (CurMsgs, but it will be a detector message so update different counters).

	Return	Pointer to node, if one available. (Also, update pointers and counters).
			NULL if no available node.

	NOTE: MakeNMsg() is CORE.C just gets 'NewMsgs' for itself (to speed it up).

******/

struct MsgNode	 *GtNxtMNd ( List )
	char	List;
{
	struct MsgNode	*mptr;

	if ( List == 'c' || List == 'd' ) {
		if ( List == 'c' ) {
			if ( NmCMsgIn > NmIMsgMx )	/* If no room for internal message */
				return( NULL ); 		/* its an error */
			else
				++NmCMsgIn; 			/* else increment counter */
		}		

		else {							
			if ( NmDetMsg > NmDMsgMx )	/* If no room for detector message */
				return( NULL ); 		/* its an error */
			else
				++NmDetMsg; 			/* else increment the counter */
		}

		mptr = NextCMsg;				/* There is room, so get pointer and update counters */
		++NextCMsg;
		++NmCMsgs;
		++LastCMsg;
	}

	else if ( List == 'o' ) {		/* store an old message */
		if ( NmOMsgs > MSGLSTSZ )		/* If too many (more than whole list), */
			return( NULL ); 			/* just quit and return NULL. */
		mptr = NextOMsg;				/* Otherwise get pointer and update counters */
		++NextOMsg;
		++NmOMsgs;
		++LastOMsg;
	}

	else {
		sprintf(GOutBuff,"\nGetNxtMNd: Bad List '%c'.\n", List );
		WriteStd( GOutBuff );
		exit( ERROR );
	} 

	return( mptr );

} /* GtNxtMNd */

#if MPWC
#define __SEG__ CFSCWRITE
#endif

/****************************

DsplMsgs		Display contents of current message list CurMsgs.

	FDName	string name of output file/device.
			(Default is to write to DisOutFN -- the Display output stream.)

	Format	parameter determines the format in which messages are to be displayed.
			See PutMNd (below) for Format values and what they do.

	List	which msglist list to display:
				'c' - current message list (CurMsgs).
				'o' - old message list (OldMsgs).
				'd' - detector messages (just detector messages in CurMsgs list).

	DsplMsgs just opens the file, calls WrtMsgs to do the work, and closes the file.

	NOTE: If List isn't one of {c,o,d}, { 'c' is displayed.

******/ 				

int DsplMsgs ( FDName, Format, List )
	char	*FDName;
	int		Format;
	char	List;
{
	FILE			*destin;

	if ( Format == DSPLOFF )
		return( OK );

	if ( strcmp( FDName, DisOutFN ) == 0 || *FDName == '\0' )
		destin = DisOFILE;									/* DisOutFN is already open */
	else if ( strcmp( FDName, StdOutFN ) == 0 ) 
		destin = StdOFILE;									/* StdOutFN is also open */
	else if ( (destin = GetFILE( FDName, "", "w" )) == NULL )
		return( ERROR );

	WrtMsgs( destin, Format, List );

	if ( destin != DisOFILE && destin != StdOFILE ) 		/* Dont close these streams! */
		CloseFILE( FDName, "", destin );

	return( OK );

} /* DsplMsgs */


/****************************

WrtMsgs 		Write messages to open file/device.

	Destin	Opened file/device.

	Format	Control format of display--see PutMNd for details.

	List	List to be written (current,old, or detectors).

******/

VOID WrtMsgs ( Destin, Format, List )
	FILE	*Destin;
	int		Format;
	char	List;
{
	int		 mcnt, maxcount, detector, numprint;
	struct MsgNode	*mptr;

	mptr	 = CurMsgs; 									/* Assume print current messages */
	maxcount = LastCMsg;									/* maximum list size */
	detector = FALSE;										/* Assume not detector messages */
	numprint = 0;											/* printed 0 so far */

	if ( List == 'd' )										/* Display detector messages */
		detector = TRUE;
	else if ( List == 'o' ) {							/* display old messages */
		mptr = OldMsgs;
		maxcount = LastOMsg;
	}

	if ( Format != SAVEFMT ) {
		if		 ( List == 'd' )
			WriteS( "\n\nDetector Msgs:\n", Destin );
		else if ( List == 'o' )
			WriteS( "\n\nOld Msgs:\n", Destin );
		else
			WriteS( "\n\nCurrent Msgs:\n", Destin );
	}

	if ( Format == DEMOFMT1 )
		WriteS( "\n Id		  Message		  Producer	 Intensity\n\n", Destin );

	for ( mcnt = 0; mcnt <= maxcount; ++mcnt, ++mptr ) {
		if ( ( List != 'd' && mptr->IsMsg && !mptr->FromDet ) ||
			 ( mptr->FromDet && (List == 'd' || List == 'o') )	 ) { 
			PutMNd( mptr, GMsgBuff, Format );
			if ( Format == SAVEFMT )
				fprintf( Destin, "%s", GMsgBuff );				/* If caller is SaveSys(), don't use Writes */
			else												/* because that may write to the log file, too! */
				WriteS( GMsgBuff, Destin );
			numprint++;
		}
	}

	if	( Format == SAVEFMT )
		fprintf( Destin, "ENDMSGS\n" );
	else
		WriteS( "\nEnd-of-list.\n", Destin );

} /* WrtMsgs */

/****************************

PutMNd		Write contents of message node into specified buffer in specified format.

	MsgPtr	pointer to message structure to display.

	Buff	buffer long enough to hold all the parts of message.

	Format	indicates display format: 
				DFLTFMT 	use DEMOFMT1 as default.
				DEMOFMT1	(id) message producer	'intensity'
				SAVEFMT 	message, id, intensity, producer, mtchmsg1, mtchmsg2	(same as used by LoadMsgs)
				GENFMT		message (intensity)

******/

VOID PutMNd ( MsgPtr, Buff, Format )
	struct	MsgNode	*MsgPtr;
	char			Buff[];
	int 			Format;
{
	int				i, block, read, write;
	unsigned int	match1, match2;
	char msgbuf[STRNGSZ+(STRNGSZ/4)+1], /* room for a blank every 4 loci */
		 prodbuf[10];
	extern short	DsplMsBl; 			/* Flag TRUE means insert blanks in message for display */

	BMsgtoA( MsgPtr->Message, msgbuf );
	if	( !DsplMsBl )								/* Don't display with blanks */
		msgbuf[STRNGSZ] = '\0';
	else {		
		msgbuf[STRNGMX+(STRNGSZ/4)] = '\0';
		for ( block = 1, read = STRNGMX, write = STRNGMX+(STRNGSZ/4)-1; write >= 0; ++block ) {
			for ( i = 1; i <= 4; ++i )
				msgbuf[write--] = msgbuf[read--];
			if ( write > 0 )
				msgbuf[write--] = ' ';
		}
	}

	if ( MsgPtr->MtchMsg1 == NULL )		match1 = 0;				/* No match1 used by producer */
	else	 match1 = MsgPtr->MtchMsg1->Msg_Id; 				/* Id of match1 message */	
	if ( MsgPtr->MtchMsg2 == NULL )		match2 = 0;				/* No match2 used by producer */
	else	 match2 = MsgPtr->MtchMsg2->Msg_Id; 				/* Id of match2 message */	


	if ( MsgPtr->Producer != NULL ) 
		sprintf( prodbuf, "%d", MsgPtr->Producer->Cf_Id );
	else {													/* No producer (from detectors or a file) */
		if ( Format != SAVEFMT )
			sprintf( prodbuf, "None");	
		else
			sprintf( prodbuf, "0" );							/* Use 0 to indicate none in SAVEFMT */
	}

	if ( Format == SAVEFMT	)
		sprintf( Buff, "%s, %u, %7.2f, %s, %u, %u\n", msgbuf, 
				 MsgPtr->Msg_Id, MsgPtr->Intensit, prodbuf, match1, match2 );
	else if ( Format == GENFMT )
		sprintf( Buff, "%s (%.2f)", msgbuf, MsgPtr->Intensit );
	else
		sprintf( Buff, "%-4.4d    %s    %8s      %7.2f\n", MsgPtr->Msg_Id, msgbuf, prodbuf, MsgPtr->Intensit );

} /* PutMNd */

#if MPWC
#define __SEG__ CFSCREAD
#endif

/****************************

LoadCfs 	Load classifiers from a file into the system.

	FDName	""			load from a default file.
			"filename"	load from specified file.

	How		'r'			replace the current list with the classifiers being read in.
			'a'			append the classifiers being read in.

	NOTE:	Input Lines that { with COMCHAR are ignored.

******/

VOID LoadCfs ( FDName, How )
	char	*FDName, How; 
{
	FILE	*Source; 

	if ( (Source = GetFILE( FDName, CfInFN, "r" )) == NULL )
		WriteStd(" -- No classifiers loaded.\n" );
	else {	
		ReadCfs( Source, How );
		CloseFILE( FDName, CfInFN, Source );
	}

} /* LoadCfs	*/

/****************************

ReadCfs 	Load classifiers from an open file into the system.

	Source	Open *FILE from which classifiers are to be read,
				one classifier per line, ended by a line that has ENDCFS on it.

	How		'r'	- replace the current list with the classifiers being read in.
			'a'	- append the classifiers being read in.

	NOTE:	Input Lines that begin with COMCHAR are ignored.

******/

VOID ReadCfs ( Source, How )
	FILE	*Source;
	char	How;
{
	unsigned int	loadcnt = 0;			/* number actually loaded now. */
	int		retlen;
	char	*rchar, *wchar;

	if ( How == 'r' )	InitCfLs();		/* If Replace, initialize classifier list */

		/* Read in each classifier from specified Source. Stop on end-of-file or on 0 length line. */
	
	while ( (ReadS( GInBuff, GIBUFFSZ-1, Source, &retlen) != EOF) && retlen != 0 && NmCfs <= CFLSTSZ ) {
		if ( *GInBuff == COMCHAR )
			continue;
		else if ( strcmp( GInBuff, "ENDCFS" ) == 0 )		/* thats all, folks */
			break;
		else if ( NmCfs >= CFLSTSZ ) {
			sprintf(GOutBuff,"\nWarning: can't load more than %d classifiers.\nSome not loaded.\n",CFLSTSZ);
			WriteStd( GOutBuff );
			break;
		}
		else {
			for ( wchar = rchar = GInBuff; *rchar != '\0'; ++rchar ) /* strip blanks */
				if ( *rchar != ' ' )	*wchar++ = *rchar;
			*wchar = '\0';
			if ( StoreCf( GInBuff ) != ERROR ) 		 /* If stored ok, update count */
				++loadcnt;
		}
	}

	NmCfsMx = NmCfs;						/* Maximum number is total */

	if ( NmCfs != 0 ) {
#if SUN3 /* kludge for bug in SUN compiler */
		AveCfStr	= TotCfStr / (int) NmCfs;		/* Recompute the average */
		AveCfBR		= TotCfBR / (int) NmCfs;
#else
		AveCfStr	= TotCfStr / NmCfs;		/* Recompute the average */
		AveCfBR		= TotCfBR / NmCfs;
#endif

		ReSrtCfs();	 /* Sort them to start */
	}
	else
		AveCfStr = AveCfBR = 0;

	if ( strcmp( GInBuff, "ENDCFS" ) != 0 ) 		
		while ( ReadS( GInBuff, GIBUFFSZ-1, Source, &retlen) != EOF )
			if ( strcmp( GInBuff, "ENDCFS" ) == 0 ) break;

	sprintf(GOutBuff,"\nLoaded %u cfs, total now %u.\n", loadcnt, NmCfs );
	WriteStd( GOutBuff );

} /* ReadCfs */


/****************************

StoreCf		Create and store new classifier in the CurCfs list.

	LineBuff	should be contain the information to store, in the following format:

	cond1,cond2/actop/action,Strength,BidRatio,Cf_Id,ChngStr,ToTMmBid,TotMtch,
		TotProd,TotPost,TotEMtch,TotEAct,TotPosRw,TotNegRw,
		TotNmOfs,StpCrtd,StpLBid,StpLPrd,StpLPst,NoBBFlg,NoRplFlg,NoPrnFlg

 where all but cond1, cond2, actop (action operator), and action are optional.

******/

int StoreCf ( LineBuff )
	char	LineBuff[];
{
	char			cond1[STRNGSZ+1], cond2[STRNGSZ+1], action[STRNGSZ+1],
					*lbufptr, tbuff[STRNGSZ+1], *GetFloat(), *GetUInt(), *GetSInt();
	unsigned int	cnd1type, cnd2type, id, actcode,
					totnmbid, totmtch, totprod, totpost, totemtch, toteact, totposrw, totnegrw, totnmofs,
					stpcrtd, stplbid, stplprd, stplpst;
	unsigned int	err;
	short			nobbflg, norplflg, noprnflg;
	float			strength, bidratio, chngstr, defstr, UNoise(), CalcSpec();
	struct	CfOpNode *atptr, *GetCOpNa();

		/* Get the first condition and (optional) condition-type prefix. */

	lbufptr = LineBuff;
	if ( *lbufptr != '1' && *lbufptr != '0' && *lbufptr != '#' )
		++lbufptr;									/* skip the condition type, and ... */
	cnd1type = CMATCH;								/* ...ignore it - always use MATCH type */

	lbufptr = StpTok2( lbufptr, cond1, sizeof(cond1), ",");
	if ( !IsCondAct( cond1 ) ) {
		sprintf(GOutBuff,"%s cond-1, in '%s'\n", StCfEMsg, LineBuff);
		WriteStd( GOutBuff );
		return( ERROR );
	}	

		/* Get the second condition and (optional) condition-type prefix. */

	if ( *lbufptr == '~' )	{
		cnd2type = CNOTMATCH;						/* NOT-MATCH condition 2 */
		++lbufptr;									/* move past the ~ */
	}	
	else {
		cnd2type = CMATCH;							/* MATCH Type */
		if ( *lbufptr == 'm' )	++lbufptr;			/* move past the m if its there */
	}

	lbufptr = StpTok2( lbufptr, cond2, sizeof(cond2), "/");
	if ( !IsCondAct( cond2 ) ) {
		sprintf(GOutBuff,"%s cond 2, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
		return( ERROR );
	}

		/* Get the action type and string. */

	lbufptr = StpTok2( lbufptr, tbuff, sizeof(tbuff), "/");
	if ( tbuff[0] == '\0' ) 						/* None supplied -- Use a default type. */
		strcpy( tbuff, "PASS");

	atptr = GetCOpNa( tbuff, &err );	
	if ( err == ERROR ) {
		sprintf(GOutBuff,"%s act-type in '%s'\n", StCfEMsg, LineBuff);
		WriteStd( GOutBuff );
		return( ERROR );
	}	
	else	actcode = atptr->CfOpCode;

	lbufptr = StpTok2( lbufptr, action, sizeof(action), ", ");
	if ( !IsCondAct( action ) ) {
		sprintf(GOutBuff,"%s action, in '%s'\n", StCfEMsg, LineBuff);
		WriteStd( GOutBuff );
		return( ERROR );
	}

		/* Now get and test the strength, bid-ratio, and classifier id. */

	defstr = CfStrDf / 20.0;
	defstr = CfStrDf + UNoise( defstr );

	lbufptr = GetFloat( lbufptr, &strength, defstr, ", ", &err );
	if ( err || strength < CfStrMin || strength > CfStrMax ) {
		sprintf(GOutBuff,"%s strength, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
		strength = defstr;
	}

		/*	See if there is a bid-ratio. Default is specificity, i.e., a count of
			non-#'s in the condition parts, divided by the STRNGSZ*2. 
		*/

	bidratio = CalcSpec( cond1, cond2, cnd1type, cnd2type );
	lbufptr = GetFloat( lbufptr, &bidratio, bidratio, ", ", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s bid-ratio, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &id, 0, ",", &err );		 /* use 0 to indicate one must be supplied later. */
	if ( err ) { 
		sprintf(GOutBuff,"%s id, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

		/* Get all the 'statistics/counting' variable values, if any supplied. */

	lbufptr = GetFloat( lbufptr, &chngstr, 0.0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s ChngStr, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totnmbid, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s ToTNmBid, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totmtch, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotMtch, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totprod, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotProd, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totpost, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotPost, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totemtch, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotEMtch, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &toteact, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotEAct, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totposrw, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotPosRw, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totnegrw, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotNegRw, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &totnmofs, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s TotNmOfs, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &stpcrtd, 0, ",", &err );	/* **** NOTE the default = 0 **** */
	if ( err ) { 
		sprintf(GOutBuff,"%s StpCrtd, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &stplbid, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s StpLBid, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &stplprd, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s StpLPrd, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetUInt( lbufptr, &stplpst, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s StpLPst, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetSInt( lbufptr, &nobbflg, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s NoBBFlg, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetSInt( lbufptr, &norplflg, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s NoRplFlg, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

	lbufptr = GetSInt( lbufptr, &noprnflg, 0, ",", &err );
	if ( err ) { 
		sprintf(GOutBuff,"%s NoPrnFlg, in '%s'\n", StCfEMsg, LineBuff );
		WriteStd( GOutBuff );
	}

		/* Add classifier with all these values to classifier list. */
        /* Final FALSE means do NOT add to ReplaceFirst list */

	if ( AddCf( cond1, cond2, action, cnd1type, cnd2type, actcode, id, strength, bidratio,
			chngstr, totnmbid, totmtch, totprod, totpost, totemtch, toteact, totposrw, totnegrw, totnmofs,
			stpcrtd, stplbid, stplprd, stplpst, nobbflg, norplflg, noprnflg, FALSE ) == ERROR )	 {
		return( ERROR );
	}

	return( OK );

} /* StoreCf */

/****************************

AddCf		Add classifier node to the CurCfs list and to the strength-sorted list.

Initialize classifier state variables, and increment the NmCfs counter and NextCf pointer.

******/

int AddCf ( Cond1, Cond2, Action, Cond1Type, Cond2Type, ActCode, Id, Str, BRatio,
			 ChangeS, TNmBid, TMtch, TProd, TPost, TEMtch, TEAct, TPosRw, TNegRw, TNmOfs,
			 StpC, StpLBid, StpLPr, StpLPs, NoBB, NoRpl, NoPrn, ReplaceFirst )
	char			Cond1[], Cond2[], Action[];
	unsigned int	Cond1Type, Cond2Type, ActCode, Id;
	float			Str, BRatio, ChangeS;
	unsigned int	TNmBid, TMtch, TProd, TPost, TEMtch, TEAct, TPosRw, TNegRw, TNmOfs, StpC, StpLBid, StpLPr, StpLPs;
	short			NoBB, NoRpl, NoPrn;
    short           ReplaceFirst;
{

	NxtFrCf->Cnd1Type = Cond1Type;
	ACndtoB( Cond1, NxtFrCf->Cnd1Bits, NxtFrCf->Cnd1DCs );
	NxtFrCf->Cnd2Type = Cond2Type;
	ACndtoB( Cond2, NxtFrCf->Cnd2Bits, NxtFrCf->Cnd2DCs );
	NxtFrCf->ActType = ActCode;
	AActtoB( Action, NxtFrCf->ActBits, NxtFrCf->ActDCs );
	NxtFrCf->Strength = Str;
	NxtFrCf->BidRatio = BRatio;
	NxtFrCf->Var      = Str * BRatio / (float) 10.0;

		/* Initialize the counters for this classifier */

	NxtFrCf->NmMtch = NxtFrCf->NmProd = NxtFrCf->NmPost = 0;

	NxtFrCf->ChngStr	= ChangeS;
	NxtFrCf->TotNmBid	= TNmBid;
	NxtFrCf->TotMtch	= TMtch;
	NxtFrCf->TotEMtch	= TEMtch;
	NxtFrCf->TotProd	= TProd;
	NxtFrCf->TotPost	= TPost;
	NxtFrCf->TotEAct	= TEAct;
	NxtFrCf->TotPosRw	= TPosRw;
	NxtFrCf->TotNegRw	= TNegRw;

	NxtFrCf->TotNmOfs	= TNmOfs;
	NxtFrCf->StpCrtd	= StpC;
	NxtFrCf->StpLBid	= StpLBid;
	NxtFrCf->StpLPrd	= StpLPr;
	NxtFrCf->StpLPst	= StpLPs;
	NxtFrCf->NoBBFlg	= NoBB;
	NxtFrCf->NoRplFlg	= NoRpl;
	NxtFrCf->NoPrnFlg	= NoPrn;

		/* Insert into list sorted on strength	*/

	InSrtCf( NxtFrCf );

		/* Increment global cflist state variables, and assign the Cf_Id if need be. */

	++NmCfs;							 /* Update number of cfs in list */
	if ( Id == 0 )
		NxtFrCf->Cf_Id	= NxtCfId++;	 /* Use next id and increment it. */
	else
		NxtFrCf->Cf_Id	= Id;			 /* Use the supplied non-zero value */
	++NxtFrCf;							 /* 'Pop' the node just used */
	TotCfStr	+= Str;					 /* Update total strength for all classifiers */
	TotCfBR		+= BRatio; 				 /* Update total bid ratio (used to get average) */

	return( OK );

} /* AddCf */

#if MPWC
#define __SEG__ CFSCWRITE
#endif

/****************************

DsplCfs 	Display all the current classifiers.
WrtCfs	

	FDName	String form of file/device to be written to.
			(Default for "" means use DisOutFN--the Display output stream.)

	Format	Specifies the format of the display:
			See PutCNd (below) for Format values and what they do.
******/

int DsplCfs ( FDName, Format )
	char	*FDName;
	int		Format;
{
	FILE	*destin;

	if ( Format == DSPLOFF )
		return( OK );

	if ( strcmp( FDName, DisOutFN ) == 0 || *FDName == '\0' )
		destin = DisOFILE;									/* DisOutFN is already open */
	else if ( strcmp( FDName, StdOutFN ) == 0 ) 
		destin = StdOFILE;									/* StdOutFN is also open */
	else if ( (destin = GetFILE( FDName, "", WRITEMODE )) == NULL ) 
		return(ERROR);

	WrtCfs( destin, Format );

	if	( destin != DisOFILE && destin != StdOFILE )		/* Dont close this stream */
		CloseFILE( FDName, "", destin );
	
	return( OK );

} /* DsplCfs	*/


VOID WrtCfs( Destin, Format )
	FILE	*Destin;
	int		Format;
{
	int		cfcnt;
	struct	CfNode *cp;
	float	dsplstr; 		/* get average S over partial cf list when displaying selected cfs */
	int		dsplcnt;
	extern	unsigned int TOTNmOfs;

	if ( NmCfs > 0 && Format != SAVEFMT )		/* Sort on strength before display */
		ReSrtCfs( );

		/* zero-out all the accumulator variables. */

    TotCfStr = AveCfStr = 0;
	WCtotstr = WCtotchng = WCtotbr = WCtotbid = 0.0;
	WCtotmat = WCtotprd = WCtotpst = WCtoteff = WCtotpos = WCtotneg = WCtotofs = 0;
	dsplstr = dsplcnt = 0;

	if ( Format != SAVEFMT ) {
		switch ( DsCfLst ) {
			case ('a'):
				sprintf( GOutBuff, "\n\nCurrent Classifiers (cycle-step %u), Str > Ave:\n", CycleStp ); 
				WriteS( GOutBuff, Destin );
				break;
			case ('b'):
				sprintf( GOutBuff, "\n\nCurrent Classifiers (cycle-step %u), bidders:\n", CycleStp );
				WriteS( GOutBuff, Destin );
				break;
			case ( 'w' ):
				sprintf( GOutBuff, "\n\nCurrent Classifiers (cycle-step %u), winners:\n", CycleStp );
				WriteS( GOutBuff, Destin );
				break;
			case ( 'p' ):
				sprintf( GOutBuff, "\n\nCurrent Classifiers (cycle-step %u), producers:\n", CycleStp ); 
				WriteS( GOutBuff, Destin );
				break;
			default:
				sprintf( GOutBuff, "\n\nCurrent Classifiers (cycle-step %u):\n", CycleStp );
				WriteS( GOutBuff, Destin );
				break;
		} /* switch */
	}

	if ( Format == DEMOFMT1 ) {
		sprintf( GOutBuff, "\nId					Classifier									Str. BidR\n");
		WriteS( GOutBuff, Destin );
	}
	else if ( Format == DEMOFMT2 )	 {
		sprintf( GOutBuff, "\nId    Strnth  ChgStr  BidR  Var   TBid   TMat   TPro   TPst  TEff TPRw TNRw Crtd Ofs\n");
		WriteS( GOutBuff, Destin );
	}
	
	if ( Format == IDSORTFMT )
		for ( cfcnt = 1, cp = CurCfs; cfcnt <= NmCfs; ++cfcnt, ++cp ) {
			WCtotstr += cp->Strength;	WCtotchng += cp->ChngStr;
			WCtotbr  += cp->BidRatio;	WCtotbid += cp->TotNmBid;
			WCtotmat += cp->TotMtch;	WCtotprd += cp->TotProd;
			WCtotpst += cp->TotPost;	WCtoteff += cp->TotEAct;
			WCtotpos += cp->TotPosRw;	WCtotneg += cp->TotNegRw;
			WCtotofs += cp->TotNmOfs;

			if ( DsCfLst == 'a' && cp->Strength < AveCfStr )
				continue;
			if ( DsCfLst == 'b' && cp->NmMtch == 0 )
				continue;
			if ( DsCfLst == 'w' && cp->NmProd == 0 )
				continue;
			if ( DsCfLst == 'p' && cp->NmPost == 0 )
				continue;
			if ( DsCfLst == '1' && !InDsCfL( cp->Cf_Id ) )
				continue;
			dsplstr = cp->Strength;
			++dsplcnt;
			PutCNd( cp, GCfBuff, Format );
			 if ( Format == SAVEFMT )
				fprintf( Destin, "%s", GCfBuff );	/* If caller is SaveSys(), don't use Writes */
			else									/* because that may write to the log file, too! */
				WriteS( GCfBuff, Destin );
		}

	else if ( Format == SAVEFMT )
		for ( cfcnt = 1, cp = CurCfs; cfcnt <= NmCfs; ++cfcnt, ++cp ) {
			WCtotstr += cp->Strength;	WCtotchng += cp->ChngStr;
			WCtotbr  += cp->BidRatio;	WCtotbid += cp->TotNmBid;
			WCtotmat += cp->TotMtch;	WCtotprd += cp->TotProd;
			WCtotpst += cp->TotPost;	WCtoteff += cp->TotEAct;
			WCtotpos += cp->TotPosRw;	WCtotneg += cp->TotNegRw;
			WCtotofs += cp->TotNmOfs;
			PutCNd( cp, GCfBuff, Format );
			 fprintf( Destin, "%s", GCfBuff );	/* If caller is SaveSys(), don't use Writes */
		}

	else { /* Display from high strength to low */
		for ( cfcnt = 1, cp = HiStrCf; cfcnt <= NmCfs && cp != NULL; ++cfcnt, cp = cp->PrvStrCf ) {
			WCtotstr += cp->Strength;	WCtotchng += cp->ChngStr;
			WCtotbr  += cp->BidRatio;	WCtotbid += cp->TotNmBid;
			WCtotmat += cp->TotMtch;	WCtotprd += cp->TotProd;
			WCtotpst += cp->TotPost;	WCtoteff += cp->TotEAct;
			WCtotpos += cp->TotPosRw;	WCtotneg += cp->TotNegRw;
			WCtotofs += cp->TotNmOfs;
			if ( ( DsCfLst == 'a' && cp->Strength < AveCfStr ) ||
				( DsCfLst == 'b' && cp->NmMtch == 0 ) ||
				( DsCfLst == 'w' && cp->NmProd == 0 ) ||
				( DsCfLst == 'p' && cp->NmPost == 0 ) ||
				( DsCfLst == '1' && !InDsCfL( cp->Cf_Id ) ) )
				 continue;

			dsplstr += cp->Strength;
			++dsplcnt;

			if ( Format >= ENVFMT1 && Format <= ENVFMT10 )
				DisCfIE( cp, GCfBuff, Format );
			else
				PutCNd( cp, GCfBuff, Format );

			if ( Format == SAVEFMT )
				fprintf( Destin, "%s", GCfBuff );
			else
				 WriteS( GCfBuff, Destin );
		}
	}

    TotCfStr = WCtotstr;
    if ( NmCfs != 0 ) 
        AveCfStr = TotCfStr / (int) NmCfs;

	if ( Format == DEMOFMT2 ) {
		if ( NmCfs != 0 ) {
			sprintf( GOutBuff,"\n      %6.0f %8.2f %4.2f -Var- %6u %6u %6u %6u %5u %4u %4u      %3u\n",
#if SUN3	/* kludge for SUN compiler bug */
				WCtotstr, WCtotchng, (WCtotbr/ (int) NmCfs), WCtotbid, WCtotmat, WCtotprd, WCtotpst, WCtoteff,
#else
				WCtotstr, WCtotchng, (WCtotbr/NmCfs), WCtotbid, WCtotmat, WCtotprd, WCtotpst, WCtoteff,
#endif
				WCtotpos, WCtotneg, WCtotofs );
			WriteS( GOutBuff, Destin );
		}

		sprintf( GOutBuff,"\n\nTotals for run:            %6u %6u %6u %6u %5u %4u %4u      %3u", 
			TOTNmBid, TOTMtch, TOTMsPrd, TOTMsPst, TOTEAMsg, TOTNmPRw, TOTNmNRw, TOTNmOfs );
		WriteS( GOutBuff, Destin );
	}
	
	if ( Format == SAVEFMT )
		fprintf( Destin, "ENDCFS\n" );
	else {
		sprintf( GOutBuff, "\n\nNumber of Classifiers: %d. Ave. strength %6.2f (total %7.2f).\n",
			NmCfs, AveCfStr, TotCfStr );
		WriteS( GOutBuff, Destin );
		if ( DsCfLst != 0 && dsplcnt != 0 ) {
#if SUN3	/* kludge for SUN compiler bug */
			sprintf( GOutBuff, "Ave Str of %u in DsCfLst: %7.1f.\n", dsplcnt, dsplstr/ (int) dsplcnt );
#else
			sprintf( GOutBuff, "Ave Str of %u in DsCfLst: %7.1f.\n", dsplcnt, dsplstr/ dsplcnt );
#endif
			WriteS( GOutBuff, Destin );
		}
	}

} /* WrtCfs */

/****************************

PutCNd		Writes contents of a CfNode into supplied buffer.

	CP		pointer to CfNode to write.
	OutBuf	buffer large enough to hold all the parts.
	Format	control what is displayed and how it is formatted. Values:
				value		format
				DFLTFMT		default - use DEMOFMT1.
				DEMOFMT1	display full classifier: id. classifier  strength  BidRatio
				SAVEFMT		display full classifier in format used by LoadCfs.
				DEMOFMT2	display part classifier: id. strength	BidRatio &etc.
				IDSORTFMT	same as DEMOFMT1, except caller display then in id order.
				GENFMT		same as DEMOFMT1
******/

VOID PutCNd ( CP, OutBuf, Format )
	struct CfNode	*CP;
	char			OutBuf[];
	int 			Format;	
{
	int		retval;
	char	cfbuf[3*(STRNGSZ)+32],			/* assemble the classifier itself here */
			tbuf[STRNGSZ+10]; 				/* temporary buffer */
	struct CfOpNode *coptr, *GetCOpCd();	/* pointer to classifier operator (ActType) node. */

	if ( Format == GENFMT ) 
		Format = DEMOFMT1;

	OutBuf[0] = cfbuf[0] = '\0';

	if ( Format == DEMOFMT1 || Format == SAVEFMT ) { 
		if ( CP->Cnd1Type == 0 )
			cfbuf[0] = 'm';
		else
			cfbuf[0] = '~';
		BCndtoA( CP->Cnd1Bits, CP->Cnd1DCs, tbuf );
		tbuf[STRNGSZ] = ',';
		tbuf[STRNGSZ+1] = '\0';
		CopyChar( &cfbuf[1], tbuf, STRNGSZ/2 );
		cfbuf[(STRNGSZ/2)+1] = ' ';
		cfbuf[(STRNGSZ/2)+2] = '\0';
		strcat( cfbuf, &tbuf[STRNGSZ/2] );

		if ( CP->Cnd2Type == 0 )
			tbuf[0] = 'm';
		else
			tbuf[0] = '~';
		BCndtoA( CP->Cnd2Bits, CP->Cnd2DCs, &tbuf[1]);
		tbuf[STRNGSZ+1] = '/';
		tbuf[STRNGSZ+2] = '\0';
		strcat( cfbuf, tbuf );

		coptr = GetCOpCd( CP->ActType, &retval );
		 if ( retval == ERROR ) {
			sprintf(GOutBuff,"\n***PutCNd: cf %u has bad ActType %u.\n", CP->Cf_Id, CP->ActType);
			WriteStd( GOutBuff );
		}
		strcat( cfbuf, coptr->CfOpName );
		strcat( cfbuf, "/" );

		BActtoA( CP->ActBits, CP->ActDCs, tbuf );
		if ( Format != DEMOFMT1 ) {
			tbuf[STRNGSZ]	= ',';
			tbuf[STRNGSZ+1] = '\0';
		}
		else
			tbuf[STRNGSZ] = '\0';
		strcat( cfbuf, tbuf );
	} /* Format == display cf itself */

	if ( Format == DEMOFMT1 )
		sprintf( OutBuf, "%-5u %s  %6.0f %4.2f\n", CP->Cf_Id, cfbuf, CP->Strength, CP->BidRatio );
	else if ( Format == DEMOFMT2 )
		sprintf( OutBuf,"%-5u %6.0f %8.2f %4.2f %5.3f %6u %6u %6u %6u %5u %4u %4u %4u %3u\n",
			CP->Cf_Id, CP->Strength, CP->ChngStr, CP->BidRatio, CP->Var, CP->TotNmBid, CP->TotMtch,
			CP->TotProd, CP->TotPost, CP->TotEAct, CP->TotPosRw, CP->TotNegRw, CP->StpCrtd, CP->TotNmOfs );
	else	 /* Format is SAVEFMT */
		sprintf( OutBuf, "%s%f,%f,%u,%f,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u\n",
			cfbuf,CP->Strength,CP->BidRatio,CP->Cf_Id, CP->ChngStr, CP->TotNmBid, CP->TotMtch,
			CP->TotProd, CP->TotPost, CP->TotEMtch, CP->TotEAct, CP->TotPosRw, CP->TotNegRw, CP->TotNmOfs,
			CP->StpCrtd, CP->StpLBid, CP->StpLPrd, CP->StpLPst,
			CP->NoBBFlg, CP->NoRplFlg, CP->NoPrnFlg );

} /* PutCNd */
