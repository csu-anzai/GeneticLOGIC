/*			Main Entry Point for the CFS-C Classifier System.

This file,	CFSC.C, contains the main entry point for the
CFS-C classifier system  (Copyright 1986,1988 Rick L. Riolo).

To run the system, it must be linked to some domain-specific subroutines to implement
an environment, e.g., the FSW1 environment.  If the runable file is called FSW1, then use

	fsw1  <init-path>  <load-file>
	
where <init-path> is an optional path name that will be used to locate the file init.cfs (note the case ),
which contains a list of all run-time settable variables (and some information about them).
The file init.cfs should only be changed when
(1) variables are added to or removed from the system.
(2) hi/low bounds on a variable are to be changed.
(3) default names for char variables are changed (e.g., default names for init files).
<load-file> is also optional; it specifies the full path/filename for a file that was
created with the CFS-C SAVESYS command.  It basically contains the state of the system
at the time the command was issued, and when the system is started with such a file it
starts in that state.

This file contains these subroutines:

	main		the main entry for the system.
	DoCmd		execute a user command.
	ExecCmds	read and execute commands from a file.
	Classify	the major-cycle loop.		
	StartStp,EndStep	initialize and clean-up before/after a major-cycle step.
	Init_CFS,InitVT 	initialize the system.
	InitCfLs	initialize the classifier list.

	LoadSys 	load entire system from a file.
	ReadVars	read in core system variables
	LinkCMs 	link classifiers and messages after a "re-start" LoadSys().
	GetMsgAd	get address for message with specified Msg_Id.
	GetCfAd 	get address for classifier with specified Cf_Id.

	SaveSys 	write entire system state to file for use by LoadSys().
	WrtCons 	write core system constants
	ReadEffs	read effector conditions, names, etc, and store in effector nodes (EffNode's).
	WrtEffs 	write EffNode contents in format ready to be read by ReadEffs().

	InitDate	initialize the RunDate, RunTime and SaveSyFN variables.
**/

char	CRight[] = "Copyright 1986,1988 Rick L. Riolo";

#include	"compiler.h"		/* set a switch indicating compiler/machine being used */
#include <stdio.h>
#if  ( LATTICEC || CBELLMTS || SUN3 || MPWC )
#include	<ctype.h>
#endif
#if  MACAZTEC
#include	"QuickDraw.H"			/* For using QuickDraw routines */
#include	"Font.H"				/* For setting font type and char. size */
#endif
#include	"utility.h" 			/* General purpose constants */
#include	"usercmd.ext"			/* some stuff for processing user commands */
#include	"cfsio.ext" 			/* some I/O declarations */
#include	"core.ext"				/* extern's for major data structures */ 
#include	"dsclearn.ext"			/* Used in StartStp() to reset vars */

/**
  Variables to count cycles and decide when to auto-print variables or reports.

  AutoDspl = number of AD...Int items that are > 0 (0 is 'no-display').
			 So if AutoDspl is 0, there is nothing to autodisplay.

  ADxxxInt = display interval for item 'xxx'.  0 is taken as 'no-display'.
  ADxxxFmt = display format	for item 'xxx'.
**/

short	 int  ClampFlg = FALSE; 		/* Set TRUE to stop all learning, AND CycleStp */
short	 int  StopFlag = FALSE; 		/* Set TRUE to stop run. */

unsigned int  AutoDspl	= 0;			/* OFF if ALL autodisplay is off, ON if any one or more are ON */

unsigned int  ADCMInt	= ADOFF;		/* CM = current messages */
unsigned int  ADCMFmt	= DEMOFMT1; 
unsigned int  ADDMInt	= ADOFF;		/* DM = detector messages */
unsigned int  ADDMFmt	= DEMOFMT1; 

unsigned int  ADCfInt	= ADOFF;		/* Cf = classifiers */
unsigned int  ADCfFmt	= DEMOFMT2; 
unsigned int  ADVarInt	= ADOFF;		/* Var = system variables */
unsigned int  ADVarFmt	= DEMOFMT1; 

unsigned int  ADCycInt	= ADOFF;		/* Cyc = Cycle Step */
unsigned int  ADCycFmt	= DEMOFMT1;
unsigned int  ADEnvInt	= ADOFF;		/* Env = environment  */
unsigned int  ADEnvFmt	= DEMOFMT1; 

unsigned int  SaveInt  = ADOFF; 		/* AutoSave interval */
unsigned int  SaveFNum = 1; 			/* Number of NEXT AutoSave file */


char  Version[] = "0.95";				/* Version number	****  How to include the compilation date?? **** */
char  RunDate[] = "00/00/00";			/* Date of run. */
char  RunTime[] = "00:00";				/* Time of run. */

VOID  main(), DoCmd(), ExecCmds(), InitCfLs(), LoadSys(), ReadVars(), LinkCMs(),
	  WrtCons(), WrtEffs(), InitDate(); 

/*******************************

Main		The main entry point for the system.

This calls functions to initialize the system, and then enters a loop to process user commands. 
Each command is read and then processeed by appropriate function.

*******/

#if !MACLSC
VOID  main( argc, argv )
#else
VOID  _main( argc, argv )
#endif
	int		argc;
	char	*argv[];
{
	int		CmdCode;
	char	CmdPars[CMDPARSZ+1];

#if  MACAZTEC
	InitFonts();
	TextFont( 4 );							/* Monoco 9 point seems pretty good */
	TextSize( 9 );
#endif

	if ( argc == 2 )
		Init_CFS( argv[1], "" );
	else if ( argc == 3 )
		Init_CFS( argv[1], argv[2] );
	else
		Init_CFS( "", "" );
		
	while ( !StopFlag ) {

		CmdCode = GUserCmd( CmdPars );		/* Get code for command. */

		DoCmd( CmdCode, CmdPars );			/* Execute the command (may set StopFlag TRUE) */

	}

	if ( LogFlg ) {
		LogFlg = OFF;
		CloseFILE( LogFName, LogFName, LogFILE );
		sprintf(GOutBuff,"\nLogging on '%s' terminated.\n", LogFName);
		WriteStd( GOutBuff );
	}

	if ( GenFlg ) {
		GenFlg = OFF;
		CloseFILE( GenFName, GenFName, GenFILE );
		sprintf(GOutBuff,"\nGeneology recording on '%s' terminated.\n", GenFName);
		WriteStd( GOutBuff );
	}

} /* main */

/*******************************

DoCmd		  Execute Command entered by user.

  CmdCode	  Code for command, returned by GUserCmd().

  CmdPars	  Parameters included on command.

  Note that this may set the global variable StopFlag TRUE to stop the run.

******/

VOID  DoCmd ( CmdCode, CmdPars )
	unsigned int  CmdCode;
	char		 *CmdPars;
{

	switch(CmdCode) {
		case STOP_CC:	StopFlag = TRUE;
						break;
		case LOADC_CC:	LoadCfs(CmdPars, 'r');	 /*  Load classifiers (replace old) */
						break;
		case LOADM_CC:	LoadMsgs(CmdPars,'r','c');	 /* Load messages (replace old) into current list. */
						break;
		case LOADE_CC:	LoadEnv(CmdPars);		 /*  user supplies a file or use default */
						break;
		case GENRC_CC:	GnRndCfs(CmdPars);		 /*  generate random classifiers: par="how_many,meanStr,meanBR" */
						break;
		case DISPL_CC:	Display(CmdPars);		 /*  [parameter [,parameter]+]	 */
						break;
		case SET_CC: 	Set(CmdPars);			 /*  parameter	value  */
						break;
		case CLASS_CC:	Classify(CmdPars);		 /* Run model major cycle. CmdPars indicates how may steps. */
						break;
		case STEP_CC:	Classify("1");			 /* special case of RUN with  CmdPars = '1'  */
						break;
		case CMDS_CC:	ExecCmds( CmdPars );	 /* read and execute commands from file */
						break;
		case ECMD_CC:	DoEnvCmd(CmdPars);		 /* Do an environment-specific command */
						break;
		case SAVES_CC:	SaveSys(CmdPars);		 /* Save entire system to file */
						break;
		case MDCF_CC:	Mod_Cf(CmdPars);		 /* prompts for cf-id and new value */
						break;
		case MDMSG_CC:	Mod_Msg(CmdPars);		 /* Prompts for msg-id and new value */
						break;
		case APCF_CC:	LoadCfs(CmdPars, 'a');	 /* Same as Load, except append classifers to current list. */
						break;
		case APMSG_CC:	LoadMsgs(CmdPars, 'a','c');  /* Same as Load, except append messages to current list.  */
						break;
		case HELP_CC:	Help(CmdPars);
						break;
		case NULL_CC:	break;				 /* Don't do anything for a null line */
		default: 		WriteStd("\n\nIllegal command. For help, enter HELP .\n");
						break;
	}

}  /*  DoCmd	*/


/*******************************

ExecCmds	  Execute commands from a file until EOF reached.

  FDName	  Null-terminated string name of file from which commands are to be read.

******/

VOID  ExecCmds ( FDName )
	char  *FDName;
{
	int		retlen, CmdCode, cnt;
	char	CmdPars[CMDPARSZ+1], *lptr,
			Command[CMDNAMSZ+1];				/* room for command name and \0 */
	FILE	*fptr;
	extern short EchoFlg;						/* see USERCMD.DEF */

	if ( (fptr = GetFILE( FDName, "", READMODE )) == NULL ) 
		WriteStd(" No commands to execute.\n"); 
	else {
		while ( ReadS( GInBuff, GIBUFFSZ, fptr, &retlen) != EOF ) {
			if ( EchoFlg )	{
				WriteStd( GInBuff );	
				WriteStd( "\n" );
			}

			for ( lptr = GInBuff; *lptr == ' '; ++lptr )  ; 	 /* skip leading blanks */

			for ( cnt=0; *lptr!=COMCHAR && *lptr!=' ' && *lptr!='\0' && cnt<=CMDNAMSZ; ++lptr, ++cnt )
				Command[cnt] =  isupper( *lptr) ? tolower( *lptr ) : *lptr;
			Command[cnt] = '\0';

				/* Get CmdCode, or null line command code or error command code. */

			if	( cnt == 0 )
				CmdCode = NULL_CC;
			else if ( (CmdCode = GetCmdCd( Command )) == ERROR )
				CmdCode = ERROR_CC;
			else {
				while ( *lptr == ' ')	++lptr; 			 /* skip blanks before parameter */

				for ( cnt=0; *lptr!=COMCHAR && *lptr!='\0' && cnt<=CMDPARSZ; ++cnt, ++lptr )
					CmdPars[cnt] = isupper( *lptr) ? tolower( *lptr ) : *lptr;
				CmdPars[cnt] = '\0';
			}

			DoCmd( CmdCode, CmdPars );

		}  /* more lines in file */

		CloseFILE( FDName, "", fptr );

	} /* file was opened */

}  /* ExecCmds */ 


/*******************************

Classify	The major cycle.

On entry, StepsPar tells how many cycle steps should be executed.
The while-statement keeps executing all the major cycle subroutines until
the specified number of steps have been done or until something  sets StopFlag TRUE.

At the end of each major-cycle, the CurMsgs becomes the OldMsgs list, the
NewMsgs (generated during that step) becomes the CurMsgs, and the space used by
the previous OldMsgs list is setup to be used for NewMsgs on the next step.

******/

Classify( StepsPar )
	char StepsPar[];
{
    int             err;
	unsigned int    StopStep, Steps, savesd;
    char            *GetUInt();

    GetUInt( StepsPar, &Steps, 0, ";", &err );
	if ( err || Steps < 1 ) {
		sprintf(GOutBuff,"\nERR--illegal count for CLASSIFY: %s", StepsPar );
		WriteStd( GOutBuff );
		 return(ERROR);
	 }

	for ( StopStep=CycleStp + Steps, StopFlag=FALSE; CycleStp < StopStep && !StopFlag; ++CycleStp ) {

		StartStp(); 			/* Initialize various variables for bidding, etc. */

		RdDetect(); 			/* Get messages from detectors */

		GenCands(); 			/* Generate lists of candidate classifiers/matches; Accumulate support */

		GenNMsgs(); 			/* Generate bids, pick winning classifiers, generate new messages. */

		GenBehav(); 			/* Mark those effectors satisfied by the new messages, and link the
									satifying messages to those effectors. Resolve conflicts, if any.
									Update candidate classifiers and new messages to reflect any messages
									deleted by the conflict resolution mechanism. Generate behavior from
									activated effectors, and get reward from enviornment.
								*/
		if ( !ClampFlg ) {
			UpdCfStr(); 		/* Update Classifier strengths using payoffs from Environment,	and taxes. */

			Discover(); 	/* Check "discovery" trigger conditions, and apply algorithms if appropriate. */
		}

		EndStep();				/* Exchange new, current, and old message lists, and do other clean-up jobs. */ 

		if ( AutoDspl > 0 ) 
			AuDisplay();						/* AutoDisplay whatever the user has requested. */

		if ( CycleStp %  SaveInt == 0 ) {	/* Save the state of the system */
			if ( !ClampFlg ) {
				++CycleStp; 					/* KLUDGE: Make CycleStp equal to what it will be at the start */	
				SaveSys ( "" ); 				/* of the next cycle, when a user could enter a SAVESYS command. */
				--CycleStp; 					/* undo the kludge! */
			}
		}

	} /* major cycle */

	if ( ClampFlg ) 	/* if we are clamped, de-increment CycleStp */	
		--CycleStp; 	/* ***NOTE WELL*** You must STEP when ClampFlg is on to keep CycleStp right */

	return( OK );

} /* Classify */

/*******************************

StartStp	  Initialize variables for bidding, etc., at start of major-cycle step.

******/

StartStp ( )
{
	struct	CfNode	 *cp;
	struct	MsgNode  *mp;
	register int mcnt;
	
	TotCCSup = TotBBid = TotCCBid = HiCCBid = 0;
	LowCCBid  = BIGFLOAT;
	NmCandCf = NmMtchs = NmCfWon = NmCfPost = 0;

    NmActCfTminus2 = NmActCfTminus1;  /* Now its number active two steps ago */
	NmActCfTminus1 = 0;			    /* Get ready to store active classifiers in prev active list */

	for ( cp = CandCfs; cp != NULL; cp = cp->NxtCand ) {
		if ( cp->NmPost > 0 ) {						/* Its active, so store in prev list */
			cp->NmPost = cp->PstEfMsg = 0;
			ActCfPrv[NmActCfTminus1].ActCf = cp;				/* store pointer to it, and */
			ActCfPrv[NmActCfTminus1].ChngStr = cp->ChngStr;	/* change in strength this step */
			++NmActCfTminus1; 								/* and increment count */
		}			
		cp->NmMtch = cp->NmProd = 0;
	}

	CandCfs = NULL;

	for ( mp = CurMsgs, mcnt = 0; mcnt <= LastCMsg; ++mp, ++mcnt ) {
		mp->MsgUsed  = FALSE;
		mp->MsgMtchd = FALSE;
	}

		/*	Make current active cf list into previous, and init current
			(This is used by DscACPC() in DSCOPS2.C)
		*/

	return( OK );

}  /* StartStp */


/*******************************

EndStep 	  Do clean-up jobs at the end of a major-cycle step.

	Exchange new, current, and old message lists.

	Update some global state variables (eg, AveCfStr - average strength of all classifiers).

******/

EndStep ( )
{
	struct MsgNode	*oldlist;

	TOTNmBid += NmCandCf;			 /* Totals for classifiers	*/
	TOTNmWin += NmCfWon;
	TOTCfPst += NmCfPost;
	TOTMtch  += NmMtchs;			 /* Total for matches, i.e., potential matches */
	TOTMsPst += NmNMsgs;			 /* Total of messages produced (after effector conflict resolution) */

	oldlist = OldMsgs;				 /* save pointer to those nodes */

	OldMsgs  = CurMsgs; 			 /* Make current list the old list. */
	LastOMsg = LastCMsg;
	NmOMsgs  = NmCMsgs;
	NmOMsgIn = NmCMsgIn;

	CurMsgs  = NewMsgs; 			 /* Make new messages current */
	LastCMsg = LastNMsg;
	NmCMsgs  = NmNMsgs;
	NmCMsgIn = NmNMsgIn;
	NextCMsg = NextNMsg;

	NewMsgs  = oldlist; 			 /* Use (saved) old-message space for new list */
	NextNMsg = NewMsgs;
	NmNMsgs  = NmNMsgIn = 0;
	LastNMsg = -1;					 /* NewMsgs list is empty to start a cycle-step. */

	if ( NmCfs > 0 )
#if SUN3  /* kludge for SUN compiler */
		AveCfStr = TotCfStr  / (int) NmCfs;
#else
		AveCfStr = TotCfStr  / NmCfs;
#endif
	else
		AveCfStr = 0;

	return( OK );

}  /*  EndStep  */


#if  MPWC
#define  __SEG__ CFSCINIT
#endif

/*******************************

Init_CFS	  Initialize the classifier system. 

First get space for various runtime settable variables, and load information from them
from the file InitPath\INIT.CFS into the RTVars table, using RTVarAdd to link to variable address.
(See USERCMD.? for definitions and use of these tables.)

Then get memory for the message lists, classifiers, and effectors,
and do some preliminary initialization of variables related to those data structures.

Then if RunFName is not a null string (""), initialize EVERYTHING from that file (if its openable).
Otherwise if RunFName  is a null string,
a. If STARTUP.CFS is openable file, initialize EVERYTHING from it.
b. Otherwise, just use defaults for variables and then load messages, classifiers,
	and the environment from the default files for each (see CFSIO.DEF).

******/

Init_CFS ( InitPath, RunFName )
	char  *InitPath, *RunFName;
{
	int 			i, retlen, err, err1;
	char			*calloc(), *malloc(), *GetUInt(), *GetFloat(), *StpTok2();
	char			*fname, *cp;
	FILE			*fptr, *GetFILE();
	struct MsgNode	*cmp, *nmp, *omp;
#if  M_I86HM
	void	*halloc();	/* Huge model for MS C */
	unsigned int cfnodesz, cfnodeaz;
	long unsigned int cflstsz;
#endif

#if ( CI86 || CBELLMTS )
	extern	FILE  *StdOFILE, *DisOFILE;
	StdOFILE = DisOFILE = stdout;
#endif

	InitVT( InitPath );
	
	InitDate( );		/* Get RunDate and RunTime, setup SaveSyFN */

		/* Set up message list size limits: 
			 NmDMsgMx - setable number of spots on message list reserved for detector messages.
			 NmIMsgMx - maximum number of 'internal' (non-detector) messages, which is all
						not reserved for detector messages (user/program setable).
			Thus ( NmDMsgMx + NmIMsgMx ) better be <= MSGLSTSZ !
		*/

	if ( NmIMsgMx + NmDMsgMx != MSGLSTSZ ) { 
		WriteStd("\n\nNmIMsgMx and NmDMsgMx values in CORE.DEF not equal to MSGLSTSZ.");
		WriteStd("\nSetting NmIMsgMx to 24 and NmDMsgMx to whatever is left!\n");
		NmIMsgMx = min( 24, MSGLSTSZ );
		NmDMsgMx = MSGLSTSZ - NmIMsgMx;
	}	

		/* Get space for message lists and initialize them */

	CurMsgs = (struct MsgNode *) calloc( MSGLSTSZ, sizeof(struct MsgNode) );
	if ( CurMsgs == NULL ) {
		WriteStd("\nERR in Init_CFS while getting memory for CurMsgs - calloc returned NULL!");
		exit( );
	}

	NewMsgs = (struct MsgNode *) calloc( MSGLSTSZ, sizeof(struct MsgNode) );
	if ( NewMsgs == NULL ) {
		WriteStd("\nERR in Init_CFS while getting memory for NewMsgs - calloc returned NULL!");
		exit( );
	}

	OldMsgs = (struct MsgNode *) calloc( MSGLSTSZ, sizeof(struct MsgNode) );
	if ( OldMsgs == NULL ) {
		WriteStd("\nERR in Init_CFS while getting memory for OldMsgs - calloc returned NULL!");
		exit( );
	}

	for ( i = 1,cmp=CurMsgs,nmp=NewMsgs,omp=OldMsgs; i <= MSGLSTSZ; ++i,++cmp,++nmp,++omp ) {
		cmp->Msg_Id = nmp->Msg_Id = omp->Msg_Id = 0;	
	}

	NextCMsg = CurMsgs; 			 /* Next messages will go at beginning. */
	NextOMsg = OldMsgs;
	NextNMsg = NewMsgs;

		/* Get space for classifiers and initialize them */

#if  M_I86HM
	cflstsz = CFLSTSZ;
	cfnodesz = sizeof(struct CfNode);
	cfnodeaz = cfnodesz + 6;			 /* **NOTE**  Must fix to be 2**i ***  */
	CfLst = (struct CfNode *) halloc( cflstsz, cfnodeaz );
	if ( CfLst == NULL ) {
		WriteStd("\nERR: Init_CFS, for CfLst: halloc return NULL!");
		sprintf( GOutBuff, "\n	 CFLSTSZ %d, CfNode alloc size %d (size %d)",
					cflstsz, cfnodeaz, cfnodesz );
		WriteStd( GOutBuff );
		exit( );
	}
	else {
		sprintf( GOutBuff, "\n  [** Using halloc: CFLSTSZ %ld, CfNode alloc size %d (real size %d) **]",
				 cflstsz, cfnodeaz, cfnodesz );
		WriteStd( GOutBuff );
	}
#else
	CfLst = (struct CfNode *) calloc( CFLSTSZ, sizeof(struct CfNode) );
	if ( CfLst == NULL ) {
		WriteStd("\nERR: Init_CFS, for CfLst: calloc return NULL!");
		exit( );
	}
#endif
	InitCfLs();

		/* Set up array of unsigned int's, so that i-th bit of i-th entry is set to 1, other bits 0. */
	
	for ( i = 0; i < INTSZ; ++i )
		ORMBits[i] = 1 << i;

	sprintf(GOutBuff,"\nCFC-C Version %s. %s. RunDate %s (%s).", Version, CRight, RunDate, RunTime);
	WriteStd( GOutBuff );

#if  M_I86HM
	cflstsz = CFLSTSZ; cflstsz *= cfnodeaz;
	sprintf( GOutBuff, "\n[MsgNode Sz %d, CfNode Sz %d, CFLSTSZ %d, ]", 
				sizeof(struct MsgNode), sizeof(struct CfNode), CFLSTSZ );
	WriteStd( GOutBuff );
	sprintf( GOutBuff, "CfLst size %lu].", cflstsz );
#else
#if LATTICEC
#if __HIGHC__
	sprintf( GOutBuff, "\n[MsgNode Sz %d, CfNode Sz %d, CFLSTSZ %d, CfLst sz %u, start %x.",
			sizeof(struct MsgNode), sizeof(struct CfNode), CFLSTSZ, CFLSTSZ * sizeof(struct CfNode), CfLst );
#else
	sprintf( GOutBuff, "\n[MsgNode Sz %d, CfNode Sz %d, CFLSTSZ %d, CfLst sz %u, start %x, memavl %d].",
			sizeof(struct MsgNode), sizeof(struct CfNode), CFLSTSZ, CFLSTSZ * sizeof(struct CfNode), CfLst, _memavl() );
#endif
#else
	sprintf( GOutBuff, "\n[MsgNode Sz %d, CfNode Sz %d, CFLSTSZ %d, CfLst size %u, start %X].", 
			sizeof(struct MsgNode), sizeof(struct CfNode), CFLSTSZ, CFLSTSZ * sizeof(struct CfNode), CfLst );
#endif
#endif
	WriteStd( GOutBuff );

		/* If there is a RunFName, try to open it. Otherwise try to open the default startup file. */

	if ( RunFName[0] != '\0' ) { 
		if ( (fptr=GetFILE( RunFName, RunFName, READMODE )) == NULL )
			exit( );
		else
			fname = RunFName;			  /* RunFName is open, so will load from it */
	}
	else if ( LoadSyFN[0] != '\0' ) {
		if ( (fptr = fopen( LoadSyFN, READMODE )) != NULL )
			fname = LoadSyFN;
	}

	if ( fptr != NULL ) 				  /* one of the startup files is open, so... */
		LoadSys( fname, fptr ); 		  /* use it to initialize the system */

	else {							  /* neither open, so load from separate (default) files */
		LoadCfs( CfInFN, 'r' ); 	
		LoadMsgs( MsgInFN, 'r', 'c' );
		LoadEnv( EnvInFN );
	}

	InitRand();		 /* init random number generator (if needed) */

	return( OK );

} /* Init_CFS  */



int  InitVT( InitPath )
	char *InitPath;
{
	int 			i, retlen, vint, err, err1;
	float			vmin, vmax;
	unsigned int	vuint, vcode, nmloaded, nmleft;
	char			*calloc(), *malloc(), *GetUInt(), *GetFloat(), *StpTok1(), *StpTok2();
	char			*fname, *cp, *chardft, vname[16], alias[16], vflags[4];
	FILE			*fptr, *GetFILE();
	
		/*	First get some space for some global scratch variables. */

	GOutBuff = malloc( GOBUFFSZ );
	if ( GOutBuff == NULL ) {
		WriteStd("\nERR (InitVT) malloc->NULL to GOutBuff!");
		fflush( stdout );
		exit( );
	}
	GInBuff = malloc( GIBUFFSZ );
	GCfBuff = malloc( CFBUFSZ );
	GMsgBuff = malloc( MSGBUFSZ );
	if ( GInBuff == NULL || GCfBuff == NULL || GMsgBuff == NULL ) {
		WriteStd("\nERR (InitVT) malloc->NULL to GInBuff &etc!");
		exit( );
	}

	LogFName = malloc( FNAMESZ );
	GenFName = malloc( FNAMESZ );
	StdInFN = malloc( FNAMESZ );
	StdOutFN = malloc( FNAMESZ );
	DisOutFN = malloc( FNAMESZ );
	MsgInFN = malloc( FNAMESZ );
	CfInFN = malloc( FNAMESZ );
	LoadSyFN = malloc( FNAMESZ );
	SaveSyFN = malloc( FNAMESZ );
	SaveSyPa = malloc( FNAMESZ );
	EnvInFN = malloc( FNAMESZ );
	if ( LogFName == NULL || GenFName == NULL || StdInFN == NULL || StdOutFN == NULL ||
		 DisOutFN == NULL || MsgInFN == NULL || CfInFN == NULL || LoadSyFN == NULL ||
		 SaveSyFN == NULL || SaveSyPa == NULL || EnvInFN == NULL ) {
		WriteStd("\nERR (InitVT) malloc->NULL to LogFName &etc!");
		exit( );
	}

	strcpy( MsgInFN, "init.msg" );	/* initial values, just in case none loaded later */
	strcpy( CfInFN, "init.cf" );
	strcpy( EnvInFN, "init.env" );
	*LogFName = *GenFName = *LoadSyFN = *SaveSyFN = *SaveSyPa = '\0';
		
		/*	Read in the information from the init.cfs file. The first non-comment line contains
			the number of variables to expect, in the form:
				NUM-VARS 100
			so read it and use it to allocate space.
			First use GOutBuff to construct name of init file.
		*/

	strcpy( GCfBuff, InitPath );		/* get path name if one specified */
	strcat( GCfBuff, "init.cfs" ); 	/* and here is the init file name */
	if ( (fptr=GetFILE( GCfBuff, GCfBuff, READMODE )) == NULL ) {
		WriteStd( "can't load runtime variable lists!\n" );
		exit( );
	}

	while ( ReadS( GInBuff, GIBUFFSZ, fptr, &retlen ) != EOF ) {
		if ( *GInBuff == COMCHAR )
			continue;
		else if ( strncmp( GInBuff, "NUM-VARS", 8) == 0 )
			break;
	}

	sscanf( GInBuff, "NUM-VARS %d", &RTVarsSz );
	if ( RTVarsSz <= 0 ) {
		sprintf( GOutBuff, "\nNUM-VARS line not found or bad in file '%s'.", GCfBuff );
		WriteStd( GOutBuff );
		exit();
	}
	
	RTVars = (struct RTVarNd *) calloc( RTVarsSz+10, sizeof(struct RTVarNd) );
	if ( RTVars == NULL ) {
		WriteStd("\nERR InitVT: calloc->NULL to RTVars!");
		exit( );
	}
	RTVarNxt = RTVars;
	nmloaded = 0;

		/*	ready to read lines of the form:
				n vname[,alias]/ T[SL] / values
			where:
			"n" is the code stored in RTVarAdd (in USERCMD.DEF) for the variable named 'vname',
				with the (optional) alternative name 'alias';
			"T" is its type (s=short, u=unsigned int, i=int, f=float, c=char);
			"S" is 's' if Special flag is to be set (or nothing if not special);
			"L" is 'l' if variable is to be dumped/loaded on save/restart (or nothing if not);
			"values" is either: 
				low,high 	value pair for numeric variables for range checks
							('high' can also be 'u' (USINTMAX), 'i' (INTMAX), 'b' (BIGFLOAT),
							 'c' (CFLSTSZ), or 'm' (MSGLSTSZ); 
				default 	char value for string variables (e.g., LogFName default).
		*/
		
	while ( ReadS( GInBuff, GIBUFFSZ, fptr, &retlen ) != EOF  && nmloaded < RTVarsSz )	{
		if ( *GInBuff == COMCHAR )
			continue;
		else {		/* let read a line */
			for ( cp = GInBuff; *cp != '\0'; ++cp )
				*cp = isupper( *cp ) ? tolower( *cp ) : *cp;
			cp = GetUInt( GInBuff, &vcode, 0, " ", &err );	/* get the code */		
			if ( err ) {
				sprintf( GOutBuff, "\nERR InitVT: vcode in '%s'.", GInBuff );
				WriteStd( GOutBuff );
			}
			else {
				for ( i = 0; i < 16; ++i )
					vname[i] = alias[i] = '\0';
				for ( i = 0; i < 4; ++i )
						vflags[i] = '\0';
					
				cp = StpTok1 ( cp, vname, 16, ",/" );		/* get the name itself--cp points to end-char */
				if ( *cp == ',' )							/* get an alias for it */
					cp = StpTok2 ( ++cp, alias, 16, "/" );
				else if ( *cp != '\0' )
					++cp;
				
				cp = StpTok2 ( cp, vflags, 4, "/" );		/* get the type and load flags */
				if ( strlen( vflags ) != 2 || ( vflags[0] != 's' && vflags[0] != 'i' && vflags[0] != 'u'
						&& vflags[0] != 'f' && vflags[0] != 'c' )  ) {
					sprintf( GOutBuff, "\nERR InitVT: vflags ('%s') in '%s'.", vflags, GInBuff );
					WriteStd( GOutBuff );
				}
				else {
					if ( vflags[0] != 'c' ) {
						cp = GetFloat( cp, &vmin, 0.0, ",", &err );	/* get min and max for non-char vars */
						for ( ; *cp == ' ' && *cp != '\0'; ++cp ) ; /* skip blanks */
						switch ( *cp )
						{	case 'u':	/* first some special constants */
								vmax = USINTMAX;
								break;
							case 'i':
								vmax = INTMAX;
								break;
							case 'b':
								vmax = BIGFLOAT;
								break;
							case 'm':
								vmax = MSGLSTSZ;
								break;
							case 'c':
								vmax = CFLSTSZ;
								break;
							default:	/* should be a numeric maximum value */
								cp = GetFloat( cp, &vmax, 0.0, "/", &err1 );
								if ( err || err1 )
								{	sprintf( GOutBuff, "\nERR InitVT: vmin/max in '%s'.", GInBuff );
									WriteStd( GOutBuff );
								}
								break;
						}	/* endswitch */
					}
					
					else {		/* else it is c char type, so save default value if there */
						for ( ; *cp != '\0' && *cp == ' '; ++cp ) ; /* skip the blanks */
						chardft = cp;
					}

						/* we have all the values, so lets store them in the next RTVars node */

					vname[16] = '\0';	/* just to be sure */
					for ( cp = vname; *cp != ' ' && *cp != '\0'; ++cp ) ;	/* find first blank or end */
					*cp = '\0';
					RTVarNxt->Name = malloc( strlen( vname ) + 1 );
					if ( RTVarNxt->Name == NULL ) {
						sprintf( GOutBuff, "ERR InitVT: malloc->NULL for vname (in '%s').", GInBuff );
						WriteStd( GOutBuff );
						exit( );
					}
					strcpy( RTVarNxt->Name, vname );
					
					alias[16] = '\0';	/* just to be sure */
					for ( cp = alias; *cp != ' ' && *cp != '\0'; ++cp ) ;	/* find first blank or end */
					*cp = '\0';
					RTVarNxt->Alias = malloc( strlen( alias ) + 1 );
					if ( RTVarNxt->Alias == NULL )
					{	sprintf( GOutBuff, "ERR InitVT: malloc->NULL for Alias (in '%s').", GInBuff );
						WriteStd( GOutBuff );
						exit( );
					}
                    if ( strlen( alias ) > 0 ) {
						strcpy( RTVarNxt->Alias, alias );
					}
                    else {
                        RTVarNxt->Alias = '\0';
                    }

					RTVarNxt->Type = vflags[0];

					if ( vflags[2] == 'l' )
						RTVarNxt->Load = 'L';
					else
						RTVarNxt->Load = 0;

					if ( RTVarNxt->Type != 'c' ) {	/* its numeric, so lets store min/max and find its address */
						RTVarNxt->Low = vmin;
						RTVarNxt->High = vmax;
							/* lets find address of the variable itself */
						for ( i = 0; RTVarAdd[i].Addr != 0; ++i )	/* end-of-table is addr=0 */
							if ( vcode == RTVarAdd[i].Code )
								break;
						if ( RTVarAdd[i].Addr == 0 ) {
							sprintf(GOutBuff,"\nERR InitVT: vcode not in RTAddNd table (USERCMD.DEF).\n(init.cfs line='%s')."
								,GInBuff);
							WriteStd( GOutBuff );
						}
						else {
							RTVarNxt->Addr = RTVarAdd[i].Addr;	/* got it! */
							++RTVarNxt; 						/* get ready for next one */
							++nmloaded;
						}
					}
					else {		/* its type c, so store a default value if one is there. */
						if ( strlen( chardft ) >= FNAMESZ ) {
							sprintf( GOutBuff, "\nERR InitVT: char default too long (in '%s').", GInBuff );
							WriteStd( GOutBuff );
						}
						else {
								/* lets get address in RTVarNxt pointing to proper spot... */
							if ( strcmp( RTVarNxt->Name, "logfname" ) == 0 )
							{	RTVarNxt->Addr = LogFName;
								if ( *chardft != '\0' )  strcpy ( LogFName, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "genfname" ) == 0 )
							{	RTVarNxt->Addr = GenFName;
								if ( *chardft != '\0' )  strcpy ( GenFName, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "stdinfn" ) == 0 )
							{	RTVarNxt->Addr = StdInFN;
								if ( *chardft != '\0' )  strcpy ( StdInFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "stdoutfn" ) == 0 )
							{	RTVarNxt->Addr = StdOutFN;
								if ( *chardft != '\0' )  strcpy ( StdOutFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "disoutfn" ) == 0 )
							{	RTVarNxt->Addr = DisOutFN;
								if ( *chardft != '\0' )  strcpy ( DisOutFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "msginfn" ) == 0 )
							{	RTVarNxt->Addr = MsgInFN;
								if ( *chardft != '\0' )  strcpy ( MsgInFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "cfinfn" ) == 0 )
							{	RTVarNxt->Addr = CfInFN;
								if ( *chardft != '\0' )  strcpy ( CfInFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "loadsyfn" ) == 0 )
							{	RTVarNxt->Addr = LoadSyFN;
								if ( *chardft != '\0' )  strcpy ( LoadSyFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "savesyfn" ) == 0 )
							{	RTVarNxt->Addr = SaveSyFN;
								if ( *chardft != '\0' )  strcpy ( SaveSyFN, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "savesypa" ) == 0 )
							{	RTVarNxt->Addr = SaveSyPa;
								if ( *chardft != '\0' )  strcpy ( SaveSyPa, chardft );
							}
							else if ( strcmp( RTVarNxt->Name, "envinfn" ) == 0 )
								{	RTVarNxt->Addr = EnvInFN;
								if ( *chardft != '\0' )  strcpy ( EnvInFN, chardft );
							}
							else
							{	sprintf( GOutBuff, "\nERR InitVT: unknown c var name (in '%s').", GInBuff );
								WriteStd( GOutBuff );
							}
							
							++RTVarNxt;
							++nmloaded;
						}
					}
				}
			}
		}
	}

	if ( nmloaded != RTVarsSz ) {
		sprintf( GOutBuff, "\nWARNING: RTVarsSz specified was %d, but %d loaded.\n", RTVarsSz, nmloaded );
		WriteStd( GOutBuff );
	}
	
	RTVarsSz = nmloaded;

	nmleft = 0; 
	while ( ReadS( GInBuff, GIBUFFSZ, fptr, &retlen ) != EOF )
		if ( *GInBuff != COMCHAR )
			++nmleft;

	if ( nmleft != 0  ) {
		sprintf( GOutBuff, "\n**In init.cfs, %d vars not loaded.", nmleft );
		WriteStd( GOutBuff );
	}
	
	printf( "\nLoaded %d vars.", RTVarsSz );

	return( 0 );

}  /* InitVT */


/*******************************

InitCfLs - initialize classifier list. 

	On entry, CfLst points to block of memory big enough for CFLSTSZ nodes.
	
******/

VOID  InitCfLs ( )
{
	register int  i, j;

	for ( i = 1, CurCfs = CfLst; i <= CFLSTMX; ++i, ++CurCfs ) {
		CurCfs->Cf_Id  = 0;
		for ( j = 0; j < INTPRSTR; ++j ) {
			CurCfs->Cnd1Bits[j] = 0;
			CurCfs->Cnd1DCs[j]	= 0;
			CurCfs->Cnd2Bits[j] = 0;
			CurCfs->Cnd2DCs[j]	= 0;
			CurCfs->ActBits[j]	= 0;
			CurCfs->ActDCs[j]	= 0;
		}
		for ( j = 0; j < INTPRML; ++j )
			CurCfs->MMBitsC1[j] = CurCfs->MMBitsC2[j] = 0;
		CurCfs->NxtCand = NULL;
		CurCfs->NmMtch	= CurCfs->NmMMC1 = CurCfs->NmMMC2 = CurCfs->NmProd = CurCfs->NmPost = 0;
		CurCfs->TotNmBid = CurCfs->TotMtch = CurCfs->TotProd = CurCfs->TotPost = 0;
		CurCfs->TotEMtch = CurCfs->TotEAct = CurCfs->TotPosRw = CurCfs->TotNegRw = 0;
		CurCfs->ChngStr = 0;
		CurCfs->ReplFlg = CurCfs->NoBBFlg = CurCfs->NoRplFlg = CurCfs->NoPrnFlg = FALSE;
	}

	NmCfs	 = 0;									 /* No classifiers yet. */
	NxtFrCf  = CfLst;								 /* Next free node starts at beginning of memory-block. */
	CurCfs	 = CfLst;								 /* When classifiers added, they will start here. */
	StrSrtCf = NULL;								 /* Non sorted on strength yet. */

	TotCfStr = AveCfStr = TotCfBR = AveCfBR = 0.0;

}  /* InitCfLs */


/*******************************

LoadSys 	  Load system start-state from the specified (open) file.

  Just call subroutines to load the various system parts, i.e.,
  the messages lists, classifiers, environment state, and system variables.

******/

VOID  LoadSys( FileName, FilePtr )
	char  *FileName;
	FILE  *FilePtr;
{
	
	ReadMsgs( FilePtr, 'r', 'o' );				 /* load the old message list */
	ReadMsgs( FilePtr, 'r', 'c' );				 /* load the current message list */
	ReadCfs( FilePtr, 'r' );					 /* load the  classifiers */
	LinkCMs( ); 								 /* Replace Msg and Classifer id's with pointers (where appropriate) */

	ReadEnv( FilePtr );

	ReadVars( FilePtr );

	if ( CloseFILE( FileName, "", FilePtr ) == ERROR ) {
		WriteStd("--system not properly loaded.\n\n");
		exit( );
	}

}  /* LoadSys */

/*******************************

ReadVars	Read values for setable core variables from a file.

FilePtr 	FILE pointer to already opened file/device.

Each input lines should be of the form:
	varname=value
where 'varname' is the name of a variable in the CVTable of core variables (see USERCMD.DEF),
and 'value' is the value to be assigned.
(Lines beginning with a semicolon are ignored as 'comment' lines.)

******/

VOID  ReadVars ( FilePtr )
	FILE  *FilePtr;
{
	int	retlen;			
	
		/* Read lines from input, call SetVar to do the assignment. Stop on end-of-file or "END-VARIABLES" string. */
		
	while ( ReadS( GInBuff, GIBUFFSZ, FilePtr, &retlen) != EOF )  {
		if ( *GInBuff == COMCHAR )			/* ignore comments */
			continue;

		else if ( strcmp( GInBuff, "END-VARIABLES" ) == 0 ) /* thats all, folks  */
			break;

		else
			SetVar( GInBuff, 'l' ); 		/* The 'l' means respect the CVLoad flag for the variable. */

	}
	
}  /* ReadVars  */


/*******************************

LinkCMs 	  Link messages and classifiers.

  When the system is saved by SaveSys(), the id's of messages and classifiers
  are written to indicate:
	- which classifier produced a message  (*Producer in MsgNodes).
	- which messages (if any) were used by that classifier to produce the message (*MtchMsg1 and 2 in MsgNodes).
  When the system is later loaded by LoadSys(), those id's are stored in the indicated pointer variables.

  LinkCMs() goes back and gets addresses for the messages and classifiers
  with those id numbers, and stores those addresses in the proper pointer variables.
  NOTE that the MtchMsg1's and 2's are messages in the OldMsgs list.

  This is all required so that the bucket-brigade will work after a SaveSys/LoadSys cycle.

******/

VOID  LinkCMs ( )
{
	unsigned int		i;
	struct	MsgNode		*curmsg, *GetMsgAd();
	struct	CfNode		*GetCfAd();
	extern	struct	MsgNode  *CurMsgs;

	for ( i = 1, curmsg = CurMsgs; i <= NmCMsgs; ++i, ++curmsg )  {
		if ( curmsg->Producer != NULL ) 
			if ( (curmsg->Producer = GetCfAd( (unsigned int) curmsg->Producer )) == NULL )	{
				sprintf( GOutBuff, "LinkCMs couldn't find Producer for Msg %u. Storing NULL.\n",
							curmsg->Msg_Id );
				WriteStd( GOutBuff );
			}
		if ( curmsg->MtchMsg1 != NULL )
			if ( (curmsg->MtchMsg1 = GetMsgAd( (unsigned int) curmsg->MtchMsg1, 'o')) == NULL ) {
				sprintf( GOutBuff, "LinkCMs couldn't find MtchMsg1 for Msg %u. Storing NULL.\n",
							curmsg->Msg_Id );
				WriteStd( GOutBuff );
			}
		if ( curmsg->MtchMsg2 != NULL )
			if ( (curmsg->MtchMsg2 = GetMsgAd( (unsigned int) curmsg->MtchMsg2, 'o')) == NULL ) {
				sprintf( GOutBuff, "LinkCMs couldn't find MtchMsg2 for Msg %u. Storing NULL.\n",
							curmsg->Msg_Id );
				WriteStd( GOutBuff );
			}
	}

}  /*  LinkCMs  */


/*******************************

GetMsgAd	  Get address of MsgNode that contains message with specified Id.

  Id		  Id of message to look for.

  List		  List to search. Currently this is always 'o' -- search the OldMsgs list.

  Return:	  Pointer to MsgNode found, or NULL (and print message) if none found.

******/

struct	MsgNode  *GetMsgAd ( Id, List )
	unsigned int  Id;
	char		  List;
{
	unsigned int	  i;
	struct	MsgNode  *mptr;
	extern	struct	MsgNode  *OldMsgs, *CurMsgs;
	extern	unsigned int  NmOMsgs, NmCMsgs;

	if ( List == 'o' ) { 
		for ( i = 1, mptr = OldMsgs; i <= NmOMsgs; ++i, ++mptr )
			if ( mptr->Msg_Id == Id )
				return( mptr );
	}
	else
		WriteStd( "\n\nBUG (GetMsgAd): illegal List specified.\n\n");

	sprintf( GOutBuff, "\n\nERR (GetMsgAd): can't find message with Msg_Id %u.\n", Id );
	WriteStd( GOutBuff );
	return( NULL ); 

}  /* GetMsgAd */


/*******************************

GetCfAd 	  Get address of CfNode that contains classifer with specified Id.

  Id		  Id of classifier to look for.

  Return:	  Pointer to CfNode found, or NULL (and print message) if none found.

******/

struct	CfNode	*GetCfAd ( Id )
	unsigned int  Id;
{
	unsigned int	  i;
	struct	CfNode	*cptr;
	extern	struct	CfNode	*CurCfs;
	extern	unsigned int  NmCfs;

	for ( i = 1, cptr = CurCfs; i <= NmCfs; ++i, ++cptr )
		if ( cptr->Cf_Id == Id )
			return( cptr );

	sprintf( GOutBuff, "\n\nERR (GetCfAd): can't find classifier with Cf_Id %u.\n", Id );
	WriteStd( GOutBuff );
	return( NULL ); 

}  /* GetCfAd */


/*******************************

SaveSys 	  Save entire system state, in format that LoadSys can use.

  FDName	  Name of file to be used for output.
			  If its a null string (""), write to next autosave file.

******/ 

int  SaveSys ( FDName )
	char  *FDName;
{
	FILE	*fileptr;
	char	savefn[FNAMESZ];

	while ( *FDName == ' ' )  ++FDName; 				/* Skip blanks */

	if	( *FDName == '\0' )  {						/* No file specified, so construct one */
		sprintf( savefn, "%s%s.%03u", SaveSyPa, SaveSyFN, SaveFNum );	  /* including the path and autosave number */
		SaveFNum++; 									/* increment it now for next use */
		FDName = savefn;								/* and that's what we'll try to use. */
	}

	if ( (fileptr = GetFILE( FDName, FDName, WRITEMODE )) == NULL ) 
		return( ERROR );

	fprintf( fileptr, ";  CONSTANTS\n" );
	WrtCons( fileptr );

	fprintf( fileptr, ";  OLD-MSGLIST\n");
	WrtMsgs( fileptr, SAVEFMT, 'o' );

	fprintf( fileptr, ";  CUR-MSGLIST\n");
	WrtMsgs( fileptr, SAVEFMT, 'c' );

	fprintf( fileptr, ";  CLASSIFIERS\n" );
	WrtCfs( fileptr, SAVEFMT );

	WrtEnv( fileptr, SAVEFMT );

	fprintf( fileptr, ";  VARIABLES\n" );
	WrtVars( fileptr, SAVEFMT );

	CloseFILE( FDName, FDName, fileptr );

	return( OK );

}  /* SaveSys */

/*******************************

WrtCons 	  Write system constants and other non-settable items that serve
			  to define the state of the classifier system during a run.

  FilePtr	  FILE pointer to open file/device.

******/

VOID  WrtCons ( FilePtr )
	FILE  *FilePtr;
{
	
	fprintf( FilePtr, ";  Run-Date:  %s.  Run-Time:  %s.\n", RunDate, RunTime );

	fprintf( FilePtr, ";  STRNGSZ  (message-length):			 %d\n", STRNGSZ );
	fprintf( FilePtr, ";  MSGLSTSZ (max. number of msgs):		%d\n", MSGLSTSZ );
	fprintf( FilePtr, ";  CFLSTSZ  (max. number of classifers):  %d\n", CFLSTSZ );
	fprintf( FilePtr, ";  EFFLSTSZ (max. number of effectors):   %d\n", EFFLSTSZ );
	fprintf( FilePtr, ";Conditional-compilation switch settings:\n" );
	fprintf( FilePtr, ";  CTEST   (test code):						 %d\n", CTEST );
	fprintf( FilePtr, ";  CDSC_ALL (Compile all discovery code):	   %d\n", CDSC_ALL );
	fprintf( FilePtr, ";  CRANDCFS (Compile GENRANDCFS command code):  %d\n", CRANDCFS );
	
	fprintf( FilePtr, ";END-CONSTANTS\n" ); 

}  /*  WrtCons  */


/*******************************

ReadEffs	  Read effector condition-strings from a file, and initialize effectors.

  FilePtr	  FILE pointer to open file/device with effector strings, one per line,
			  followed by a line with the string "END-EFFS" starting in column one.
			  The lines should look like these:
				;  effector-condition,	effector-name,	Total-Matches, TotalActivations
				m0101 0101 0101 0100, Guess Letter, 0, 0
				~#### #### #### ####, Default Eff, 0, 0
				END-EFFS

  Returns	  The number of effectors read and loaded.

******/

int  ReadEffs ( FilePtr )
	FILE  *FilePtr;
{
	char	buff[STRNGSZ+1], name[EFFNAMSZ+1], *lptr, *rptr, *StpTok2(), *GetUInt();
	int		retlen, nmefload, etotmtch, etotact, err, ecndtype;
	
	nmefload = 0;
	while ( ReadS( GInBuff, GIBUFFSZ, FilePtr, &retlen) != EOF )  {
		if ( *GInBuff == COMCHAR )
			continue;
		if ( strcmp( GInBuff, "END-EFFS" ) == 0 )
			break;
		 else if ( nmefload >= EFFLSTSZ ) {
			sprintf( GOutBuff, "\n\nWARNING (ReadEff): you are trying to load more effector conditions (%d)", nmefload );
			WriteStd( GOutBuff );
			sprintf( GOutBuff, "\n than the maximum allowed (%d).", EFFLSTSZ );
			WriteStd( GOutBuff );
			continue;
		}
	
		for ( lptr = rptr = GInBuff; *rptr != '\0'; ++rptr )	/* strip blanks */
			if ( *rptr != ' ' )		*lptr++ = *rptr;
		*lptr = '\0';

			/* check out this line, store parts in local variables */

		lptr = GInBuff;
		if ( *lptr == '~' )  {
			ecndtype = CNOTMATCH;						  /* NOT-MATCH condition 2 */
			++lptr; 									  /* move past the ~ */
		}	
		else  {
			ecndtype = CMATCH;							  /* MATCH Type */
			if ( *lptr == 'm' )		++lptr;				  /* move past the m if its there */
		}

		lptr = StpTok2( lptr, buff, STRNGSZ+1, ",");
		if ( !IsCondAct( buff ) ) {
			sprintf( GOutBuff, "\nERR (ReadEff): illegal effector condition:\n'%s'\nnot loaded.\n", GInBuff );
			WriteStd( GOutBuff );
			continue;
		}

		lptr = StpTok2( lptr, name, sizeof(name), ",");

		lptr = GetUInt( lptr, &etotmtch, 0, ",", &err );		
		if ( err == TRUE )	{
			sprintf(GOutBuff,"\nWARNING (ReadEff): Illegal ETotMtch on line:\n%s\nUsing 0.", GInBuff );
			WriteStd( GOutBuff );
		}

		lptr = GetUInt( lptr, &etotact, 0, ",", &err ); 	
		if ( err == TRUE )	{
			sprintf(GOutBuff,"\nWARNING (ReadEff): Illegal ETotAct on line:\n%s\nUsing 0.", GInBuff );
			WriteStd( GOutBuff );
		}

			/* We got it all, so store in the next effector node */

		strcpy( EffLst[nmefload].EffName, name );
		ACndtoB( buff, EffLst[nmefload].ECndBit, EffLst[nmefload].ECndDCs );
		EffLst[nmefload].ETotMtch	= etotmtch;
		EffLst[nmefload].ETotAct	= etotact;
		EffLst[nmefload].ECndType	= ecndtype;
		EffLst[nmefload].Eff_Id		= nmefload + 1;	/* Id's assigned in order */
		++nmefload;
	}
	
	return( nmefload );

} /* ReadEffs */


/*******************************

WrtEffs 	Write out effectors in format ready to be read by ReadEffs().

FilePtr 	FILE pointer to open file/device.

******/

VOID WrtEffs ( FilePtr )
	FILE	*FilePtr;
{
	struct	EffNode 		*eptr;
	int 					i;
	char					buff[STRNGSZ+1], m;
	extern	struct EffNode	EffLst[];
	extern	unsigned int	NmEffs;

	for ( eptr = EffLst, i = 1; i <= NmEffs; ++i, ++eptr ) {

		BCndtoA( eptr->ECndBit, eptr->ECndDCs, buff );
		buff[STRNGSZ] = '\0';
		
		if ( eptr->ECndType == CMATCH )
			m = 'm';
		else
			m = '~';

		fprintf( FilePtr, "%c%s, %s, %u, %u\n", m, buff, eptr->EffName, eptr->ETotMtch, eptr->ETotAct );

	}

	fprintf( FilePtr, "END-EFFS\n" );

} /* WrtEffs */


/*******************************

InitDate	Initialize RunDate, RunTime and SaveSyFN, based on time and date from system.
		
******/

VOID InitDate	( )
{
	int 	month, day, hour, minute;

	GetDate( RunDate ); 			/*	mm/dd/yy format */
	GetCFSCTime( RunTime, 0 );			/*	hh:mm	 format */

	RunDate[8] = '\0';				/* Just in case... */
	RunTime[5] = '\0';

		/*	Put into SaveSyFN a string of the form	"Smddhhmm.", where
				S		is constant, to make it easy to find all S files.
				m		is the RunDate month, in HEX
				dd		is the RunDate day. 
				hh		is the RunTime hour.
				mm		is the RunTime minute.
			Also put that into the LogFName, except start with L and end LOG,
			and into the GenFName, with the suffix GEN. 
		*/

	sscanf( RunDate, "%2d/%2d/", &month, &day );	
	sscanf( RunTime, "%2d:%2d", &hour, &minute );

	sprintf( SaveSyFN, "S%1x%02d%02d%02d", month, day, hour, minute );
	sprintf( LogFName, "L%1x%02d%02d%02d.LOG", month, day, hour, minute );
	sprintf( GenFName, "G%1x%02d%02d%02d.GEN", month, day, hour, minute );

} /* InitDate */
