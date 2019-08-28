
/*	 LETSEQ1.C	-- Letter Sequence Environment for the CFS-C classifier system.

This file, LETSEQ1.C, contains subroutines that define the LETSEQ1 environment
for the CFS-C classifier system.  The LETSEQ1 ("LETter SEQuence") environment
is described in "LETSEQ1: An implementation of the CFS-C Classifier System
in a Task-Domain that Involves Learning to Predict Letter Sequences."

(LETSEQ1 program and documentation, Copyright 1986,1988 Rick L. Riolo).

This file contains the following subroutines:

A. Subroutines required for all environments linked to the CFS-C system:
   LoadEnv	 |- Load environment and effectors from a file.
   ReadEnv	 |
   InitEnv	 - Initialize the environent variables.
   DsplEnv	 |- display environment variables, status, etc.
   WrtEnv	 |
   SaveEnv	 |
   GetDMsgs  - get new messages to be added to the message list, i.e., use state-of-environment and state-of-cfs in
			  that environement to determine what new messages are to be appended to the current message list.
   DoEnvCmd  - execute an environment command.
   GtSysRew  - Get reward for system, after activation of effectors.
   StoreSup  - get sum/high support for each symbol.
   MkGuess	 - Pick Guess based on support; advance window into letters.
   DftGuess  - make default guess if no effector activated.
   EQGuess	 - see if message is consistent with guess made.
   DoBkgBeh  - re-initialize attribute settings.
   MakeActS  - Make an action string.
   DisCfIE	 - Display classifiers, Interpreted wrt the Environment implemented.

B. Subroutines specific to the LETSEQ environment:
   ReadLetS  - load letters sequence and payoff function from file (called by ReadEnv).
   EnvAlloc, EnvFree - used to get/free space for environment objects.
   SameType  - return TRUE iff two letters are of same type (eg both vowels).
   GtLetTyp  - return int code for type of letter (vowel, consonant, etc.)
   Strg2Let( String ) = int form of a letter.
   Let2Strg( Let, String ) - with other loci zero.
   SetEnv	 - set environment variable (via ECMD command).
   HelpEnv	 - help specific to environment (via ECMD command).
   DsplDetM  - display next messages from detectors (via ECMD command).

C. An example of function to return rewards specific to classifiers.
   ISysRew	 - returns float reward that (possibly) unique to a classifier.

*******/

#include "config.h"

#include "utility.h"
#include "cfsio.ext"			/* Constants and variables for doing I/O */
#include "core.ext"			/* Declarations of MsgNode's, NewMsgs list, etc. */
#include "cfops.ext"			/* required for DsCfIE(), displaying cfs */
#include "letseq1.def"			/* the LETSEQ data structures */

char LCRight[] = "Copyright 1986,1988 Rick L. Riolo";

extern unsigned int DemoLev;		/* Control demonstration-output */

#if MPWC
#define __SEG__ CFSCINIT
#endif

/*********************************

LoadEnv 	  Load environment for LETSEQ from specified file.

  FDName	  String name of file from which initial state of environment is to be read.

  LoadEnv() just opens the file, calls ReadEnv()( to do the work, and closes the file.

******/

int 
LoadEnv (FDName)
    char *FDName;
{
    FILE *fileptr;
    extern char *EnvInFN;

    while (*FDName == ' ')
	++FDName;			/* strip blanks */

    if ((fileptr = GetFILE (FDName, EnvInFN, "r")) == NULL)
	return (ERROR);

    ReadEnv (fileptr);

    if (CloseFILE (FDName, EnvInFN, fileptr) == ERROR)
	return (ERROR);

    return (OK);

}					/* LoadEnv  */



/*********************************

ReadEnv 	  Load letters and effectors from a file.

  FilePtr	  Open FILE pointer.
			  Format of information in file is:
				; Rewards and flags:
				; SameLRew SameTRew WrongRew DftBehFlg NulGsMFlg EfRslMch
					50		 10 	  -10		 1		   0		0
				; Total number of each kind of Reward / total default guesses
					 0		   0		 0		 0
				; one or more lines with letters in sequence)
				abababab
				ENDLETSEQ
				; one or more effectors
				 10## #### #### ####, Guess Letter, 0, 0
				~10## #### #### ####, Default Guess, 0, 0
				END-EFFS

  This just calls ReadLetS() and ReadEff() to do the work.

******/

VOID 
ReadEnv (FilePtr)
    FILE *FilePtr;
{
    int StoreSup (), MkGuess (), EQGuess (), DftGuess ();	/* Make addresses resolvable */

    InitEnv ();				/* Get memory, initialize sequence list */

    ReadLetS (FilePtr);			/* Read in rewards and letter sequence itself */

    EffLst[0].Eff_Set = StoreSup;	/* Address of function to set effector */
    EffLst[0].Eff_Fn = MkGuess;		/* Address of MkGuess() subroutine */
    EffLst[0].EQEfAct = EQGuess;	/* Address of EQGuess subroutine */
    EffLst[0].CEffFlg = TRUE;		/* Apply DscCEff() to this effector to generate new actions */

    EffLst[1].Eff_Set = StoreSup;	/* this and next not used */
    EffLst[1].Eff_Fn = DftGuess;	/* Address of DftGuess() subroutine... */
    EffLst[1].EQEfAct = EQGuess;
    EffLst[1].CEffFlg = FALSE;		/* DscCEff() doesn't work on this effector */

    NmEffs = ReadEffs (FilePtr);	/* Call to read effector conditions, names, etc. until END-EFFS read. */

    sprintf (GOutBuff, "\nLoaded %d effectors.\n", NmEffs);
    WriteStd (GOutBuff);

}					/* ReadEnv */



/*********************************

InitEnv 	 Initialize the environment, for   LETSEQ environment.

  This just prints a message and gives back any EnvNode's that are
  on the circular letter-sesequence list.
	
******/

VOID 
InitEnv ()
{
    int i;
    struct EnvNode *lastlet, *newlet;

    WriteStd ("\nInitialize LETSEQ environment.");

    if (CurLet != NULL) {		/* Give back all the nodes */
	printf ("\nReturn nodes.");
	newlet = CurLet -> NxtEnvNd;
	while (newlet != CurLet) {
	    lastlet = newlet;
	    newlet = newlet -> NxtEnvNd;
	    EnvFree (lastlet);
	}
	EnvFree (CurLet);
	CurLet = NULL;
    }
    TotSymSu = HiSup = 0.0;
    GuessAtt = HiSupSym = NxtSym = 0;
    for (i = 0; i <= SYMSUPMX; ++i) {
	SymSup[i] = 0;
	Symbols[i] = -1;
    }

    NmDMsgMx = STMemSz;
    NmIMsgMx = MSGLSTSZ - NmDMsgMx;

}					/* InitEnv */



/*********************************

ReadLetS	  Read sequence of letters.
			
  FilePtr	  FILE pointer to opened file that has sequence of letters on one or more lines,
			  followed by  the string "ENDLETSEQ" starting in column one of a line.

******/

VOID 
ReadLetS (FilePtr)
    FILE *FilePtr;
{
    int i, col, RetLen;
    char EnvBuf[ENVLINSZ];
    struct EnvNode *lastlet, *newlet, *EnvAlloc ();
    struct EnvNode *firstlet = NULL;

    DetMsgMn = STMemSz;			/* Minimum space for detector messages. */

   /* Read in line with reward levels and flag settings  */

    while (ReadS (EnvBuf, ENVLINMX, FilePtr, &RetLen) != EOF) {
	if (EnvBuf[0] == COMCHAR || RetLen == 0)
	    continue;
	else {

#ifdef DOS
	    sscanf (EnvBuf, "%f %f %f %h %h %d",
#else
	    sscanf (EnvBuf, "%f %f %f %hd %hd %d",
#endif
		    &SameLRew, &SameTRew, &WrongRew, &DftBehFlg, &NulGsMFlg, &EfRslMch);
	    break;
	}
    }

   /* Read in line with total number of each type of guess  */

    while (ReadS (EnvBuf, ENVLINMX, FilePtr, &RetLen) != EOF) {
	if (EnvBuf[0] == COMCHAR || RetLen == 0)
	    continue;
	else {
	    sscanf (EnvBuf, "%d %d %d %d", &TotNmGsL, &TotNmGsT, &TotNmGsW, &TotNmDft);
	    break;
	}
    }

   /* Read in lines of letters from input file. Store each character on those lines in a node,
		   and when done, make it a circular list. Stop on end-of-file or ENDLETSEQ on a line.
		*/

    while (ReadS (EnvBuf, ENVLINMX, FilePtr, &RetLen) != EOF) {
	if (EnvBuf[0] == COMCHAR || RetLen == 0)
	    continue;
	else if (strcmp (EnvBuf, "ENDLETSEQ") == 0)
	    break;
	else {
	    for (col = 0; EnvBuf[col] != '\0'; ++col) {
		newlet = EnvAlloc ();
		newlet -> EnvLet = EnvBuf[col];
		if (firstlet == NULL)
		    firstlet = lastlet = newlet;	/* its first, and therefore last. */
		else {
		    lastlet -> NxtEnvNd = newlet;	/* its now last */
		    lastlet = newlet;
		}
	    }				/* one line read in */
	}
    }					/* there are lines to read */

    lastlet -> NxtEnvNd = firstlet;	/* Make it a cirular list. */

   /* Now load mark few letters as 'short term memory'. */

    OldLet = firstlet;			/* first mark the oldest */
    for (i = 2, CurLet = OldLet; i <= STMemSz; ++i)	/* Get pointer to most recent. */
	CurLet = CurLet -> NxtEnvNd;

    WriteStd ("\nInitial letters in STM (most recent last):  ");
    for (i = 0, newlet = OldLet; i <= STMemMx; ++i, newlet = newlet -> NxtEnvNd) {
	sprintf (GOutBuff, "%c ", newlet -> EnvLet);
	WriteStd (GOutBuff);
    }
    WriteStd ("\n");

}					/*  ReadLetS  */



/*********************************

EnvAlloc	  Call system routine alloc to get space for an EnvNode.
EnvFree 	  Give space for EnvNode back to system.

******/

struct EnvNode *
EnvAlloc ()
{
    struct EnvNode *enode;

    if ((enode = (struct EnvNode *) malloc (sizeof (struct EnvNode))) == NULL) {
	WriteStd ("\nEnvAlloc: NULL from malloc()!\n");
	exit (ERROR);
    }
    return (enode);

}					/* EnvAlloc */

/********************************

  EnvFree -- give back environment nodes.

******/

int 
EnvFree (enode)
    struct EnvNode *enode;
{

#if APOLLOC
    if (free ((char *) enode) == 0) {	/* Apollo C free() return 0 means error (I think) */
	WriteStd ("\nEnvFree: zero from by free()\n");
	exit (ERROR);
    }
#else

#if CBELLMTS || SUN3
    free ((char *) enode);
#else
    if (free ((char *) enode) != 0) {
	WriteStd ("\nEnvFree: non-0 from free()\n");
	exit (ERROR);
    }
#endif

#endif

    return (OK);

}					/* EnvFree */

#if MPWC
#define __SEG__ CFSCWRITE
#endif


/*********************************

DsplEnv 	  Display environment variable values, status, etc.

  FDName	  Name of destination file/device.

  Format	  Control format of display (or turn it off).

******/

int 
DsplEnv (FDName, Format)
    char *FDName;
    int Format;
{
    FILE *destin, *GetFILE ();
    extern char *DisOutFN, *StdOutFN;
    extern FILE *DisOFILE, *StdOFILE;

    if (strcmp (FDName, DisOutFN) == 0 || *FDName == '\0')
	destin = DisOFILE;		/* DisOutFN is already open */
    else if (strcmp (FDName, StdOutFN) == 0)
	destin = StdOFILE;		/* StdOutFN is already open */
    else if ((destin = GetFILE (FDName, "", WRITEMODE)) == NULL)
	return (ERROR);

    WrtEnv (destin, Format);

    if (destin != DisOFILE && destin != StdOFILE)	/* Dont close these streams! */
	CloseFILE (FDName, "", destin);

    return (OK);

}					/* DsplEnv	*/



/*********************************

WrtEnv		   Write environment state to open file/device.

  Destin	   Opened file/device.

  Format	   Control format of display.

******/

int 
WrtEnv (FilePtr, Format)
    FILE *FilePtr;
    int Format;
{
    int i;
    struct EnvNode *letp;
    extern unsigned int CycleStp;

    if (CurLet == NULL) {
	WriteS ("\nNo environment loaded yet!\n", FilePtr);
    }
    if (Format == SAVEFMT)
	SaveEnv (FilePtr);

    else if (Format == 1) {
	if (GuessLet != '\0')
	    sprintf (GOutBuff, "\nGuess is '%c', letter is '%c'.\n", GuessLet, CurLet -> EnvLet);
	else
	    sprintf (GOutBuff, "\nGuess is NULL, letter is '%c'.\n", CurLet -> EnvLet);
	WriteS (GOutBuff, FilePtr);
    } else if (Format == 2) {
	sprintf (GOutBuff, "\nTotNmGsL %4u, TotNmGsT %4u, TotNmGsW %4u (CycleStp %u) [TotNmDft %d]\n",
		 TotNmGsL, TotNmGsT, TotNmGsW, CycleStp, TotNmDft);
	WriteS (GOutBuff, FilePtr);
    } else {
	WriteS ("\n\nEnvironment status:", FilePtr);
	sprintf (GOutBuff, "\nTotNmGsL %4u, TotNmGsT %4u, TotNmGsW %4u  (CycleStp %u) [TotNmDft %d].\n",
		 TotNmGsL, TotNmGsT, TotNmGsW, CycleStp, TotNmDft);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\nReward for correct letter:	%6.1f", SameLRew);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nReward for correct type only: %6.1f", SameTRew);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nReward for incorrect guess:   %6.1f", WrongRew);
	WriteS (GOutBuff, FilePtr);

	WriteS ("\nDefault guessing is:			 ", FilePtr);
	if (DftBehFlg)
	    WriteS ("ON.", FilePtr);
	else
	    WriteS ("OFF.", FilePtr);
	WriteS ("\nDisplay NULL-Guess message is:   ", FilePtr);
	if (NulGsMFlg)
	    WriteS ("ON.", FilePtr);
	else
	    WriteS ("OFF.", FilePtr);
	WriteS ("\nEffector resolution uses:		", FilePtr);
	if (EfRslMch == EFRHIBID)
	    WriteS ("Highest Bid", FilePtr);
	else if (EfRslMch == EFRHISUM)
	    WriteS ("Highest Support", FilePtr);
	else
	    WriteS ("Prob. Prop. to Support", FilePtr);

	WriteS ("\n\nSTM (most-recent on right):", FilePtr);
	for (i = 1, letp = OldLet; i <= STMemSz; ++i, letp = letp -> NxtEnvNd) {
	    sprintf (GOutBuff, " %c", letp -> EnvLet);
	    WriteS (GOutBuff, FilePtr);
	}

	WriteS ("\n\nAll Letters (STM on left):\n", FilePtr);
	letp = OldLet;
	sprintf (GOutBuff, "\n  %c", letp -> EnvLet);
	WriteS (GOutBuff, FilePtr);
	for (letp = OldLet -> NxtEnvNd; letp != OldLet; letp = letp -> NxtEnvNd) {
	    sprintf (GOutBuff, " %c", letp -> EnvLet);
	    WriteS (GOutBuff, FilePtr);
	}
	WriteS ("\n", FilePtr);
    }

    return (OK);

}					/* WrtEnv */



/*********************************

SaveEnv 	  Write environment state in form ready for reload by LoadEnv().

  FilePtr	  FILE pointer to already open file to write to.

******/

VOID 
SaveEnv (FilePtr)
    FILE *FilePtr;
{
    struct EnvNode *letp;
    extern struct EffNode EffLst[];
    extern char *GOutBuff;

    fprintf (FilePtr, "; LETSEQ Environment state:\n");
    fprintf (FilePtr, " %f %f %f %d %d %d\n", SameLRew, SameTRew, WrongRew, DftBehFlg, NulGsMFlg, EfRslMch);
    fprintf (FilePtr, " %d %d %d %d\n", TotNmGsL, TotNmGsT, TotNmGsW, TotNmDft);
    letp = OldLet;
    fprintf (FilePtr, "%c", letp -> EnvLet);
    for (letp = OldLet -> NxtEnvNd; letp != OldLet; letp = letp -> NxtEnvNd)
	fprintf (FilePtr, "%c", letp -> EnvLet);
    fprintf (FilePtr, "\nENDLETSEQ\n");

    WrtEffs (FilePtr);			/* This writes the effectors and END-EFFS terminator */

}					/* SaveEnv  */

#if MPWC
#define __SEG__ Main
#endif



/*********************************

GetDMsgs	  Get messages from detectors to be added to the message-list.

  MStrings	  Char array to hold ALL messages, end to end.
			  Each message in the array is terminated by a '\0'.
			  There is room for DMSGSMX messages -- see CORE.H for DMSGSMX definition.

  MIntens	  Point to first of array of floats, each one the
			  'Intensity' to store with the corresponding message strings pointed to
			  by the MStrings array. (Room for DMSGSMX of these, too.)

  Return:  Number of messages from detectors.

*****/

int 
GetDMsgs (MStrings, MIntens)
    char MStrings[];
    float MIntens[];
{
    struct EnvNode *let;
    int i, letnum;
    char *nxtmsg, mess[STRNGSZ + 1];
    int NumMsgs = 0;

   /*	SEQLET environment: Map OldLet ... CurLet values into messages.
			There are LETMEMSZ entries to map, but only the first STMemSz are mapped into messages.
			CurLet is pointer to letter just read in, and OldLet is pointer to oldest
			letter previously read in that is mapped into a message.
		*/

    for (let = OldLet, letnum = STMemMx, nxtmsg = MStrings; letnum >= 0; --letnum, let = let -> NxtEnvNd, ++NumMsgs) {
	for (i = 0; i <= STRNGMX; ++i)
	    mess[i] = '0';
	Let2Strg (let -> EnvLet, &mess[STRNGSZ - STRLETSZ]);
	mess[STRNGSZ] = '\0';
	mess[0] = '0';
	mess[1] = '0';			/* Mark as message from detector */
	Int2Loci (letnum, &mess[2], 3);	/* Mark 'recency' of this letter, 0 is most recent. */
	strcpy (nxtmsg, mess);
	nxtmsg = &MStrings[(STRNGSZ * (NumMsgs + 1)) + (NumMsgs + 1)];
	MIntens[NumMsgs] = MsgIntDf;	/* Just use default message intenisty. */
    }

#if CTEST
    if (DemoLev == DEMDEBUG) {
	WriteStd ("\nShort term letter memory, from oldest to latest (on right): ");
	for (i = 0, let = OldLet; i < STMemSz; ++i, let = let -> NxtEnvNd) {
	    sprintf (GOutBuff, " %c", let -> EnvLet);
	    WriteStd (GOutBuff);
	}
    }
#endif

    return (NumMsgs);

}					/* GetDMsgs */



/*********************************

StoreSup	  Subroutines to store support in set specified by the effector message.

  MsgPtr	  The message that activated this effector.

  Add support to each effector value specified by Msg (&increment total support).
  'Support' is the biased bid of classifier that produced this message.

  Symbols supported so far are stored in the array Symbols[0...NxtSym-1],
  in strictly binary form of rightmost STRLETSZ loci of EMsg.
  Support for each is in parallel spot in the SymSup array.
  So, if the symbol specified by EMsg is not found, add it at NxtSym position;
  otherwise increment support for symbol when its already in the array.
  Also store highest Support value seen so far, and the symbol supported.

******/

int 
StoreSup (MsgPtr)
    struct MsgNode *MsgPtr;
{
    register unsigned int s, i;
    register float support;
    char EMsg[STRNGSZ + 1];

    BMsgtoA (MsgPtr -> Message, EMsg);	/* Get ascii version of msg */
    EMsg[STRNGSZ] = '\0';
    support = MsgPtr -> Producer -> CfBBid;	/* use biased bid value */

    s = Loci2Int (&EMsg[STRNGSZ - STRLETSZ], STRLETSZ);

    for (i = 0; i < NxtSym; ++i)
	if (s == Symbols[i])
	    break;
    if (i == NxtSym) {			/* its new */
	SymSup[NxtSym] = support;	/* store support */
	Symbols[NxtSym++] = s;		/* store symbol in next array entry, advance NxtSym index */
    } else
	SymSup[i] += support;		/* its already there */
    TotSymSu += support;

    if (support > HiSup) {
	HiSup = support;
	HiSupSym = s;
    }
    return (OK);

}					/* StoreSup */



/********************************

MkGuess 	  Make guess based on support stored for various symbols.

  BehavStp	  If TRUE, advance the window and map GuessAtt -> GuessLet.

  The value of EfRslMch determines how to choose the guess:
	EFRHIBID - use symbol supported by highest bidder, HiSupSym.
	EFRHISUM - use symbol with greatest SymSup value.
	EFRPRSUM - pick symbol probabilistically, proportinal to SymSup values.
  The values in SymSup, TotSymSu, HiSupSym are already set, by StoreSup().

******/

int 
MkGuess (BehavStp)
{
    register int s;
    register float ran, cum;
    char buff[STRNGSZ + 1];
    extern unsigned int DemoLev;

    switch (EfRslMch) {
     case EFRHIBID:
	 GuessAtt = HiSupSym;
	 break;
     case EFRHISUM:
	 HiSupSym = 0;
	 HiSup = SymSup[0];
	 for (s = 1; s < NxtSym; ++s)
	     if (SymSup[s] > HiSup || (SymSup[s] == HiSup && 0.5 > URand01 ())) {
		 HiSup = SymSup[s];
		 HiSupSym = s;
	     }
	 GuessAtt = Symbols[HiSupSym];
	 break;
     default:
	 ran = TotSymSu * URand01 ();

#if CTEST
	 if (DemoLev > 0) {
	     if (DemoLev > 1) {
		 WriteStd ("\nSymbol  Support");
		 for (s = 0, cum = 0.0; s < NxtSym; ++s) {
		     sprintf (GOutBuff, "\n%x	%6.1f", Symbols[s], SymSup[s]);
		     WriteStd (GOutBuff);
		 }
	     }
	     sprintf (GOutBuff, "\nMkGuess: ran is %f, TotSymSu is %f", ran, TotSymSu);
	     WriteStd (GOutBuff);
	 }
#endif

	 for (s = 0, cum = 0.0; s < NxtSym; ++s) {
	     cum += SymSup[s];
	     if (ran <= cum)
		 break;
	 }

	 GuessAtt = Symbols[s];

#if CTEST
	 if (DemoLev > 0) {
	     sprintf (GOutBuff, "  Symbol is %c (x %x) (cum %f).", GuessAtt, GuessAtt, cum);
	     WriteStd (GOutBuff);
	 }
#endif

	 break;
    }					/* switch	*/

    if (BehavStp) {
	Int2Loci (GuessAtt, &buff[STRNGSZ - STRLETSZ], STRLETSZ);
	GuessLet = Strg2Let (buff);
	OldLet = OldLet -> NxtEnvNd;
	CurLet = CurLet -> NxtEnvNd;
    }
    return (OK);

}					/* MkGuess */



/*********************************

DftGuess	  Make default guess, ie., do this when do nothing else.

  If  DftBehFlg  set to TRUE, { guess random letter.

  If DftBehFlg is FALSE, do nothing.

******/

int 
DftGuess (BehavStp)
    int BehavStp;
{
    char buff[STRNGSZ + 1];

    if (DftBehFlg) {
	++TotNmDft;
	GuessAtt = URandN (BINLETMX);
	if (NulGsMFlg)
	    WriteStd ("\nDefault guess made.");
    }
    if (BehavStp) {
	Int2Loci (GuessAtt, &buff[STRNGSZ - STRLETSZ], STRLETSZ);
	GuessLet = Strg2Let (buff);
	OldLet = OldLet -> NxtEnvNd;
	CurLet = CurLet -> NxtEnvNd;
    }
    return (OK);

}					/* DftGuess */



/*********************************

EQGuess 	  See if message is consistent with guess actually made.

  EMsg		  Null terminated ascii form of message to compare.

  RETURN	  TRUE if the message is consistant with actual guess.
			  FALSE otherwise.

******/

int 
EQGuess (EMsg)
    char EMsg[];
{
    register int i;

    i = Loci2Int (&EMsg[STRNGSZ - STRLETSZ], STRLETSZ);

    if (i == GuessAtt)
	return (TRUE);

    return (FALSE);

}					/* EQGuess */



/*********************************

DoBkgBeh	  This just does initialization for next step.

******/

VOID 
DoBkgBeh ()
{
    register int i;

    GuessAtt = HiSupSym = NxtSym = 0;
    TotSymSu = HiSup = 0.0;
    for (i = 0; i <= SYMSUPMX; ++i) {
	SymSup[i] = 0;
	Symbols[i] = -1;
    }

}					/* end DoBkgBeh */



/*********************************

GtSysRew	   Get reward from environment, if any to be got.
			   Set global variable SysRew to that value ( to 0 if none ).

  For LETSEQ1 default environment, there are 3 rewards, based on a comparison of:
	   GuessLet 		- letter guess of system, from effectors.
	   LetMem[CurLet]	- letter just read in from 'environment'.
  The rewards:
	1. SameLRew, given if guess is exactly the input letter.
	2. SameTRew, given if guess is same 'type' as next letter, where 'type' is
	   defined by the SameType function.
	3. WrongRew, given in all other cases.

  NOTE: This also sets the MadeMstk variable (CORE.DEF) to TRUE if guess is not correct letter.
		This is used to trigger the 'Cover Effector' discovery mechanism.

******/

int 
GtSysRew ()
{
    extern unsigned int CycleStp;

    MadeMstk = FALSE;			/* assume no mistake made */

    if (CycleStp % EffectRt != 0)	/* Get reward only when Generate behavior, every EffectRt steps */
	SysRew = 0.0;

    else {

#if CTEST
	if (DemoLev >= 1) {
	    sprintf (GOutBuff, "\n\nCfs Guess:	  '%c'.", GuessLet);
	    WriteStd (GOutBuff);
	    sprintf (GOutBuff, "\nNext letter is: '%c'.", CurLet -> EnvLet);
	    WriteStd (GOutBuff);
	}
#endif

	if (GuessLet == CurLet -> EnvLet) {
	    SysRew = SameLRew;
	    ++TotNmGsL;

#if CTEST
	    if (DemoLev >= 1) {
		sprintf (GOutBuff, "\nGuess is right! Reward is: %f.\n", SysRew);
		WriteStd (GOutBuff);
	    }
#endif
	} else if (SameType (GuessLet, CurLet -> EnvLet)) {
	    MadeMstk = TRUE;
	    SysRew = SameTRew;
	    ++TotNmGsT;

#if CTEST
	    if (DemoLev >= 1) {
		sprintf (GOutBuff, "\nGuess is wrong, but type is right. Reward is: %f.\n", SysRew);
		WriteStd (GOutBuff);
	    }
#endif
	} else {
	    MadeMstk = TRUE;
	    SysRew = WrongRew;
	    ++TotNmGsW;

#if CTEST
	    if (DemoLev >= 1) {
		sprintf (GOutBuff, "\nGuess is wrong. Reward is: %f.\n", SysRew);
		WriteStd (GOutBuff);
	    }
#endif
	}
    }

    return (OK);

}					/* GtSysRew */



#if  CISYSREW				/* This flag is defined in UTILITY.H */
/*********************************

ISysRew 		Subroutine to return reward for individual classifier.

  CfPtr 		Pointer to CfNode for classifier to be rewarded.

  Return		float value that is reward for the specified classifier.

  NOTE:  This just returns the same reward for all classifiers.
		 It is included only as a 'shell' definition of this function.
		 Note that PutCNd() (from CFMSGIO.C) is used to get ascii form of classifier.
		 One could also use BCndtoA() and BActtoA() from CFSUTIL.C file.

******/

float 
ISysRew (CfPtr)
    struct CfNode *CfPtr;
{
    extern VOID PutCNd ();		/* subroutine to get cf in ascii form */

    PutCNd (CfPtr, GCfBuff, 1);		/* get classifier in ascii form, in global buffer from CORE.DEF/EXT */



    return (SysRew);

}					/* ISysRew */

#endif /* CISYSREW */



/*********************************

MakeActS		Make new action string for the LETSEQ domain.

  NewAct		Char buffer (STRNGSZ+1) into which action string is to be placed.

  Currently: Just generate a random string as GENRANDCFS does, and then
  overlay 10 on the leftmost loci to make sure its an action.

******/

VOID 
MakeActS (NewAct)
    char NewAct[];
{
    unsigned int ri;
    extern unsigned int DscDemo;

    GnRndCA (NewAct, &ri, 'c');		/* generate random string */
    NewAct[0] = '1';			/* make it into an action */
    NewAct[1] = '0';

    NewAct[STRNGSZ] = '\0';

#ifdef CTEST
    if (DscDemo > 1) {
	sprintf (GOutBuff, "  |- MakeActS: ri %d, action '%s'\n  ", ri, NewAct);
	WriteStd (GOutBuff);
    }
#endif

}					/* end MakeActS */


/*********************************

SameType	  See if input letters are of the same 'type', return TRUE if so, FALSE elsewise.

  Types, which are defined by array entries 0 to entry before NULL entry, are:
	Vowels	 - vowels
	Consonts - consonants
	Punctua  - puncuation (everything else, currently).
  NOTE: Any other character is type ERROR, so return FALSE.

******/

int 
SameType (Let1, Let2)
    int Let1, Let2;
{
    int type1, type2;

    if ((type1 = GtLetTyp (Let1)) == ERROR || (type2 = GtLetTyp (Let2)) == ERROR)
	return (FALSE);

    if (type1 == type2)
	return (TRUE);
   /* else */
    return (FALSE);

}					/* SameType */



/*********************************

GtLetTyp - return 'type' of letter. If letter no one of the type, return ERROR.
   Types, which are defined by array entries 0 to entry before NULL entry, are:
	  Vowels   - vowels
	  Consonts - consonants
	  Punctua  - puncuation (everything else, currently).

******/

int 
GtLetTyp (Let)
    int Let;
{
    register int i;

    for (i = 0; Punct[i] != '\0'; ++i)
	if (Punct[i] == Let)
	    break;
    if (Punct[i] != '\0')
	return (TYPEPUNC);
    else {
	for (i = 0; Vowels[i] != '\0'; ++i)
	    if (Let == Vowels[i])
		break;
	if (Vowels[i] != '\0')
	    return (TYPEVOW);
	else {
	    for (i = 0; Consonts[i] != '\0'; ++i)
		if (Consonts[i] == Let)
		    break;
	    if (Consonts[i] != '\0')
		return (TYPECON);
	}
    }
    return (ERROR);

}					/* GtLetTyp */



/*********************************

Strg2Let	  Convert message string into a letter.

  String	  STRNGSZ length char array of 1,0's.
			  Only the rightmost STRLETSZ are mapped into a letter.
			  Of those, leftmost 2 specify type, rest specify character of that type.
			  Types:  1 - Vowels
					  2 - Consonants
					  2 - Puntuation

  Return	  int form of a letter (character). Return 0 (NULL char) if string does not specify a valid character.

******/

int 
Strg2Let (String)
    char String[STRNGSZ];
{
    register int i;

    i = Loci2Int (&String[STRNGSZ - STRLETSZ], 2);

    if (i == TYPEVOW) {
	i = Loci2Int (&String[STRNGSZ - STRLETSZ + 2], STRLETSZ - 2);
	if (i < 0 || i > VOWMX)
	    return ('@');
	else
	    return (Vowels[i]);
    } else if (i == TYPECON) {
	i = Loci2Int (&String[STRNGSZ - STRLETSZ + 2], STRLETSZ - 2);
	if (i < 0 || i > CONSMX)
	    return ('@');
	else
	    return (Consonts[i]);
    } else if (i == TYPEPUNC) {
	i = Loci2Int (&String[STRNGSZ - STRLETSZ + 2], STRLETSZ - 2);
	if (i < 0 || i > PUNCMX)
	    return ('@');
	else
	    return (Punct[i]);
    }
   /* else  */
    return ('@');

}					/* Strg2Let */



/*********************************

Let2Strg	  Convert a letter into a message string.

  Letter	  int form of letter to convert.

  String	  STRLETSZ length char array, i.e.,  big enough for encoding all
			  currently alphabet.

  Mapping: Left-most 2 loci specify letter type ( Vowel=01, Consonant=10, punctuation=11, None=00).
		   Rest of loci are ascii form of binary letter-number, as defined by the
		   letter's location in the appropriate character arrays (see ENVIRON.DEF).
  NOTE:  Any invalid letter gets mapped into type=00 (TYPENONE) and undefined other bits.

******/

VOID 
Let2Strg (Letter, String)
    int Letter;
    char String[];
{
    int type, let;

    type = GtLetTyp (Letter);

    if (type == TYPEVOW) {
	for (let = 0; let <= VOWMX; ++let)
	    if (Letter == Vowels[let])
		break;
	if (let > VOWMX)
	    type = TYPENONE;
    } else if (type == TYPECON) {
	for (let = 0; let <= CONSMX; ++let)
	    if (Letter == Consonts[let])
		break;
	if (let > CONSMX)
	    type = TYPENONE;
    } else if (type == TYPEPUNC) {
	for (let = 0; let <= PUNCMX; ++let)
	    if (Letter == Punct[let])
		break;
	if (let > PUNCMX)
	    type = TYPENONE;
    } else {
	type = TYPENONE;
	let = 0;
    }

    Int2Loci (type, String, 2);
    Int2Loci (let, &String[2], STRLETSZ - 2);

}					/* Let2Strg */



/*********************************

DoEnvCmd	  Execute an environment/domain-specific command.

  ParString   The name of the command and any parameters to it (\0 termintated).

*******/

int 
DoEnvCmd (ParStrng)
    char *ParStrng;
{
    int i;
    int retval = OK;
    char parname[8], *par;

    for (; *ParStrng == ' '; ++ParStrng);	/* Strip blanks */

    for (i = 0, par = ParStrng; i <= 7 && *par != ' ' && *par != '\0'; ++i, ++par)	/* get the command */
	parname[i] = *par;
    parname[i] = '\0';

    for (; *par == ' '; ++par);		/* par now at any parameter */

    if (strcmp (parname, "set") == 0)
	retval = SetEnv (par);

    else if (strcmp (parname, "help") == 0)
	HelpEnv ();

    else if (strcmp (parname, "disdetm") == 0)
	DsplDetM ();

    else {
	sprintf (GOutBuff, "\nIllegal EnvCommand  '%s' (%s) Try ECMD HELP.\n", parname, ParStrng);
	WriteStd (GOutBuff);
    }

    return (retval);

}					/* DoEnvCmd */



/*********************************

SetEnv		  Set enviornment variable to some value.

  Par		  string of form
				 parname=value
			  where parname is:
				 lr - set SameLRew value (reward for guessing same letter as comes next).
				 tr - set SameTRew value (reward for missing letter but getting its type).
				 wr  - set WrongRew value (reward for missing letter and type - being wrong).
			  and where value for these is a float value, or where Par is
				 db=[0|1]  to turn execution of default behavior on or off.
				 e = 0	use highest guess from highest bidder
				   = 1	use guess with highest summed support from all bidders
				   = 2	use guess pick with probability proportional to summed support from bidders

******/

int 
SetEnv (Par)
    char *Par;
{
    float val = 0.0;			/* Note this error value */
    char *cval;

    for (cval = Par; *cval != '=' && *cval != '\0'; ++cval);	/* get past parname */
    if (*cval != '\0')
	++cval;				/* get past the = */
    val = atof (cval);			/* should be a float */
    *Par = islower (*Par) ? toupper (*Par) : *Par;

    if (*Par == 'L')
	SameLRew = val;
    else if (*Par == 'T')
	SameTRew = val;
    else if (*Par == 'W')
	WrongRew = val;
    else if (*Par == 'D')
	DftBehFlg = val;
    else if (*Par == 'N')
	NulGsMFlg = val;
    else if (*Par == 'E' &&
	     (0 <= val && val <= 2))
	EfRslMch = val;
    else {
	sprintf (GOutBuff, "\nIllegal ecmd set paramter '%s'.\n", Par);
	WriteStd (GOutBuff);
	return (ERROR);
    }

    return (OK);

}					/* SetEnv */



/*********************************

DisCfIE 	Display classifiers, Interpreted with respect to the Environment.

  Cp		Pointer to CfNode.
  Buff		Char buffer for the output.

  This is called by WrtCfs() in the CFMSGIO.C file when the format
  for displaying classifiers is 21 or 22 or 23. The cf is displayed:

  id: stag: let [w]; stag: let [w] =actt=> stag: let [w] {s} use uic

  where:
	id		cf id number
	stag	source-tag, from leftmost 2 bits:  00->D, 10->G, 01->X, 11->Y
	itag	(fmt 22 or 23) internal tag trits, not in interpreted parts
	let 	letters (symbols) specified by rightmost 5 bits
	w		window (into recent letters) position, from loci 3 and 4 (counting from 1 on left)
	actt	action type, eg, PASS, AND, etc.  (If its PASS, don't write it)
	s		cf strength, bidratio, support
	use 	in last step:  b bid, w won, p posted (after effector confl. resol.)
	uic	 (fmt 23 only) uninterpreted classifier trits, with no punctuation (for use in convergence measures)

******/

VOID 
DisCfIE (Cp, Buff, Format)
    struct CfNode *Cp;
    char Buff[];
    unsigned int Format;
{
    char cond1[STRNGSZ + 1], cond2[STRNGSZ + 1], action[STRNGSZ + 1], tbuff[STRNGSZ + 10];
    int i;

    sprintf (Buff, "\n%4u> ", Cp -> Cf_Id);

    BCndtoA (Cp -> Cnd1Bits, Cp -> Cnd1DCs, cond1);
    cond1[STRNGSZ] = '\0';
    DsCfSTag (cond1, Buff);
    DsCfLet (cond1, Buff);
    DsCfWnd (cond1, Buff);
    if (Format == 22 || Format == 23) {	/* display internal tag region */
	tbuff[0] = ' ';
	for (i = 2; i < STRNGSZ - STRLETSZ; ++i)
	    tbuff[i - 1] = cond1[i];
	tbuff[i - 1] = ',';
	tbuff[i] = ' ';
	tbuff[i + 1] = '\0';
    } else {
	tbuff[0] = ',';
	tbuff[1] = ' ';
	tbuff[2] = '\0';
    }
    strcat (Buff, tbuff);

    BCndtoA (Cp -> Cnd2Bits, Cp -> Cnd2DCs, cond2);
    cond2[STRNGSZ] = '\0';
    DsCfSTag (cond2, Buff);
    DsCfLet (cond2, Buff);
    DsCfWnd (cond2, Buff);
    if (Format == 22 || Format == 23) {
	tbuff[0] = ' ';
	for (i = 2; i < STRNGSZ - STRLETSZ; ++i)
	    tbuff[i - 1] = cond2[i];
	tbuff[i - 1] = '\0';
	strcat (Buff, tbuff);
    }
    for (i = 0; Buff[i] != '\0' && i < 46; ++i)	/* pad out to 45 */
	;
    if (i < 46) {
	for (; i < 46; ++i)
	    Buff[i] = ' ';
	Buff[i] = '\0';
    }
    DsCfAct (Cp, Buff);

    BActtoA (Cp -> ActBits, Cp -> ActDCs, action);
    action[STRNGSZ] = '\0';
    for (i = 0; i <= STRNGMX; ++i)
	if (action[i] == '#')		/* use condition1 to fill #'s in action string */
	    action[i] = cond1[i];

    DsCfSTag (action, Buff);
    DsCfLet (action, Buff);
    DsCfWnd (action, Buff);

    if (Format == 22 || Format == 23) {
	tbuff[0] = ' ';
	for (i = 2; i < STRNGSZ - STRLETSZ; ++i)
	    tbuff[i - 1] = action[i];
	tbuff[i - 1] = ' ';
	tbuff[i] = '\0';
    } else {
	tbuff[0] = ' ';
	tbuff[1] = '\0';
    }
    strcat (Buff, tbuff);

    sprintf (tbuff, "{%.0f,%.2f,%.1f}", Cp -> Strength, Cp -> BidRatio, Cp -> Support);
    strcat (Buff, tbuff);

    if (Cp -> NmPost > 0)
	strcat (Buff, " p ");
    else if (Cp -> NmProd > 0)
	strcat (Buff, " w ");
    else if (Cp -> NmMtch > 0)
	strcat (Buff, " b ");

    if (Format == 23) {
	strcat (Buff, cond1);
	strcat (Buff, cond2);
	strcat (Buff, action);
    }
}					/*  DisCfIE  */



VOID 
DsCfSTag (CfBuff, OutBuff)		/* Display Source Tag (left 2) loci */
    char CfBuff[], OutBuff[];
{

    if (CfBuff[0] == '0' && CfBuff[1] == '0')
	strcat (OutBuff, " D: ");
    else if (CfBuff[0] == '0' && CfBuff[1] == '1')
	strcat (OutBuff, " X: ");
    else if (CfBuff[0] == '0' && CfBuff[1] == '#')
	strcat (OutBuff, "DX: ");
    else if (CfBuff[0] == '1' && CfBuff[1] == '0')
	strcat (OutBuff, " G: ");
    else if (CfBuff[0] == '1' && CfBuff[1] == '1')
	strcat (OutBuff, " Y: ");
    else if (CfBuff[0] == '1' && CfBuff[1] == '#')
	strcat (OutBuff, "GY: ");
    else if (CfBuff[0] == '#' && CfBuff[1] == '0')
	strcat (OutBuff, "DG: ");
    else if (CfBuff[0] == '#' && CfBuff[1] == '1')
	strcat (OutBuff, "XY: ");
    else if (CfBuff[0] == '#' && CfBuff[1] == '#')
	strcat (OutBuff, "DGXY: ");

}					/*  DsCfSTag  */



VOID 
DsCfLet (CfBuff, OutBuff)		/* Display Letter or letters */
    char CfBuff[], OutBuff[];
{
    int i, w, let;
    char tbuf[4], lbuff[STRNGSZ + 1];

    w = 0;

    if (CfBuff[STRNGSZ - STRLETSZ] != '1' && CfBuff[STRNGSZ - STRLETSZ + 1] != '0') {
	lbuff[STRNGSZ - STRLETSZ] = '0';
	lbuff[STRNGSZ - STRLETSZ + 1] = '1';
	for (i = 1; i < 31; ++i) {
	    Int2Loci (i, &lbuff[STRNGSZ - STRLETSZ + 2], STRLETSZ - 2);
	    if ((let = Strg2Let (lbuff)) != '@') {
		if (IsMemSym (i, &CfBuff[STRNGSZ - STRLETSZ + 2])) {
		    if (w != 0)
			strcat (OutBuff, ",");
		    sprintf (tbuf, "%c", let);
		    strcat (OutBuff, tbuf);
		    ++w;
		}
	    }
	}
    }
    if (CfBuff[STRNGSZ - STRLETSZ] != '0' && CfBuff[STRNGSZ - STRLETSZ + 1] != '1') {
	lbuff[STRNGSZ - STRLETSZ] = '1';
	lbuff[STRNGSZ - STRLETSZ + 1] = '0';
	for (i = 1; i < 31; ++i) {
	    Int2Loci (i, &lbuff[STRNGSZ - STRLETSZ + 2], STRLETSZ - 2);
	    if ((let = Strg2Let (lbuff)) != '@') {
		if (IsMemSym (i, &CfBuff[STRNGSZ - STRLETSZ + 2])) {
		    if (w != 0)
			strcat (OutBuff, ",");
		    sprintf (tbuf, "%c", let);
		    strcat (OutBuff, tbuf);
		    ++w;
		}
	    }
	}
    }
    strcat (OutBuff, " ");

}					/* DsCfLet */


VOID 
DsCfWnd (CfBuff, OutBuff)		/* Display ``window'' counter (current 0, one-back 1, etc.) */
    char CfBuff[], OutBuff[];
{

    if (CfBuff[3] == '0' && CfBuff[4] == '0')
	strcat (OutBuff, " [0]");
    else if (CfBuff[3] == '0' && CfBuff[4] == '1')
	strcat (OutBuff, " [1]");
    else if (CfBuff[3] == '0' && CfBuff[4] == '#')
	strcat (OutBuff, "[01]");
    else if (CfBuff[3] == '1' && CfBuff[4] == '0')
	strcat (OutBuff, " [2]");
    else if (CfBuff[3] == '1' && CfBuff[4] == '1')
	strcat (OutBuff, " [3]");
    else if (CfBuff[3] == '1' && CfBuff[4] == '#')
	strcat (OutBuff, "[23]");
    else if (CfBuff[3] == '#' && CfBuff[4] == '0')
	strcat (OutBuff, "[02]");
    else if (CfBuff[3] == '#' && CfBuff[4] == '1')
	strcat (OutBuff, "[13]");
    else if (CfBuff[3] == '#' && CfBuff[4] == '#')
	strcat (OutBuff, "[0123]");

}					/* DsCfWnd */


VOID 
DsCfAct (CPtr, Buff)			/* Display action type */
    struct CfNode *CPtr;
    char Buff[];
{
    struct CfOpNode *coptr, *GetCOpCd ();	/* pointer to classifier operator (ActType) node. */
    int retval;

    strcat (Buff, " /");
    coptr = GetCOpCd (CPtr -> ActType, &retval);
    if (strcmp (coptr -> CfOpName, "PASS") != 0)
	strcat (Buff, coptr -> CfOpName);
    strcat (Buff, "/ ");

}					/* DsCfAct */



/********************************

IsMemSym		See if symbol is member of set of symbols consistent with message.

  Symbol		Binary form of symbol to check.
  Set			NMLETATT long ascii string in {0,1,#} that specifies set of symbols.

******/

int 
IsMemSym (Symbol, Set)
    unsigned int Symbol;
    char Set[];
{
    unsigned int aloci, SetBits, SetDCs;

    SetBits = SetDCs = 0;

    for (aloci = 0; aloci < NMLETATT - 2; ++aloci) {
	SetBits <<= 1;			/* zero into lsb */
	SetDCs <<= 1;
	switch (Set[aloci]) {
	 case '0':
	     SetDCs |= LSB;
	     break;
	 case '1':
	     SetBits |= LSB;
	     SetDCs |= LSB;
	     break;
	 case '#':
	     break;
	 default:
	     sprintf (GOutBuff, "\nERR (IsMemSym): illegal char. in condition: '%c'", Set[aloci - 1]);
	     WriteStd (GOutBuff);
	     return (ERROR);
	};
    }

    if (!((Symbol & SetDCs) ^ SetBits))
	return (TRUE);

    return (FALSE);

}					/* end IsMemSym */



/*********************************

HelpEnv 	  Help command for ECMDs.

******/

VOID 
HelpEnv ()
{

    WriteStd ("\nEnvironment commands:\n");
    WriteStd ("\ndisdetm	 display next detector messages.");
    WriteStd ("\nset		 set one of these LETSEQ parameters:");
    WriteStd ("\n  lr=d.d	  reward for correct letter.");
    WriteStd ("\n  tr=d.d	  reward for correct type (vowel/cons) only.");
    WriteStd ("\n  wr=d.d	  reward for wrong guess.");
    WriteStd ("\n  e=0		 use guess from highest bidder.");
    WriteStd ("\n   =1		 use guess with highest summed support from all bidders");
    WriteStd ("\n   =2		 use guess with probability proportional to summed support");
    WriteStd ("\n  db=[0|1]	do-default-guess at each step on (1) or off(0).");
    WriteStd ("\n  ng=[0|1]	null-guess message on (1) or off (0).");
    WriteStd ("\n\nTo see 'interpreted' classifiers, DISPLAY CL,21 or CL,22\n\n");

}					/* HelpEnv */



/*********************************

DsplDetM	  Display next detector messages.

  This implements the  'disdetm' ECMD.

******/

VOID 
DsplDetM ()
{
    struct EnvNode *let;
    int i, letnum;
    char mess[STRNGSZ + 1];

    WriteStd ("\n\nNext messages from detectors (oldest seen to most recently seen):");

    for (let = OldLet, letnum = STMemMx; letnum >= 0; --letnum, let = let -> NxtEnvNd) {
	for (i = 0; i <= STRNGMX; ++i)
	    mess[i] = '0';
	Let2Strg (let -> EnvLet, &mess[STRNGSZ - STRLETSZ]);
	mess[STRNGSZ] = '\0';
	mess[0] = '0';
	mess[1] = '0';			/* Mark as message from detector */
	Int2Loci (letnum, &mess[2], 3);	/* Mark 'recency' of this letter, 0 is most recent. */
	sprintf (GOutBuff, "\n%d. %c:   %s", letnum, let -> EnvLet, mess);
	WriteStd (GOutBuff);
    }

    WriteStd ("\n\n");

}					/*  DsplDetM  */


#ifdef	THISCODE			/* Not usually compiled */
/*********************************

GtUsrMsg	  Get messages from user to be treated as detector messages.
			  This is just a utility for DEBUGGING.

  Load each message into char buffer MStrings at position NxtMsg (and increment that pointer).
  Store associated intensity in MIntense array at position NumMsgs, and then increment NumMsgs.

******/

GtUsrMsg (NxtMsg, MStrings, MIntense, NumMsgs)
    char *NxtMsg, MStrings[];
    float MIntense[];
    int *NumMsgs;
{
    int done, RetVal;
    char mess[STRNGSZ + 1], line[65];
    float intense;

    for (done = FALSE; !done && *NumMsgs < DMSGSMX; *NumMsgs++) {
	WriteStd ("\nDetector message? ");
	mess[0] = '\0';
	if ((RetVal = ReadS (line, 64, stdin, sizeof (line))) == ERROR) {
	    WriteStd ("\nERROR returned from ReadS.");
	    break;
	}
	RetVal = sscanf (line, "%16s,%f", mess, &intense);
	if (RetVal == EOF || RetVal == 0)
	    done = TRUE;
	else {
	    MIntense[*NumMsgs] = intense;
	    strcpy (NxtMsg, mess);
	    NxtMsg = &MStrings[(STRNGSZ * (*NumMsgs + 1)) + (*NumMsgs + 1)];
	}
    }

}					/* GtUsrMsg */

#endif /* THISCODE */
