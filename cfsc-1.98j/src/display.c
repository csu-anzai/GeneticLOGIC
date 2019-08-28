
/*			DISPLAY for the CFS-C Classifier System

This file, DISPLAY.C, is part of the CFS-C classifier system (Copyright 1986,1988 Rick L. Riolo).

The subroutines in this file are called to DISPLAY variables, help, etc.
The subroutines:

	Display 	main entry point for DISPLAY command.
	DsplVar 	display a variable.
	AuDisplay	main entry for auto-display mechanism.
	DsplVars	| display selected variables.
	PrnVars 	|
	WrtVars 	|
	DsplSets	Display special settings, constants, etc.
	Help		The HELP command text.
**/

#include	"config.h"

#include	"utility.h"
#include	"core.ext"
#include	"cfsio.ext"
#include	"usercmd.ext"

#include	"display.h"

extern unsigned int ADCycInt;

#if MPWC
#define __SEG__ CFSCWRITE
#endif


/*******************************

Display 	Called when user enters a DISPLAY command.
	
CmdPars 	A character array buffer that contains any parameters specified on the DISPLAY command.

The parameters have been converted to lowercase, and the string is
terminated by a '\0'. Each parameter in CmdPars is of the form:
	what[,format[,where]]
Each parameter is separated from the next by one or more blanks.
Display() picks off each parameter until none are left. For each one,
it calls approriate function to display it (or prints error message if unknown 'what').

******/

VOID 
Display (CmdPars)
    char CmdPars[];
{
    int formati = DFLTFMT;		/* int form of display forrmat */
    char par[PARNAMSZ + PARVALSZ + 1];	/* room for parameter and \0 */
    char name[PARNAMSZ + 1];		/* room for name part and \0 */
    char format[PARNAMSZ + 1];		/* room for the format part */
    char where[PARNAMSZ + 1];		/* room for where part */
    char *parptr, *lineptr;		/* pointer to next part of Par; Pointer to next par to fetch from CmdPars */
    extern FILE *DisOFILE;		/* display output stream */
    extern char Version[], RunDate[], RunTime[];

    for (lineptr = CmdPars; TRUE;) {
	lineptr = StpTok2 (lineptr, par, sizeof (par), " ");	/* Pick off next par, if any */
	if (par[0] == '\0')
	    break;			/* None found -- all done */
	else {				/* Get the various parts of this parameter from par variable. */
	    parptr = StpTok2 (par, name, sizeof (name), ",");
	    parptr = StpTok2 (parptr, format, sizeof (format), ",");
	    parptr = StpTok2 (parptr, where, sizeof (where), " ");
	    if (format[0] != '\0') {
		if (!IsInt (format))
		    WriteStd ("\nDISPLAY 'format' value should be an integer.\n");
		else
		    formati = atoi (format);
	    }
	    if (strcmp (name, "cl") == 0)
		DsplCfs (where, formati);

	    else if (strcmp (name, "ml") == 0)
		DsplMsgs (where, formati, 'c');

	    else if (strcmp (name, "oml") == 0)
		DsplMsgs (where, formati, 'o');

	    else if (strcmp (name, "env") == 0)
		DsplEnv (where, formati);

	    else if (strcmp (name, "vars") == 0)
		DsplVars (where, formati);

	    else if (strcmp (name, "settings") == 0)
		DsplSets (where, formati);

	    else if (strcmp (name, "cyclestp") == 0 || strcmp (name, "step") == 0) {
		sprintf (GOutBuff, "\nNext Major-Cycle Step:   %u.\n", CycleStp);
		WriteS (GOutBuff, DisOFILE);
	    } else if (strcmp (name, "banner") == 0) {
		sprintf (GOutBuff, "\nCFC-C Version %s.	 Run-Date  %s  (%s).\n", Version, RunDate, RunTime);
		WriteS (GOutBuff, DisOFILE);
	    } else if (strcmp (name, "time") == 0) {
		WriteS ("\nTime:  ", DisOFILE);
		GetCFSCTime (GOutBuff, 1);	/* hh:mm:ss form */
		*(GOutBuff + 8) = '\n';
		*(GOutBuff + 9) = '\0';
		WriteS (GOutBuff, DisOFILE);
	    } else
		DsplVar (name, where);	/* format ignored */
	}
    }

}					/* Display */


/*******************************

DsplVar 	Display value of specific variable in the RTVars Table (see USERCMD.DEF).

	VarName	Name of variable to display.

	FDName	String name of File/device.
			(Default for "" is DisOutFN--the display output stream.)

******/

int 
DsplVar (VarName, FDName)
    char *VarName, *FDName;
{
    struct RTVarNd *rtv;
    char *cp;
    FILE *fileptr;
    extern char *DisOutFN, *StdOutFN;
    extern FILE *DisOFILE, *StdOFILE;

    while (*VarName == ' ')
	++VarName;			/* skip leading blanks, and cut off trailors. */
    for (cp = VarName; *cp != ' ' && *cp != '\0'; ++cp);
    if (*cp == ' ')
	*cp = '\0';

    while (*FDName == ' ')
	++FDName;

    if (strcmp (FDName, DisOutFN) == 0 || *FDName == '\0')
	fileptr = DisOFILE;		/* DisOutFN is already open */
    else if (strcmp (FDName, StdOutFN) == 0)
	fileptr = StdOFILE;		/* StdOutFN is already open */
    else if ((fileptr = GetFILE (FDName, "", WRITEMODE)) == NULL)
	return (ERROR);

    if ((rtv = FindRTVN (VarName)) == NULL) {
	sprintf (GOutBuff, "\nVariable '%s' not found.", VarName);
	WriteStd (GOutBuff);
	WriteS (GOutBuff, fileptr);
    } else {
	if (strcmp (VarName, "urndsd") == 0) {

#if (INTSZ == 16)			/* then URndSd is declared as long int in UTILITY.C */
	    sprintf (GOutBuff, "\n%s=%ld\n", VarName, *((long int *) rtv -> Addr));
#else

#if (INTSZ == 32)			/* else if its declared as int */
	    sprintf (GOutBuff, "\n%s=%u\n", VarName, *((int *) rtv -> Addr));
#else /* else its a mistake--should be INTSZ 16 or 32 */
	    sprintf (GOutBuff, "\n\n***DsplVar (DISPLAY.C): URndSd not displayable!!!\n\n");
	    WriteS (GOutBuff, fileptr);	/* to the log or save file, if on */
	    printf (GOutBuff);		/* to the screen, so user sees it */
#endif

#endif
	} else if (rtv -> Type == 's')
	    sprintf (GOutBuff, "\n%s=%d\n", VarName, *((short *) rtv -> Addr));
	else if (rtv -> Type == 'i')
	    sprintf (GOutBuff, "\n%s=%u\n", VarName, *((int *) rtv -> Addr));
	else if (rtv -> Type == 'u')
	    sprintf (GOutBuff, "\n%s=%u\n", VarName, *((unsigned int *) rtv -> Addr));
	else if (rtv -> Type == 'f')
	    sprintf (GOutBuff, "\n%s=%f\n", VarName, *((float *) rtv -> Addr));
	else if (rtv -> Type == 'c')
	    sprintf (GOutBuff, "\n%s=%s\n", VarName, rtv -> Addr);
	else
	    sprintf (GOutBuff, "\n**ERR DsplVar: illegal Type (%c) in RTVarNd for '%s'!\n",
		     rtv -> Type, VarName);
	WriteS (GOutBuff, fileptr);
    }

    if (fileptr != DisOFILE && fileptr != StdOFILE)	/* Dont close StdOutFN stream! */
	CloseFILE (FDName, "", fileptr);

    return (OK);

}					/* DsplVar */


/*******************************

AuDisplay	Auto-display various items user wants to see.

This is called from Classify() when AutoDspl > 0, i.e., when the user
wants one or items automatically displayed every specified number of steps.

Check each ADxxxInt variable: if CycleStp mod ADxxxInt = 0 then display the item 'xxx'.
Display item xxx in format specified by ADxxxFmt variable

NOTE: Detector messages are auto-displayed in RdDetect (since this function, AuDisplay,
is called at the bottom of the Major cycle before there are any detector messages.

******/

VOID 
AuDisplay ()
{
    extern unsigned int ADCMInt, ADCMFmt;	/* Current messages */
    extern unsigned int ADCfInt, ADCfFmt, ADVarInt, ADVarFmt;	/* Classifiers and system variables */
    extern unsigned int ADEnvInt, ADEnvFmt;	/* The Environment */
    extern FILE *DisOFILE;		/* display output stream */

    if (CycleStp % ADCycInt == 0) {
	sprintf (GOutBuff, "%\n**** At end of Cycle Step %u ****\n", CycleStp);
	WriteS (GOutBuff, DisOFILE);
    }
    if (CycleStp % ADCMInt == 0)
	DsplMsgs ("", ADCMFmt, 'c');

    if (CycleStp % ADCfInt == 0)
	DsplCfs ("", ADCfFmt);

    if (CycleStp % ADVarInt == 0)
	DsplVars ("", ADVarFmt);

    if (CycleStp % ADEnvInt == 0)
	DsplEnv ("", ADEnvFmt);

}					/* AuDisplay */


/*******************************

DsplVars	Display system variables.

	FDName	string name of output file/device.
			(Default for "" is to write to DisOutFN--the display output stream.)

	Format	parameter determines the format in which variables are to be displayed.
			See WrtMsgs (below) for Format values and what they do.

DsplVars just opens the file, calls WrtVars or PrnVars to do the work, and closes the file.
**/

int 
DsplVars (FDName, Format)
    char *FDName;
    int Format;
{
    FILE *destin;
    extern char *DisOutFN, *StdOutFN;
    extern FILE *DisOFILE, *StdOFILE;

    if (Format == DSPLOFF)
	return (OK);

    if (strcmp (FDName, DisOutFN) == 0 || *FDName == '\0')
	destin = DisOFILE;		/* DisOutFN is already open */
    else if (strcmp (FDName, StdOutFN) == 0)
	destin = StdOFILE;		/* StdOutFN is already open */
    else if ((destin = GetFILE (FDName, "", WRITEMODE)) == NULL)
	return (ERROR);

    if (Format == SAVEFMT)
	WrtVars (destin, Format);
    else
	PrnVars (destin, Format);

    if (destin != DisOFILE && destin != StdOFILE)	/* Don't close these streams! */
	CloseFILE (FDName, "", destin);

    return (OK);

}					/* DsplVars */


/*******************************

PrnVars 	Write variables to open file/device.

	FilePtr	Opened file/device.

	Format	Select which variables to print.

******/

VOID 
PrnVars (FilePtr, Format)
    FILE *FilePtr;
    unsigned int Format;
{
    struct RTVarNd *rtv;
    extern struct RTVarNd *RTVarNxt;
    int nmvars;

    extern unsigned int DetectRt, EffectRt, NmIMsgMx;
    extern float SysTreas, Bid_k, BRPow, HeadTax, BidTax, PrdTaxMx, FrPayDet, CfStrDf, CfSuppDf, MsgIntDf, CfStrMin, CfStrMax,
        CfBidMin;
    extern float TotCCSup, TotCCBid, HiCCBid, LowCCBid, AveCfBR;
    extern short ShareRew, AllPayBd;
    extern unsigned int TOTNmBid, TOTMtch, TOTMsPrd, TOTMsPst, TOTNmWin, TOTCfPst, TOTBGStr;
    extern unsigned int TOTNmPRw, TOTNmNRw, TOTSyRwP, TOTSyRwN, TOTEMtch, TOTEAct, TOTEAMsg;
    extern unsigned int TOTNmOfs, TotBkgGA, TotBGASC, TotBGANC, TotBGAFC, TotMu, TotBGABP;
    extern unsigned int TotCDM, TotCDMC, TotCDML, TotCEf, TotCEfW, TotCEfB, TotACPC, TotCSS, TotTLB;

    FindHighLowStrCf ();		/* Set HighestStrCf and LowestStrCf to proper values. */

    sprintf (GOutBuff, "\nSystem Variables at end of Major-Cycle step #%u : ", CycleStp);
    WriteS (GOutBuff, FilePtr);

    if (Format == 1 || Format == 2) {
	sprintf (GOutBuff, "\n\nNumber of classifiers:		  %3u (Max %u)", NmCfs, NmCfsMx);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nNum. Candidate-classifiers:	 %3u;   %3u Cfs. won (%3u posted msgs).",
		 NmCandCf, NmCfWon, NmCfPost);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nSupport for bidders:	   %8.1f", TotCCSup);
	WriteS (GOutBuff, FilePtr);
	if (NmCandCf > 0) {

#if SUN3				/* kludge for SUN compiler bug */
	    sprintf (GOutBuff, "\nAverage (Hi,Low) bids:	 %8.2f (%.2f,%.2f)",
		     (TotCCBid / (int) NmCandCf), HiCCBid, LowCCBid);
#else
	    sprintf (GOutBuff, "\nAverage (Hi,Low) bids:	 %8.2f (%.2f,%.2f)", (TotCCBid / NmCandCf), HiCCBid, LowCCBid);
#endif

	    WriteS (GOutBuff, FilePtr);
	} else
	    WriteS ("\nAverage (Hi,Low) bids:		  ---", FilePtr);

	sprintf (GOutBuff, "\nNum. candidate matches:	%8u", NmMtchs);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nNumber of messages:			 %3u   (Max-Int %u, Max %u)",
		 NmCMsgs, NmIMsgMx, MSGLSTSZ);
	WriteS (GOutBuff, FilePtr);

	if (NmCfs > 0) {
	    sprintf (GOutBuff, "\n\nHigh/Low strength Cfs:   %8.1f (%u) / %8.1f (%u)",
		     HighestStrCf -> Strength, HighestStrCf -> Cf_Id, LowestStrCf -> Strength, LowestStrCf -> Cf_Id);
	    WriteS (GOutBuff, FilePtr);
	}
	sprintf (GOutBuff, "\nTotal strength (ave):	%8.1f  (%.1f)", TotCfStr, AveCfStr);
	WriteS (GOutBuff, FilePtr);
	if (NmCfs > 0) {

#if SUN3				/* kludge for SUN compiler bug */
	    sprintf (GOutBuff, "\nAverage BidRatio:			%4.2f", TotCfBR / (int) NmCfs);
#else
	    sprintf (GOutBuff, "\nAverage BidRatio:			%4.2f", TotCfBR / NmCfs);
#endif

	    WriteS (GOutBuff, FilePtr);
	} else
	    WriteS ("\nAverage BidRatio:			----", FilePtr);
	sprintf (GOutBuff, "\nSystem treasury:		%9.1f", SysTreas);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nNum of bids > strength:   %3u", TOTBGStr);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\n\nTOTNmBid %6u  TOTNmWin %6u  TOTCfPst %6u", TOTNmBid, TOTNmWin, TOTCfPst);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nTOTMtch  %6u  TOTMsPrd %6u  TOTMsPst %6u", TOTMtch, TOTMsPrd, TOTMsPst);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\nTOTEMtch %6u  TOTEMsg  %6u  TOTEAct  %6u", TOTEMtch, TOTEAMsg, TOTEAct);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nTOTNmPRw %6u  TOTNmNRw %6u", TOTNmPRw, TOTNmNRw);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nTOTSyRwP %6u  TOTSyRwN %6u", TOTSyRwP, TOTSyRwN);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\nTOTNmOfs %6u  TotMu	%6u  TotBkgGA (BP %u) %6u", TOTNmOfs, TotMu, TotBGABP, TotBkgGA);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nTotBGASC %6u  TotBGAFC %6u  TotBGANC %6u", TotBGASC, TotBGAFC, TotBGANC);
	WriteS (GOutBuff, FilePtr);
	if (TotCDMC != 0)
	    sprintf (GOutBuff, "\nTotCDM   %6u  TotCDML  %6u  TotCDMC  %6u (Loci/Cond %4.1f)",

#if SUN3				/* kludge for SUN compiler bug */
		     TotCDM, TotCDML, TotCDMC, (1.0 * TotCDML / (int) TotCDMC));
#else
		     TotCDM, TotCDML, TotCDMC, (1.0 * TotCDML / TotCDMC));
#endif

	else
	    sprintf (GOutBuff, "\nTotCDM   %6u  TotCDML  %6u  TotCDMC  %6u (Loci/Cond ----)",
		     TotCDM, TotCDML, TotCDMC);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nTotCEf   %6u (Wrg %d, Bd %d)   TotACPC  %u   TotCSS %u   TotTLB %u",
		 TotCEf, TotCEfW, TotCEfB, TotACPC, TotCSS, TotTLB);
	WriteS (GOutBuff, FilePtr);
    }
    if (Format == 2 || Format == 3) {
	sprintf (GOutBuff, "\n\nBid_k:							   %6.3f", Bid_k);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\n\nBRPow							  %6.3f", BRPow);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nMinimum bid allowed:				 %6.2f", CfBidMin);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nHeadTax (per step):				  %6.3f", HeadTax);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nBidTax  (per bid):				   %6.3f", BidTax);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nPrdTaxMx:							%6.3f", PrdTaxMx);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nFraction of bid-share to Detectors:  %6.3f", FrPayDet);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\nDefault classifier strength:		 %6.1f", CfStrDf);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nMinimum/Maximum cf strength allowed: %6.2f / %8.1f", CfStrMin, CfStrMax);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nDefault message intensity:		   %6.1f", MsgIntDf);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nDefault classifier support (NOTs):   %6.1f", CfSuppDf);
	WriteS (GOutBuff, FilePtr);

	sprintf (GOutBuff, "\n\nDetector-Sampling Rate:		  %3u", DetectRt);
	WriteS (GOutBuff, FilePtr);
	sprintf (GOutBuff, "\nEffector-activation Rate:		%3u", EffectRt);
	WriteS (GOutBuff, FilePtr);
    }
    if (Format == 9) {
	for (rtv = RTVars, nmvars = 0; rtv != RTVarNxt; ++rtv, ++nmvars) {
	    if (rtv -> Type == 's')
		sprintf (GOutBuff, "\n%s=%d", rtv -> Name, *((short *) rtv -> Addr));
	    else if (rtv -> Type == 'i')
		sprintf (GOutBuff, "\n%s=%u", rtv -> Name, *((int *) rtv -> Addr));
	    else if (rtv -> Type == 'u')
		sprintf (GOutBuff, "\n%s=%u", rtv -> Name, *((unsigned int *) rtv -> Addr));
	    else if (rtv -> Type == 'f')
		sprintf (GOutBuff, "\n%s=%f", rtv -> Name, *((float *) rtv -> Addr));
	    else if (rtv -> Type == 'c')
		sprintf (GOutBuff, "\n%s=%s", rtv -> Name, rtv -> Addr);
	    else {
		sprintf (GOutBuff, "\n**ERR PrnVars: illegal Type (%c) in RTVarNd for '%s'!\n",
			 rtv -> Type, rtv -> Name);
		--nmvars;
	    }
	    WriteS (GOutBuff, FilePtr);
	    if (rtv -> Type != 'c')
		sprintf (GOutBuff, "  (%.2g, %.2g) stored at %u.", rtv -> Low, rtv -> High, rtv -> Addr);
	    else
		sprintf (GOutBuff, "; Var '%s' stored at %u.", rtv -> Name, rtv -> Addr);
	    WriteS (GOutBuff, FilePtr);
	}
	sprintf (GOutBuff, "\nThere are %d vars.", nmvars);
	WriteS (GOutBuff, FilePtr);
    }
    WriteS ("\n", FilePtr);

}					/* PrnVars */


/*******************************

WrtVars 	Write variables to already opened file/device.

	FilePtr	Pointer to open FILE.

	Format	Format to use for display:
				 SAVEFMT Just write variables in the CVTable (see USERCMD.DEF).

NOTE: SAVEFMT is the only format implemented.

******/

int 
WrtVars (FilePtr, Format)
    FILE *FilePtr;
    unsigned int Format;
{
    struct RTVarNd *rtv;

    for (rtv = RTVars; rtv != RTVarNxt; ++rtv) {
	if (rtv -> Type == 's')
	    sprintf (GOutBuff, "%s=%d\n", rtv -> Name, *((short *) rtv -> Addr));
	else if (rtv -> Type == 'i')
	    sprintf (GOutBuff, "%s=%u\n", rtv -> Name, *((int *) rtv -> Addr));
	else if (rtv -> Type == 'u')
	    sprintf (GOutBuff, "%s=%u\n", rtv -> Name, *((unsigned int *) rtv -> Addr));
	else if (rtv -> Type == 'f')
	    sprintf (GOutBuff, "%s=%f\n", rtv -> Name, *((float *) rtv -> Addr));
	else if (rtv -> Type == 'c')
	    sprintf (GOutBuff, "; %s=%s\n", rtv -> Name, rtv -> Addr);
	else {
	    sprintf (GOutBuff, "\n; **ERR PrnVars: illegal Type (%c) in RTVarNd for '%s'!\n",
		     rtv -> Type, rtv -> Name);
	    WriteStd (GOutBuff);
	}
	fprintf (FilePtr, GOutBuff);
    }

    fprintf (FilePtr, "END-VARIABLES\n");

    return (OK);

}					/* WrtVars */


/*******************************

DsplSets	 Display system control settings, constants, etc.

	The parameter specifies a format.

******/

int 
DsplSets (FDName, Format)
    char *FDName;
    int Format;
{
    FILE *destin;
    extern char *MsgInFN, *CfInFN, *SaveSyPa, *SaveSyFN, *LogFName, *DisOutFN, *StdOutFN;
    extern unsigned int SaveFNum;
    extern FILE *DisOFILE, *StdOFILE;

    if (Format == DSPLOFF)
	return (OK);

    if (strcmp (FDName, DisOutFN) == 0 || *FDName == '\0')
	destin = DisOFILE;		/* DisOutFN is already open */
    else if (strcmp (FDName, StdOutFN) == 0)
	destin = StdOFILE;		/* StdOutFN is already open */
    else if ((destin = GetFILE (FDName, "", WRITEMODE)) == NULL)
	return (ERROR);

    WriteS ("\n\nSystem Settings:\n", destin);

    WriteS ("\nDefault I/O files/devices:", destin);
    sprintf (GOutBuff, "\n  Load messages from:	 %s", MsgInFN);
    WriteS (GOutBuff, destin);
    sprintf (GOutBuff, "\n  Load classifiers from:  %s", CfInFN);
    WriteS (GOutBuff, destin);
    sprintf (GOutBuff, "\n  Load environment from:  %s", EnvInFN);
    WriteS (GOutBuff, destin);

    sprintf (GOutBuff, "\n  Standard output on:	 %s", StdOutFN);
    WriteS (GOutBuff, destin);
    sprintf (GOutBuff, "\n  Display output on:	  %s", DisOutFN);
    WriteS (GOutBuff, destin);
    WriteS ("\n  Log file:			", destin);
    if (LogFName[0] == '\0')
	WriteS ("No log file.", destin);
    else {
	sprintf (GOutBuff, "%s", LogFName);
	WriteS (GOutBuff, destin);
    }
    sprintf (GOutBuff, "\n  Save-system on:		%s%s.%03d\n", SaveSyPa, SaveSyFN, SaveFNum);
    WriteS (GOutBuff, destin);

    if (destin != DisOFILE && destin != StdOFILE)	/* Don't close these streams! */
	CloseFILE (FDName, "", destin);

    return (OK);

}					/* DsplSets */


/*******************************

Help -- the HELP command.
	ParStr - indicate what display:
	 \0 	 - display command list.
	 set	 - SET command parameters.
	 display - DISPLAY command parameters.

******/

VOID 
Help (ParStr)
    char ParStr[];
{

    WriteStd ("\nSorry, no help.\n");

#if ( MPWC || SUN3 || MACLSC || APOLLOC || VAX )
    if (ParStr[0] == '\0') {
	WriteStd ("\nCommands and parameters");
	WriteStd ("\nTo stop:						  STOP");
	WriteStd ("\nTo load messages:				 { LOADC | LC } [ file-name ]");
	WriteStd ("\nTo load classifiers			   { LOADM | LM } [ file-name ]");
	WriteStd ("\nTo load the 'environment':		{ LOADE | LE } [ file-name ]");
	WriteStd ("\nTo display things:				{ DISPLAY | DI } item [ ,item ]*");
	WriteStd ("\nTo set parameters				 SET item=value [ ,item=value ]*");
	WriteStd ("\nTo run the system n steps:		{ CLASSIFY | C }   n");
	WriteStd ("\nTo run the system 1 step:		 { STEP | ST }");
	WriteStd ("\nTo execute an enviroment command: ECMD  command");
	WriteStd ("\nTo read commands from a file:	 { READCMDS | RCS } file");
	WriteStd ("\nTo save classifiers for reload:   DISPLAY cl,filename,99");
	WriteStd ("\nTo save messages for reload:	  DISPLAY ml,filename,99");
	WriteStd ("\nTo save the entire system state:  SAVESYS  [filename]");
	WriteStd ("\nTo append classifiers to list:	APPC  [ file-name ]");
	WriteStd ("\nTo append messages to list:	   APPM  [ file-name ]");
	WriteStd ("\nTo modify a classifier:		   MC	id  (not implemented)");
	WriteStd ("\nTo modify a message:			  MM	id  (not implemented)");
	WriteStd ("\nHELP							  HELP [ { SET | DISPLAY } ]");
	WriteStd ("\nUppercase means enter as shown. 'file-name' can be a path (if supported).");
	WriteStd ("\n[ ] indicates optional; { X | Y } indicate alternative forms.");
    } else if (strcmp (ParStr, "set") == 0) {
	WriteStd ("\nCommon SET-able items:");

	WriteStd ("\nTo control list sizes, taxes and other accounting:");
	WriteStd ("\n  nms=n	  -- maximum number of new (internally generated) messages.");
	WriteStd ("\n  ncfs=n	 -- maximum number of classifiers");

	WriteStd ("\n  strdf=d.d  -- default classifier strength.");
	WriteStd ("\n  strmax=d.d -- maximum classifier strength.");
	WriteStd ("\n  intdf=d.d  -- default intensity for messages (from detectors).");
	WriteStd ("\n  supdf=d.d  -- default support for classifiers (for NOT-conditions).");

	WriteStd ("\n  bidk=d.d   -- bid 'risk' factor.");
	WriteStd ("\n  bidmin=d.d -- minimum bid possible.");
	WriteStd ("\n  htax=d.d   -- head tax.");
	WriteStd ("\n  btax=d.d   -- bid  tax.");
	WriteStd ("\n  ptax=d.d   -- producer tax.");

	WriteStd ("\n  paydet=d.d -- fraction of bid to pay for detector messages");

	WriteStd ("\nTo control of detector/effector interface sampling rates:");
	WriteStd ("\n  drate=n	-- read messages from detectors every n steps.");
	WriteStd ("\n  erate=n	-- activate matched effectors every n steps.");

	WriteStd ("\nTo control Output:");
	WriteStd ("\n  echo={0|1}		 -- echo commands (for use when input is from a file).");
	WriteStd ("\n  log={on|off|file}  -- log I/O to a file (** not implemented yet **).");
	WriteStd ("\n  demo=n			 -- set demonstration output 'level' to n, where: ");
	WriteStd ("\n						  0 = Off	   1 = Summaries ");
	WriteStd ("\n						  2 = Details from 'major cycle' step.  (99 = DEBUG).");
	WriteStd ("\n						Note that this resets the dml, dcl, and denv flags.");
	WriteStd ("\n  di=n			   -- (auto)display interval, i.e., items with auto-display ");
	WriteStd ("\n						set > 0 displayed every n steps.");
	WriteStd ("\n  The auto-displayable items are:");
	WriteStd ("\n	dml={0|format}	 -- autodisplay message list.");
	WriteStd ("\n	dcl={0|format}	 -- autodisplay classifier list. ");
	WriteStd ("\n	dv={0|format}	  -- autodisplay variables.");
	WriteStd ("\n	denv={0|format}	-- autodisplay environment state.");

	WriteStd ("\nOther utility controls (for re-starting runs, etc.):");
	WriteStd ("\n  cstep=n	-- cycle step (major cycle clock).");
	WriteStd ("\n  randseed=n -- seed for pseudo-random number generator.");

	WriteStd ("\n  lmf=file   -- default file for LOADM (load messages) command.");
	WriteStd ("\n  lcf=file   -- default file for LOADC (load classifiers) commmand.");
    } else if (strcmp (ParStr, "display") == 0) {
	WriteStd ("\nDISPLAY-able items. Each item should be of the form:");
	WriteStd ("\n	  item[,format[,fname]]");
	WriteStd ("\nwhere 'format' is an integer from 0..99 which specifies the");
	WriteStd ("\nlevel of detail and/or format of the display, and where");
	WriteStd ("\n'fname' is the name of a destination file. The items:\n");

	WriteStd ("\n  env		  state of 'environment'");
	WriteStd ("\n  ml		   current message list");
	WriteStd ("\n  cl		   current classifier list");
	WriteStd ("\n  vars		 values of system variables");
	WriteStd ("\n  settings	 values of system settings, constants, etc. ");
	WriteStd ("\n  time		 current time.");
	WriteStd ("\n  banner	   CFS-C version and date and time of run.");
	WriteStd ("\n\nBy default, items are displayed on the screen (stdout)");
	WriteStd ("\nin a format deemed 'reasonable' for the item.");
    } else
	WriteStd ("\n Illegal parameter on HELP command. Enter HELP for help.");

    WriteStd ("\n");
#endif

}					/* Help */
