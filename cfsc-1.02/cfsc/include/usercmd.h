/*		USERCMD.H	for the CFS-C Classifier System.
 
Definitions of constants for the user command processor.
Files that reference these should contain
	#include USERCMD.H  
The files that use these are:
	CFSC.C  - the main program.
	USERCMD.C - the functions to prompt for and read in user commands.
        
First, some general-purpose  constants.
**/

#define		CMDNAMSZ		32		/* room for a command name. */ 
#define		CMDPARSZ		128		/* room for all the parameters on one command. */
#define		CMDLINSZ		CMDNAMSZ+CMDPARSZ	/* room for whole command line */
#define		PARNAMSZ		32		/* room for a parameters name. */
#define		PARVALSZ		32		/* room for a parameter value. */     

#define		MINTABCH		'|'		/* minimum abbreviation char used in tables. */


/**
	Codes for user- commands. 
**/

#define		ERROR_CC	-1		/* illegal command entered */
#define		NULL_CC		0		/* null line - its ok  but nothing to do */
#define		STOP_CC		1
#define		LOADC_CC	2		/* load classifiers */
#define		LOADM_CC	3		/* load messages    */
 
#define		DISPL_CC	4		/* display command */
#define		SET_CC		5
#define		CLASS_CC	6		/* classify command */
#define		STEP_CC		7
 
#define		MDCF_CC		8		/* modify a classifier */
#define		MDMSG_CC	9		/* modify a message */
#define		APCF_CC		10		/* append classifiers to the list */
#define		APMSG_CC	11		/* append messages to the list */

#define		HELP_CC		12
#define		LOADE_CC	13		/* load environment from a file */
#define		SAVES_CC	14		/* save entire system for re-loading */

#define		GENRC_CC	15		/* generate random classifiers */

#define		ECMD_CC		16		/* execute an enviornment-specific command */

#define		CMDS_CC		17		/* read commands from file */

/**
	Codes for the core variables in the CVTable (and related CVxTables's).
**/

#define  AUTODSPL   1     /* AutoDspl flag */

#define  ADCYCINT   2     /* ADCycInt  */
#define  ADCYCFMT   3     /* ADCycFmt  */
#define  ADCMINT    4     /* ADCMInt   */
#define  ADCMFMT    5     /* ADCMFmt   */

#define  ADDMINT    6     /* ADDMFmt   */
#define  ADDMFMT    7     /* ADDMFmt   */
#define  ADCFINT    8     /* ADCFFmt   */
#define  ADCFFMT    9     /* ADCFFmt   */ 

#define  ADVARINT  10     /* ADVarFmt  */
#define  ADVARFMT  11     /* ADVarFmt  */
#define  ADENVINT  12     /* ADEnvFmt  */
#define  ADENVFMT  13     /* ADEnvFmt  */


#define  SAVEINT   21
#define  SAVEFNUM  22
#define  SAVESYFN  23
#define  SAVESYPA  24
 
#define  LOGFLG    25
#define  LOGFNAME  26
#define  ECHOFLG   27
#define  STDOUTFN  28
#define  STDINFN   29
#define  DISOUTFN  30
#define  GENFLG    31
#define  GENFNAME  32

#define  DEMOLEV   35 
#define  DSPLMSBL  36
#define  DSPLCFBL  37
#define  DSCFLST   38

#define  CYCLESTP  41 
#define  URNDSD    42
#define  CLAMPFLG  43

#define  MSGINFN   51
#define  NMIMSGMX  52
#define  NMDMSGMX  53
#define  MSGINTDF  54
#define  NMCMSGS   55
#define  NMOMSGS   56
#define  NMOMSGIN  57
#define  DETMSGMN  58

#define  CFINFN    61
#define  NMCFSMX   62
#define  NMCFS     63
#define  CFSTRDF   64
#define  CFSUPPDF  65
#define  CFSTRMAX  66
#define  CFBIDMIN  67
#define  TOTCFSTR  68
#define  TOTCFBR   69
#define  NXTCFID   70
#define  CFSTRMIN  71

#define  ONEMPERC  72
#define  DELHALL   73
#define  MXDUPMSG  74

#define  SYSTREAS  81
#define  SYSREW    82
#define  FRPAYDET  83
#define  BID_K     84
#define  HEADTAX   85
#define  BIDTAX    86
#define  PRDTAXMX  87
#define  SHAREREW  88
#define  ALLPAYBD  89
#define  SUPPBID   90
#define  BRPOW     91
#define  FRPAYNEC  92
#define  BIDPOW    93
#define  EBRPOW    94
#define  PRDTAXPW  95
#define  DMSHARE   96
#define  BBTYPE    97
#define  BCSB      98
#define  BCMB      99
#define  AHEADTAX  100

#define  NMIEMMX   110
#define  NMIIMMX   111

#define  DETECTRT  121
#define  EFFECTRT  122
#define  ENVINFN   123

#define  TOTDTMSG  201
#define  TOTMTCH   202
#define  TOTNMBID  203
#define  TOTNMWIN  204
#define  TOTCFPST  205
#define  TOTMSPRD  206
#define  TOTMSPST  207
#define  TOTEMTCH  208
#define  TOTEAMSG  209
#define  TOTEACT   210
#define  TOTNMPRW  211
#define  TOTNMNRW  212
#define  TOTSYRWP  213
#define  TOTSYRWN  214

#define  GRCFRMC   301  /* gen random cfs */
#define  GRCFRNSL  302

#define  DSCDEMO   401  /* discovery vars */

#define  TOTNMOFS  402
#define  PRNTBPOW  403
#define  RPLCBPOW  404
#define  RANDRPLC  405
#define  RPLCFUBD  406
#define  RPLACUBD  407
#define  RPLACSBD  408
#define  PKPRNWOR  410
#define  FRNEWCFS  411
#define  CROWDFAC  412 
#define  MXCFCOPY  413
#define  BIDFIT    414

#define  NWCFSTRF  420
#define  NWCFSTRV  421

#define  BKGGART   431
#define  BGASCPR   432
#define  BGAFCPR   433
#define  BGANCPR   434
#define  TOTBKGGA  435
#define  TOTBGASC  436
#define  TOTBGAFC  437
#define  TOTBGANC  438
#define  BGABPPR   439
#define  TOTBGABP  440

#define  MUPRTOT   451
#define  TOTMU     452
#define  MUPRCND1  453
#define  MUPRCND2  454
#define  MUPRACT   455
#define  MUPRCT2   456
#define  MUPRAT    457
#define  MUFRNSL   458

#define  CDMSGS    471
#define  CDMSGSRT  472
#define  TOTCDM    473
#define  TOTCDMC   474
#define  TOTCDML   475

#define  CEFFS     491
#define  CEFFSRT   492
#define  TOTCEF    493
#define  TOTCEFW   494
#define  TOTCEFB   495
#define  CEFFSPLZ  496

#define  ACPCRT    511
#define  ACPCT     512
#define  TOTACPC   513
#define  NWCCSTRF  514
#define  TAGLMX    515
#define  TAGRMX    516

#define  CSSRT     521
#define  TOTCSS    522
#define  CSSFRM    523
#define  CSSFRRF   524

#define  TLBRT     531
#define  TOTTLB    532
#define  TLBFRE    533
#define  TLBT      534

#define  ENDTABLE  USINTMAX
/**/
#define  CVNAMESZ   2            /* Up to two names for each CVTable variable. */
#define  CVNAMEMX  (CVNAMESZ-1)

#define  SHORT      1            /* Define types of variables stored as the CVType value in  */
#define  INT        2            /* CVSNode and CVNode structures */
#define  UNSIGNED   3
#define  LONG       4
#define  FLOAT      5
#define  CHARPTR    6

   /* Define CVSpType values. 0 is 'none', so don't use it! */

#define  CVSTAD     1     /* AutoDspl 'level' variable */
#define  CVSTADI    2     /* 'Auto-display interval' variable type */

#define  CVSTRTI    3     /* 'interval' variables, e.g., save-interval, etc. */

#define  CVSTMLS    4     /* message-list size control -- share reserved of detectors
                             versus maximum number of internally generated messages 
                          */

#define  CVSTLOG    5     /* Set Log on, off, or to a file */
#define  CVSTMSGL   6     /* Set Message List limits: NmIMsgMx or NmDMsgMx */

#define  CVSTSOF    7     /* Set StdOutFN, the destination file/device for 
                             all output by the WriteStd() subroutine.
                          */

#define  CVSTDOF    8     /* Set DisOutFN, the default destination file/device for 
                             all output by ALL the DsplXXXX() subroutines.
                             (e.g., DsplMsgs, DsplCfs, DsplEnv, DsplVars).
                          */

#define  CVSTGEN    9     /* Set Geneolgy on, off, or to a file */

#define  CVSTDCFL  10     /* set selected list of classifiers to display */

#define  CVSTMUPR  11     /* set PuPrTot and recalculate poisson distr. */

#define  CVSTDMSH  12     /* set DMShare, and caluclate DMDscnt */
