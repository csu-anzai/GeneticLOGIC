/*
GETDVARS: Program to extract DISPLAY VARS data from log produced by CFS-C program.
This extracts information about the application of "discovery" algorithms.

See the Help() subroutine for an explanation of what this does
and how to use it.

Notes:

1. Some limits:
	NMDISSZ	 max number of data points (displays of classifiers)

2. There is lots of stuff in here I don't use anymore, and I plan to
   clean it out as soon as I can.
*/

#include "stdio.h"
#include "ctype.h"
#include "math.h"

#include "compiler.h"
#include  "utility.h"
#include "cfsio.ext"
unsigned int   URndSd;   /* we need this to include UTILITY.C at link time. */

#define   NMDISSZ	200

unsigned int	index;
unsigned int	NmStored;
unsigned int	Step[NMDISSZ];

unsigned int	TOTNmOfs[NMDISSZ];
unsigned int	TotMu[NMDISSZ];
unsigned int	TotBkgGA[NMDISSZ];
unsigned int	TotBP[NMDISSZ];
unsigned int	TotCDM[NMDISSZ];
unsigned int	TotCDML[NMDISSZ];
unsigned int	TotCDMC[NMDISSZ];
unsigned int	TotCEf[NMDISSZ];
unsigned int	TotACPC[NMDISSZ];
unsigned int	TotCSS[NMDISSZ];
unsigned int	TotTLB[NMDISSZ];

float	TTOTNmOfs[NMDISSZ];
float	TTotMu[NMDISSZ];
float	TTotBkgGA[NMDISSZ];
float	TTotBP[NMDISSZ];
float	TTotCDM[NMDISSZ];
float	TTotCDML[NMDISSZ];
float	TTotCDMC[NMDISSZ];
float	TTotCEf[NMDISSZ];
float	TTotACPC[NMDISSZ];
float	TTotCSS[NMDISSZ];
float	TTotTLB[NMDISSZ];

char   inline[255];
FILE  *InFptr;
int	NumFilC = 0;
int	numfiles, filenum;

VOID   StoreVar(), PrntFull(), PrntVar(), Help(), PrntTFull();
char  *GetInt();


/********************

*********/

VOID  main ( argc, argv )
	int	argc;
	char  *argv[];
{
	int	ret, len, i, drivecd;
	char   *cptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2();
	short  plainflg, headflg, fullflg, noflg, muflg, bgaflg, bpflg, cdmflg,
		   cdmlflg, cdmcflg, ceflg, acpcflg;

	if ( argc == 1 )
	{   Help();
		exit();
	}

	drivecd = 0;
	path[0] = '\0';

	for ( index = 0; index < NMDISSZ; ++index )  {
		Step[index] = 0 ;
		TTOTNmOfs[index] = TTotMu[index]   = 0;
		TTotBkgGA[index] = TTotBP[index]   = TTotCDM[index] = 0;
		TTotCDML[index]  = TTotCDMC[index] = TTotCEf[index] = 0;
		TTotACPC[index]  = TTotCSS[index] = TTotTLB[index] = 0;
	}

	NmStored = 0;
	plainflg = fullflg = headflg = noflg = muflg = bgaflg = bpflg = 
			   cdmflg = cdmlflg = cdmcflg = ceflg = acpcflg = FALSE;

		/* get the paramters and set the flags */

	for ( i = 2; i < argc; ++i ) {
		if ( strcmp( argv[i], "f" ) == 0 )
			fullflg = TRUE;
		else if ( strcmp( argv[i], "p" ) == 0 )
			plainflg = TRUE;
		else if ( strcmp( argv[i], "no" ) == 0 )
			noflg = TRUE;
		else if ( strcmp( argv[i], "mu" ) == 0 )
			muflg = TRUE;
		else if ( strcmp( argv[i], "bga" ) == 0 )
			bgaflg = TRUE;
		else if ( strcmp( argv[i], "bp" ) == 0 )
			bpflg = TRUE;
		else if ( strcmp( argv[i], "cdm" ) == 0 )
			cdmflg = TRUE;
		else if ( strcmp( argv[i], "cdml" ) == 0 ) 
			cdmlflg = TRUE;
		else if ( strcmp( argv[i], "cdmc" ) == 0 )
			cdmcflg = TRUE;
		else if ( strcmp( argv[i], "ce" ) == 0 )
			ceflg = TRUE;
		else if ( strcmp( argv[i], "acpc" ) == 0 )
			acpcflg = TRUE;
		else {
			printf("\nIllegal parameter '%s', ignored.", argv[i] );
			printf("\nFor help, enter getdvars \n");
		}
	}

		/* lets get the basic filename and perhaps a number of files */
	   /* could be  filename  or  filename,3  or some other digit */
	cptr = StpTok2( argv[1], buff, sizeof(buff), " ," );
	if ( *cptr == '\0' ) {
		strcpy( basefnam, buff );
		numfiles = 1;
	} 
	else {
		strcpy( basefnam, buff );
		if ( sscanf( cptr, "%d", &numfiles ) != 1 ) {
			printf("\n\nIllegal first parameter (%s)--enter getvars for help.\n\n", argv[1] );
			exit();
		}
		else if ( numfiles < 1 || numfiles > 10 ) {
			printf("\n\nIllegal number of files on first parameter (%s)--enter getvars for help.\n\n",
					 argv[1] );
			exit();
		}
	}

		/* From each file, read lines until done, filling the tables when appropriate */

	for ( filenum = 0; filenum < numfiles; ++filenum ) {
		index = 0;
		strcpy( fname, basefnam );				  /* get base name */

		if ( numfiles > 1 ) {					/* more than one file to process */
			sprintf( buff, "%d", filenum );
			strcat( fname, buff );
		}

		if ( (InFptr=fopen( fname, READMODE )) == NULL ) {
			printf("\n\nERROR--can't open log file '%s': no data collected from it.\n\n", fname );
			if ( numfiles == 1 )  {
				printf("For help, enter getvars .\n" );
				exit();
			}
			continue;
		}

		for ( index = 0; index < NMDISSZ; ++index )  {
			TOTNmOfs[index] = TotMu[index] = 0;
			TotBkgGA[index] = TotBP[index] = TotCDM[index] = 0;
			TotCDML[index] = TotCDMC[index] = TotCEf[index] = 0;
			TotACPC[index] = TotCSS[index] = TotTLB[index] = 0;
		}

		if ( !plainflg )  {
			if ( filenum != 1 )
				printf( "\n" );
			printf( "\nExtracting CFS-C data from from the file " );
				/*  lattice c version...
					i = getcd( 0, path );
					drivecd = 'A' + getdsk();		
					printf( "%c:\\%s\\", drivecd, path );
				*/
#if ( LATTICEC )
			getcwd( path, sizeof(path) );
			printf( "%s\\", path );
#endif
			printf( "%s ...\n", fname );
		}

		if ( headflg ) {
			while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
				printf( "\n%s", inline ); 
				if ( strcmp( inline, "C-? ; END-HEADER" ) == 0 ) 
					break;
			}
			printf( "\n" );
			headflg = FALSE;				/* only do it once per set of files */
		}

		index = 0;

		while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
			if ( strcmpn( inline, "System Variables", 16 ) == 0 )
				StoreVar( );
		}

		if ( fullflg ) 
			PrntFull( plainflg );

		if ( noflg )
			PrntVar( 1 );

		if ( muflg ) 
			PrntVar( 2 );

		if ( bgaflg )
			PrntVar( 3 );

		if ( bpflg ) 
			PrntVar( 4 );

		if ( cdmflg )
			PrntVar( 5 );

		if ( cdmlflg ) 
			PrntVar( 6 );

		if ( cdmcflg ) 
			PrntVar( 7 );

		if ( ceflg ) 
			PrntVar( 8 );

		if ( acpcflg )
			PrntVar( 9 );

		if ( fclose( InFptr ) != 0 ) 
			printf("\n\nERR: couldn't close log file '%s'!!\n", fname );

		++NumFilC;

	}   /* end of one file  */

	if ( NumFilC > 1 ) {
		for ( i = 0; i < NmStored && i < NMDISSZ; ++i ) {
			TTOTNmOfs[i] /= NumFilC;
			TTotMu[i]	/= NumFilC;
			TTotBkgGA[i] /= NumFilC;
			TTotBP[i]	/= NumFilC;
			TTotCDM[i]   /= NumFilC;
			TTotCDML[i]  /= NumFilC;
			TTotCDMC[i]  /= NumFilC;
			TTotCEf[i]   /= NumFilC;
			TTotACPC[i]  /= NumFilC;
			TTotCSS[i]  /= NumFilC;
			TTotTLB[i]  /= NumFilC;
		}
	
		PrntTFull( plainflg );
	}

}  /* main */


/********************

*********/

VOID  StoreVar  ( )
{
	unsigned int step;
	int  ret, len, i;
	float f1, f2;

/*
System Variables at end of Major-Cycle step #2 : 

Number of classifiers:		   10 (Max 10)
Num. Candidate-classifiers:	   0;	 0 Cfs. won (  0 posted msgs).
Support for bidders:			0.0
Average (Hi,Low) bids:		  ---
Num. candidate matches:		   0
Number of messages:			   0   (Max-Int 24, Max 32)

High/Low strength Cfs:	 1032.6 (9) /	950.4 (7)
Total strength (ave):	  9859.8  (986.0)
Average BidRatio:			0.76
System treasury:		  30009.9
Num of bids > strength:	 0

TOTNmBid	  0  TOTNmWin	  0  TOTCfPst	  0
TOTMtch	   0  TOTMsPrd	  0  TOTMsPst	  0
TOTEMtch	  0  TOTEMsg	   0  TOTEAct	   0
TOTNmPRw	  0  TOTNmNRw	  0
TOTSyRwP	  1  TOTSyRwN	  0
TOTNmOfs	  0  TotMu		 0  TotBkgGA (BP 0)	  0
TotBGASC	  0  TotBGAFC	  0  TotBGANC	  0
TotCDM		0  TotCDML	   0  TotCDMC	   0 (Loci/Cond ----)
TotCEf		0 (Wrg 0, Bd 0)   TotACPC  0   TotCSS  0   TotTLB  0
*/

	sscanf( inline, "System Variables at end of Major-Cycle step #%d)", &step );

	Step[index] = step;

	while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
		if ( strcmpn( inline, "TOTNmOfs", 8 ) == 0 ) 
			break;
	}

	if ( strcmpn( inline, "TOTNmOfs", 8 ) != 0 ) {
		printf("\nStoreVar: Didn't find TOTNmOfs line." );
		exit( ERROR );
	}
		
	sscanf( inline, "TOTNmOfs %d TotMu %d TotBkgGA (BP %d) %d", 
		  &TOTNmOfs[index], &TotMu[index], &TotBP[index], &TotBkgGA[index] );

	ret=ReadS( inline, sizeof(inline), InFptr, &len);

	ret=ReadS( inline, sizeof(inline), InFptr, &len);
	sscanf( inline, "TotCDM %d TotCDML %d TotCDMC %d",
			 &TotCDM[index], &TotCDML[index], &TotCDMC[index] );

	ret=ReadS( inline, sizeof(inline), InFptr, &len);
	sscanf( inline, "TotCEf %d (Wrg %d, Bd %d) TotACPC %d TotCSS %d TotTLB %d",
		 &TotCEf[index], &i, &i, &TotACPC[index], &TotCSS[index], &TotTLB[index] );

	TTOTNmOfs[index] += TOTNmOfs[index];
	TTotMu[index]	+= TotMu[index];
	TTotBkgGA[index] += TotBkgGA[index];
	TTotBP[index]	+= TotBP[index];
	TTotCDM[index]   += TotCDM[index];
	TTotCDML[index]  += TotCDML[index];
	TTotCDMC[index]  += TotCDMC[index];
	TTotCEf[index]   += TotCEf[index];
	TTotACPC[index]  += TotACPC[index];
	TTotCSS[index]  += TotCSS[index];
	TTotTLB[index]  += TotTLB[index];

	++index;
	if ( NumFilC == 0 )
		++NmStored;

}  /* StoreVar */

/********************

*********/

VOID  PrntFull ( Plain )
	short Plain;
{
	int i;

	if ( !Plain ) 
		printf("\nStep  | TotOf | TotMu | BkgGA |  BP   |  CEf  |  CDM  | CDML  | CDMC  |  ACPC  |  CSS   |  TLB" ); 

	for ( i = 0; i < NmStored && i < NMDISSZ; ++i ) {
		if ( i % 10 == 0 && !Plain )
			printf("\n------|-------|-------|-------|-------|-------|-------|-------|-------|------" );
		if ( !Plain ) 
			printf("\n%5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d",
			   Step[i], TOTNmOfs[i], TotMu[i], TotBkgGA[i], TotBP[i],
						TotCEf[i], TotCDM[i], TotCDML[i], TotCDMC[i], TotACPC[i], TotCSS[i], TotTLB[i] );
		else
			printf("\n%5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d %5d",
			   Step[i], TOTNmOfs[i], TotMu[i], TotBkgGA[i], TotBP[i],
						TotCEf[i], TotCDM[i], TotCDML[i], TotCDMC[i], TotACPC[i], TotCSS[i], TotTLB[i] );
	}

}  /* PrntFull */


/********************

*********/

VOID  PrntTFull ( Plain )
	short Plain;
{
	int i;

	if ( !Plain ) {
		printf("\nAverages for %d files:\n", NumFilC );
		printf("\nStep  | TotOf | TotMu | BkgGA |  BP   |  CEf  |  CDM  | CDML  | CDMC  |  ACPC |  CSS |  TLB " ); 
	}

	for ( i = 0; i < NmStored && i < NMDISSZ; ++i ) {
		if ( i % 10 == 0 && !Plain )
			printf("\n------|-------|-------|-------|-------|-------|-------|-------|-------|------|------|------" );
		if ( !Plain ) 
			printf("\n%5d | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f",
			   Step[i], TTOTNmOfs[i], TTotMu[i], TTotBkgGA[i], TTotBP[i],
						TTotCEf[i], TTotCDM[i], TTotCDML[i], TTotCDMC[i], TTotACPC[i], TTotCSS[i], TTotTLB[i] );
		else
			printf("\n%5d %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f",
			   Step[i], TTOTNmOfs[i], TTotMu[i], TTotBkgGA[i], TTotBP[i],
						TTotCEf[i], TTotCDM[i], TTotCDML[i], TTotCDMC[i], TTotACPC[i], TTotCSS[i], TTotTLB[i] );
	}

}  /* PrntTFull */

/********************

*********/

VOID  PrntVar ( Type )
	int  Type;
{

	for ( index = 0; index < NmStored; ++index ) {
		printf("\n%5d", Step[index] );
		if ( Type == 1 ) 
			printf(" %5d", TOTNmOfs[index] );
		else if ( Type == 2 )
			printf(" %5d", TotMu[index] );
		else if ( Type == 3 )
			printf(" %5d", TotBkgGA[index] );
		else if ( Type == 4 )
			printf(" %5d", TotBP[index] );
		else if ( Type == 5 )
			printf(" %5d", TotCDM[index] );
		else if ( Type == 6 )
			printf(" %5d", TotCDML[index] );
		else if ( Type == 7 )
			printf(" %5d", TotCDMC[index] );
		else if ( Type == 8 )
			printf(" %5d", TotCEf[index] );
		else if ( Type == 9 )
			printf(" %5d", TotACPC[index] );
		else if ( Type == 10 )
			printf(" %5d", TotCSS[index] );
		else if ( Type == 11 )
			printf(" %5d", TotTLB[index] );
	}

}  /* PrntVar */

/********************

*********/

VOID Help ( )
{

	printf("\nGETDVARS extracts data from a log file produced by CFS-C program:");
	printf("\nIn paritcular, it extracts counts of the activity of various");
	printf("\ndiscovery algorithms (e.g., total offspring, total mutations, etc.");
	printf("\nThe log must have had DISPLAY VARS auto-displayed periodically.");
	printf("\nProgram should be invoked by:");
	printf("\n  getdvars  logfile f p h" );
	printf("\nwhere:");
	printf("\n  logfile   is the name of a file containing the log you");
	printf("\n			want to analyze. It may also be of the form:");
	printf("\n  log.lg,n  where n is an integer 1..10, in which case data will");
	printf("\n			extracted from the files log.lg0, log.lg1,...,log.lg(n-1)");
	printf("\n			and it will be averaged over those files.");
	printf("\n  f		 means extract counts for all discovery algorithms.");
	printf("\nThe data is displayed in columns labeled:");
	printf("\nTotOf   Total offspring			  CDML Cover Detector Loci" );
	printf("\nTotMu   Total mutations			  CDMC Cover Detector Conditions");
	printf("\nBkgGA   Total BkgGA applications	 ACPC Asynchr. Couple Profit. Cf"); 
	printf("\nBP	  Number of Bidding Parents	CSS  Couple Stage Setters");
	printf("\nCEf	 Cover Effector Count		 TLB  Trigger on Low Bids");
	printf("\nCDM	 Cover Detector Count");

	printf("\n[press any key to see next screen]");
	getch();

	printf("\nAll other parameters are optional.");
	printf("\n");
	printf("\n  p		 Include to produce 'plain' output, i.e.,");
	printf("\n			without titles, bars, and lines (for input to some grapher).");
	printf("\n");

	printf("\n  h		 Include to echo all lines read until it encounters");
	printf("\n			a line with the string:");
	printf("\n				; END-HEADER");
	printf("\n			This provides easy way to get runtime parameter settings");
	printf("\n			and pre-run notes.");
	printf("\n");

}  /* Help  */


/**
strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .
**/

int  strcmpn ( Str1, Str2, N )
	char  Str1[], Str2[];
	int   N;
{
	int  ret, i;
	
	for ( ret = 1, i = 0; Str1[i] == Str2[i] && i < N; ++i )
		;

	if ( i == N ) 
		ret = 0;

	return( ret );

} /* strcmpn */

