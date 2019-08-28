/*
Program to strength data from classifier log produced by CFS-C program.
In paricular, the log must include displays of classifiers
produced by the DISPLAY CL,2 or Cl,22

See Help() for details.

Notes:

1. Some limits:
	NMCFSSZ	 max number of classifiers that can be measured
	NMFILES	 max number of files that can be averaged
	NMSTEPSZ	max number of data points (displays of classifiers)

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
short	int   EchoFlg;

#define   NMCFSSZ	  20
#define   NMFILES	  10
#define   NMSTEPSZ	 200
#define   ENDINT	   -1		 /* end-of-data mark */

unsigned int strtable[NMCFSSZ] [NMSTEPSZ];	  /* table of strengths at variables steps */
int	cflist[NMCFSSZ], strstplst[NMSTEPSZ];	/* list of cfs to look for, steps at which they were displayed */
int	strstpndx, cfindx;					   /* indexes into the arrays */
float  tstrtab[NMCFSSZ] [NMSTEPSZ];			 /* table for totals across several files. */

int	NmStrSto;					   /* number of times strength and guesses stored */
int	NmCfIds;

int	EPercent;								/* Percent of end of run to be called 'equilibrium' period. */
int	EStaStp, EEndStp;						/* equil start and end step */
 
int	EStrCnt;								 /* number of strnegth values in average */
float  EqStr[NMCFSSZ][NMFILES];				 /* Cf strengths over 'equilibium' period */
float  EqStrAve[NMCFSSZ], EqStrVar[NMCFSSZ];	/* ave and var over the runs */

int	NumFilC = 0;							 /* number of files from which data was collected */
int	numfiles, filenum;					   /* number of files from command, and file we are working on */

float  ECrtFrac = 0.90;						 /* Criterium for good performance--fraction of 'equilibrium' */
int	ECrtLen  = 100;						  /* Min number of steps at ECrtFrac to meet criteria */
int	ECrtStp  = -1;						   /* First step criterium met */
int	TECrtStp = 0;

char   inline[512];
FILE  *InFptr;

VOID   Init(), GetIDPar(), AskIDs(), StoreStr(), PrntStrs(),
	   PrntTStr(), Help(); 
char  *GetInt(), *GetFloat();
/**/

VOID  main ( argc, argv )
	int	argc;
	char  *argv[];
{
	int	ret, len, err, i, drivecd, cfid;
	short  idparflg, plainflg, headflg, strflg, tstronly;
	float  efrac;
	char   *cptr, *idptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2();

	if ( argc == 1 )
	{   Help();
		exit();
	}

	drivecd = 0;
	path[0] = '\0';
	strflg   = tstronly = idparflg  = plainflg  = headflg = FALSE;

	Init();

		/* get the paramters and set the flags */

	for ( i = 2; i < argc; ++i ) {
		if ( strcmp( argv[i], "p" ) == 0 )
			plainflg = TRUE;
		else if ( strcmp( argv[i], "h" ) == 0 )
			headflg = TRUE;
		else if ( strcmp( argv[i], "s" ) == 0 )
			strflg = TRUE;
		else if ( strcmp( argv[i], "sto" ) == 0 )
			strflg = tstronly = TRUE;
		else if ( *argv[i] == 'e' ) {		 
			cptr = argv[i];
			++cptr;
			cptr = GetInt( cptr, &EPercent, -1, " ", &err );
			if ( err  || EPercent < 1 || EPercent > 100 ) {
				printf("\nValue after 'e' must be integer from 1 to 100. Using 25.");
				EPercent = 25;
			}
		}
		else if ( strcmpn( argv[i], "cf", 2 ) == 0 ) {
			cptr = argv[i];
			++cptr;  ++cptr;
			cptr = GetInt( cptr, &ret, -1, " ", &err );
			if ( err  || ret < 1 || ret > 100 ) {
				printf("\nValue after 'cf' must be integer from 1 to 100. Using 90.");
				ECrtFrac = 0.90;
			}
			else
				ECrtFrac = ret / 100.0;
		}
		else if ( strcmpn( argv[i], "cl", 2 ) == 0 ) {
			cptr = argv[i];
			++cptr;  ++cptr;
			cptr = GetInt( cptr, &ECrtLen, -1, " ", &err );
			if ( err  || ECrtLen < 1 ) {
				printf("\nValue after 'cl' must be integer > 1. Using 100.");
				ECrtFrac = 100;
			}
		}
		else if ( *argv[i] == 'i' ) {
			idptr = argv[i];
			++idptr;
			idparflg = TRUE;
		}
		else {
			printf("\nIllegal parameter '%s', ignored.", argv[i] );
			printf("\nFor help, enter getstr1\n");
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
			printf("\n\nIllegal first parameter (%s)--enter GETSTR1 for help.\n\n", argv[1] );
			exit();
		}
		else if ( numfiles < 1 || numfiles > 10 ) {
			printf("\n\nIllegal number of files on first parameter (%s)--enter GETSTR1 for help.\n\n",
					 argv[1] );
			exit();
		}
	}

	if ( idparflg ) 
		GetIDPar ( idptr );
	else if ( strflg ) 
		AskIDs();

		/* From each file, read lines until done, filling the tables when appropriate */

	for ( filenum = 0; filenum < numfiles; ++filenum ) {
		strcpy( fname, basefnam );				  /* get base name */

		if ( numfiles > 1 ) {					/* more than one file to process */		
			sprintf( buff, "%d", filenum );
			strcat( fname, buff );
		}

		if ( (InFptr=fopen( fname, READMODE )) == NULL ) {
			printf("\n\nERROR--can't open log file '%s': no data collected from it.\n\n", fname );
			if ( numfiles == 1 )  {
				printf("For help, enter GETSTR1 \n" );
				exit();
			}
			continue;
		}

		if ( !plainflg )  {
			printf( "\nExtracting CFS-C data from from the file " );
				 /* i = getcd( 0, path );
					drivecd = 'A' + getdsk();
					printf( "%c:\\%s\\", drivecd, path );
				 */
#if LATTICEC
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

		strstpndx = 0;

		while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
			if ( strcmpn( inline, "; TEST", 6 ) == 0 ||
				 strcmpn( inline, "C-? ; TEST", 10 ) == 0 ) {
				while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF )
					if ( strcmpn( inline, "; ENDTEST", 9 ) == 0 ||
						 strcmpn( inline, "C-? ; ENDTEST", 13 ) == 0 )  
						break;
			}
			else if ( strflg && strcmpn( inline, "Current Classifiers", 19 ) == 0 )
				StoreStr( );
		}

		efrac = 1.0 - ( EPercent / 100.0 );

		EStaStp  = Round ( EEndStp * efrac );

		if ( strflg && !tstronly )
			PrntStrs( plainflg );

		if ( fclose( InFptr ) != 0 ) 
			printf("\n\nERR: couldn't close log file '%s'!!\n", fname );

		++NumFilC;

	}   /* end of one file  */

		/* Now print the total strength and performance tables */

	if ( strflg && NumFilC > 1 )
		PrntTStr( plainflg );

}  /* main */
/**/

VOID  Init()
{
	int  i;

	for ( cfindx = 0; cfindx < NMCFSSZ; ++cfindx )  {
		for ( strstpndx = 0; strstpndx < NMSTEPSZ; ++strstpndx ) {
			strtable [cfindx] [strstpndx] = 0;
			tstrtab [cfindx] [strstpndx] = 0;
		}
		for ( i = 0; i < NMFILES; ++ i )
			EqStr[cfindx][i] = 0;
		EqStrAve[cfindx] = EqStrVar[cfindx] = 0;
	}

	NmStrSto = NmCfIds = 0;

	EPercent  = 25;

}  /* Init */
/**/

VOID  PrntTStr( PlainFlg )
	short  PlainFlg;
{
	int  linecnt, i;
	float  f;
 
	if ( !PlainFlg ) {
		printf("\n\n\fAverage strengths for selected classifiers:");
		printf("\n\n	   Cf_Ids ->\n Step ");	   
		for ( i = 0; i < NmCfIds; ++i )
			printf("%6d ", cflist[i] );
		printf("\n------");
		for ( i = 0; i < NmCfIds; ++i )
			printf("|------");
	}

	for ( linecnt = strstpndx = 0; strstpndx < NmStrSto; ++strstpndx ) {
		printf("\n%5d ", strstplst[strstpndx] );

		for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
			if ( PlainFlg )
				printf(" %5.0f ", tstrtab [cfindx] [strstpndx] / NumFilC ); 
			else
				printf("|%5.0f ", tstrtab [cfindx] [strstpndx] / NumFilC ); 

		if ( ++linecnt % 4 == 0 && !PlainFlg ) {
			printf("\n------");
			for ( i = 0; i < NmCfIds; ++i )
				printf("|------");
		}
	}

	if ( !PlainFlg ) {
		printf("\n\nEquilibrium strengths (step %d to end):\n\n	  ", EStaStp );

		if ( EStrCnt != 0 ) {
				 /* calc and print mean */
			for ( i = 0; i < NumFilC; ++i )
				for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) {
					EqStr[cfindx][i] /= EStrCnt;
					EqStrAve[cfindx] += EqStr[cfindx][i];
				}
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) {
				EqStrAve[cfindx] /= NumFilC;
				printf(" %.1f ", EqStrAve[cfindx] ); 
			}
				/* calc and print var */
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) {
				for ( i = 0; i < NumFilC; ++i ) {
					f = EqStr[cfindx][i] - EqStrAve[cfindx];
					EqStrVar[cfindx] += f * f;
				}
			}
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
				EqStrVar[cfindx] = sqrt( EqStrVar[cfindx] / (NumFilC - 1.0) );
			printf("\n  SD: " );
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
				printf(" %.1f ", EqStrVar[cfindx] ); 
			
		}
		else
			printf("(no strengths?)");
	}

}  /* PrntTStr  */


VOID  GetIDPar ( idptr )
	char *idptr;
{
	int err, cfid;

	for ( cfindx = 0; cfindx < NMCFSSZ; ++cfindx ) {
		idptr = GetInt( idptr, &cfid, -1, ",", &err );
		if ( err ) {
			printf("\nIllegal Cf_Id in run parameter.\n");
			exit();
		}
		else if ( cfid == -1 )
			break;
		else {
			cflist [cfindx] = cfid;
			++NmCfIds;
		}
	}

}  /* GetIDPar */


VOID  AskIDs ( )
{
	int  len, cfid, err, ret;
	char *cptr;

	printf("\nEnter Id's for classifiers to be extracted (press RETURN after last one):\n");

	for ( cfindx = 0; cfindx < NMCFSSZ; ++cfindx ) {
		printf("Cf_Id? ");
		if ( (ret=ReadS( inline, sizeof(inline), stdin, &len)) == EOF || len == 0 )
			break;
		else {
			cptr = GetInt( inline, &cfid, -1, " ", &err );
			if ( cfid == -1 || err  ) 
				printf("\nId (%s) must be integer > 0. Try again (RETURN to quit).\n\n", inline );
			else  {
				cflist [cfindx] = cfid;
				++NmCfIds;
			}
		}
	}

}  /* AskIDs */
/**/

VOID  StoreStr ( )
{
	int  step, ret, len, cfid, strength, err, i;
	float f;
	char *cp;

	sscanf( inline, "Current Classifiers (cycle-step %d)", &step );

	strstplst [strstpndx] = step;

	ret=ReadS( inline, sizeof(inline), InFptr, &len);   /* a blank line */
	ret=ReadS( inline, sizeof(inline), InFptr, &len);   /* header line first cf */

		/* see if had header for CL,2 format */
	if ( strcmpn( inline, "Id	S", 4 ) == 0 || strcmpn( inline, "Id    S", 7 ) == 0 ) {
		while ( (ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF ) {
			strength = cfid = 0;
			if ( sscanf( inline, "%d %d", &cfid, &strength ) != 2 )
				break;
			if ( (cfindx = IntInLst( cfid, cflist, NMCFSSZ-1 )) >= 0 )  {
				strtable [cfindx] [strstpndx] = strength;
				tstrtab [cfindx] [strstpndx] += strength;
			}
		}

		++strstpndx;	
		if ( filenum == 0 )  { 
			++NmStrSto;				 /* count of strength lines stored */
			EEndStp  = step;			/* will get the end step eventually! */
		}
	}

		/* see if it is format CL,22 or other 20's */
	else {
		for ( cp = inline, i = 0; *cp != '>' && i < 10 && *cp != '\0'; ++cp, ++i )
			;
		if ( *cp == '>' ) { 
			do {
				for ( cp = inline, i = 0; *cp != '>' && i < 10 && *cp != '\0'; ++cp, ++i )
					;
				if ( *cp != '>' )
					break;
				GetInt( inline, &cfid, -1, ">", &err );
				for ( ; *cp != '{' && *cp != '\0'; ++cp )
					;
				if ( *cp == '\0' )
					break;
				++cp;
				GetFloat( cp, &f, (float) -9999, ",", &err ); 
				if ( (cfindx = IntInLst( cfid, cflist, NMCFSSZ-1 )) >= 0 )  {
					strtable [cfindx] [strstpndx] = f;
					tstrtab [cfindx] [strstpndx] += f;
				}
			}   while ((ret=ReadS( inline, sizeof(inline), InFptr, &len)) != EOF );

			++strstpndx;	
			if ( filenum == 0 )  { 
				++NmStrSto;				 /* count of strength lines stored */
				EEndStp  = step;			/* will get the end step eventually! */
			}
		}
	}

}  /* StoreStr */
/**/

VOID  PrntStrs( PlainFlg )
	short  PlainFlg;
{
	int  linecnt, i;

	EStrCnt =  0;
	for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
		EqStr[cfindx][NumFilC] = 0; 

	if ( !PlainFlg ) {
		printf("\n\nStrengths for selected classifiers:");
		printf("\n\n	   Cf_Ids ->\n Step ");	   
		for ( i = 0; i < NmCfIds; ++i )
			printf("%6d ", cflist[i] );
		printf("\n------");
		for ( i = 0; i < NmCfIds; ++i )
			printf("|------");
		printf("\n");
	}

	for ( linecnt = strstpndx = 0; strstpndx < NmStrSto; ++strstpndx ) {
		printf("%5d ", strstplst[strstpndx] );

		for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
			if ( PlainFlg )
				printf(" %5d ", strtable [cfindx] [strstpndx] ); 
			else
				printf("|%5d ", strtable [cfindx] [strstpndx] ); 

		if ( ++linecnt % 4 == 0 && !PlainFlg ) {
			printf("\n------");
			for ( i = 0; i < NmCfIds; ++i )
				printf("|------");
		}
		printf("\n");

			/* check for start and end of 'equilibirum' (end) of run. */

		if ( strstplst[strstpndx] >= EStaStp )  {
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx )
				EqStr[cfindx][NumFilC] += strtable [cfindx] [strstpndx]; 
			++EStrCnt;
		}
	}

	if ( !PlainFlg ) {
		printf("\n\nEquilibrium strengths (step %d to end):\n\n	  ", EStaStp );

		if ( EStrCnt != 0 )
			for ( cfindx = 0; cfindx < NmCfIds; ++cfindx ) 
				printf(" %.1f ", EqStr[cfindx][NumFilC]/EStrCnt ); 
		else
			printf("(no strengths?)");
	}
	printf( "\n\n" );

}  /* PrntStrs  */
VOID  Help()
{

	printf("\nGETSTR1 extracts strength data from a log file produced by CFS-C.");
	printf("\nRun must have had DISPLAY CL,2  or CL,22");
	printf("\nUsage:");
	printf("\n  getstr1  logfile  p h s e<p> i<id1>,<id2>,...");
	printf("\nwhere:");
	printf("\n  logfile   is the name of a file containing the log you");
	printf("\n			want to analyze. Logfile could be of the form:");
	printf("\n			   logfile.xx,n");
	printf("\n			if you want to analyze data from the files:");
	printf("\n			   logfile.xx0");
	printf("\n			   logfile.xx1  ....");
	printf("\n			   logfile.xx(n-1)");
	printf("\n			Up to 10 can be analyzed and averaged." );
	printf("\n");
	printf("\n[press any key to see next screen]");

#if !CBELLMTS
	getch();
#endif

	printf("\n  p		 Include to produce 'plain' output, i.e.,");
	printf("\n			without titles, bars, and lines (for input to some grapher).");
	printf("\n");
	printf("\n  h		 Include to echo all lines read until it encounters");
	printf("\n			a line with the string:");
	printf("\n				; END-HEADER");
	printf("\n			This provides easy way to get runtime parameter settings");
	printf("\n			and pre-run comments include with display of strengths.");
	printf("\n");
	printf("\n  s		 Include to extract table of classifier strengths.");
	printf("\n			Program will prompt for classifier id numbers");
	printf("\n			(or use i parameter described below).");
	printf("\n");
	printf("\n  i<id1>,.. Specifies list of classifier to include in the table.");
	printf("\n			If this is not included, the program will prompt for ids.");
	printf("\n			<id1>,<id2> and so must be unsigned integers, separated by commas.");
	printf("\n			Do NOT include ANY BLANKS in the id list.");
	printf("\n");
	printf("\n[press any key to see next screen]");

#if !CBELLMTS
	getch();
#endif

	printf("\n  e<p>	  Specifies how much of the end of the run is to be considered");
	printf("\n			the 'equilibrium' state. By default <p> is 25, i.e., the last");
	printf("\n			25% of the run is used to calculate the average performance");
	printf("\n			at equilibrium.");
	printf("\n");
	printf("\n  cf<f>	 Specifies percent of equilibrium rate that meets learning");
	printf("\n			to criteria. By default <f> is 90, i.e., 90% of equilibrium.");
	printf("\n");
	printf("\n  cl<s>	 Specifies number of steps system must have performance above");
	printf("\n			<f> * equilibrium rate to meet criteria. By default <s> is 100.");
	printf("\n");
	printf("\nNote that the parameters can be specified in any order, after the logfile name.");
	printf("\n	getstr ab-bb1.log  h p i2,4,5,6,22,33");
	printf("\nThe table will be displayed on stdout (which can be redirected, of course).");
	printf("\nThe limits:   %d classifiers, displayed %d times in the run.", NMCFSSZ, NMSTEPSZ);
	printf("\n");

}  /* Help */
/**/
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
/**/
/**
IntInLst	Check for int in list (array) of ints.

	Test	Int to look for.

	List	Array of int's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise
**/

int  IntInLst ( Test, List, Max )
	int  Test, List[], Max;
{
	int  ret, i;

	for ( i = 0; i <= Max; ++i ) 
		if ( Test == List[i] )
			break;

	if ( i <= Max )
		ret = i;
	else
		ret = -1;

	return( ret );

}  /* IntInLst */
