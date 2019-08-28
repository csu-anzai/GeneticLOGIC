
/*
Program to extract DISPLAY VARS data from log produced by CFS-C program.
This includes average strength, bidratio, number of bidders, etc.

See the Help() subroutine for an explanation of what this does
and how to use it.

Notes:

1. Some limits:
	NMDISSZ	 max number of data points (displays of classifiers)

2. There is lots of stuff in here I don't use anymore, and I plan to
   clean it out as soon as I can.
*/

#include "config.h"
#include "utility.h"
#include "cfsio.ext"
#include "getvars.h"

unsigned int URndSd;			/* we need this to include UTILITY.C at link time. */


#define   NMDISSZ	 300		/* max num of times data displayed */

unsigned int index;
unsigned int NmStored;
unsigned int Step[NMDISSZ];

unsigned int CandCf[NMDISSZ];
unsigned int CfWon[NMDISSZ];
unsigned int CfPost[NMDISSZ];
unsigned int CandMsg[NMDISSZ];
unsigned int PostMsg[NMDISSZ];
unsigned int HiStr[NMDISSZ];
unsigned int LoStr[NMDISSZ];
unsigned int AveStr[NMDISSZ];
float AveBR[NMDISSZ];

float TCandCf[NMDISSZ];
float TCfWon[NMDISSZ];
float TCfPost[NMDISSZ];
float TCandMsg[NMDISSZ];
float TPostMsg[NMDISSZ];
float THiStr[NMDISSZ];
float TLoStr[NMDISSZ];
float TAveStr[NMDISSZ];
float TAveBR[NMDISSZ];

char inbuf[255];
FILE *InFptr;
int NumFilC = 0;
int numfiles, filenum;

VOID StoreVar (), PrntFull (), PrntVar (), Help (), PrntTFull ();
char *GetInt ();

/*************************

******/

VOID 
main (argc, argv)
    int argc;
    char *argv[];
{
    int ret, len, i, drivecd;
    char *cptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2 ();
    short plainflg, headflg, fullflg, cfcflg, cfwflg, cfpflg, msgcflg, msgpflg, hsflg, lsflg, asflg, brflg;

    if (argc == 1) {
	Help ();
	exit (1);
    }
    drivecd = 0;
    path[0] = '\0';

    for (index = 0; index < NMDISSZ; ++index) {
	Step[index] = TCandCf[index] = TCfWon[index] = 0;
	TCfPost[index] = TCandMsg[index] = TPostMsg[index] = 0;
	THiStr[index] = TLoStr[index] = TAveStr[index] = 0;
	TAveBR[index] = 0;
    }

    NmStored = 0;
    plainflg = fullflg = headflg = cfcflg = cfwflg = cfpflg = msgcflg =
	msgpflg = hsflg = lsflg = asflg = brflg = FALSE;

   /* get the paramters and set the flags */

    for (i = 2; i < argc; ++i) {
	if (strcmp (argv[i], "f") == 0)
	    fullflg = TRUE;
	else if (strcmp (argv[i], "p") == 0)
	    plainflg = TRUE;
	else if (strcmp (argv[i], "cfc") == 0)
	    cfcflg = TRUE;
	else if (strcmp (argv[i], "cfw") == 0)
	    cfwflg = TRUE;
	else if (strcmp (argv[i], "cfp") == 0)
	    cfpflg = TRUE;
	else if (strcmp (argv[i], "msgc") == 0)
	    msgcflg = TRUE;
	else if (strcmp (argv[i], "msgp") == 0)
	    msgpflg = TRUE;
	else if (strcmp (argv[i], "hs") == 0)
	    hsflg = TRUE;
	else if (strcmp (argv[i], "ls") == 0)
	    lsflg = TRUE;
	else if (strcmp (argv[i], "as") == 0)
	    asflg = TRUE;
	else if (strcmp (argv[i], "br") == 0)
	    brflg = TRUE;
	else {
	    printf ("\nIllegal parameter '%s', ignored.", argv[i]);
	    printf ("\nFor help, enter getvars \n");
	}
    }

   /* lets get the basic filename and perhaps a number of files */
   /* could be  filename  or  filename,3  or some other digit */
    cptr = StpTok2 (argv[1], buff, sizeof (buff), " ,");
    if (*cptr == '\0') {
	strcpy (basefnam, buff);
	numfiles = 1;
    } else {
	strcpy (basefnam, buff);
	if (sscanf (cptr, "%d", &numfiles) != 1) {
	    printf ("\n\nIllegal first parameter (%s)--enter getvars for help.\n\n", argv[1]);
	    exit (1);
	} else if (numfiles < 1 || numfiles > 10) {
	    printf ("\n\nIllegal number of files on first parameter (%s)--enter getvars for help.\n\n",
		    argv[1]);
	    exit (1);
	}
    }

   /* From each file, read lines until done, filling the tables when appropriate */

    for (filenum = 0; filenum < numfiles; ++filenum) {
	strcpy (fname, basefnam);	/* get base name */

	if (numfiles > 1) {		/* more than one file to process */
	    sprintf (buff, "%d", filenum);
	    strcat (fname, buff);
	}
	if ((InFptr = fopen (fname, READMODE)) == NULL) {
	    printf ("\n\nERROR--can't open log file '%s': no data collected from it.\n\n", fname);
	    if (numfiles == 1) {
		printf ("For help, enter getvars .\n");
		exit (1);
	    }
	    continue;
	}
	if (!plainflg) {
	    if (filenum != 1)
		printf ("\n");
	    printf ("\nExtracting CFS-C data from from the file ");
	   /*  lattice c version...
					i = getcd( 0, path );
					drivecd = 'A' + getdsk();
					printf( "%c:\\%s\\", drivecd, path );
				*/

#if ( LATTICEC )
	    getcwd (path, sizeof (path));
	    printf ("%s\\", path);
#endif

	    printf ("%s ...\n", fname);
	}
	if (headflg) {
	    while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF) {
		printf ("\n%s", inbuf);
		if (strcmp (inbuf, "C-? ; END-HEADER") == 0)
		    break;
	    }
	    printf ("\n");
	    headflg = FALSE;		/* only do it once per set of files */
	}
	for (index = 0; index < NMDISSZ; ++index) {
	    CandCf[index] = CfWon[index] = 0;
	    CfPost[index] = CandMsg[index] = PostMsg[index] = 0;
	    HiStr[index] = LoStr[index] = AveStr[index] = 0;
	    AveBR[index] = 0;
	}

	index = 0;

	while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF) {
	    if (strcmpn (inbuf, "System Variables", 16) == 0)
		StoreVar ();
	}

	if (fullflg)
	    PrntFull (plainflg);

	if (cfcflg)
	    PrntVar (1);

	if (cfwflg)
	    PrntVar (2);

	if (cfpflg)
	    PrntVar (3);

	if (msgcflg)
	    PrntVar (4);

	if (msgpflg)
	    PrntVar (5);

	if (hsflg)
	    PrntVar (6);

	if (lsflg)
	    PrntVar (7);

	if (asflg)
	    PrntVar (8);

	if (brflg)
	    PrntVar (9);

	if (fclose (InFptr) != 0)
	    printf ("\n\nERR: couldn't close log file '%s'!!\n", fname);

	++NumFilC;

    }					/* end of one file  */

    if (NumFilC > 1) {
	for (index = 0; index < NmStored && index < NMDISSZ; ++index) {
	    TCandCf[index] /= NumFilC;
	    TCfWon[index] /= NumFilC;
	    TCfPost[index] /= NumFilC;
	    TCandMsg[index] /= NumFilC;
	    TPostMsg[index] /= NumFilC;
	    THiStr[index] /= NumFilC;
	    TLoStr[index] /= NumFilC;
	    TAveStr[index] /= NumFilC;
	    TAveBR[index] /= NumFilC;
	}

	PrntTFull (plainflg);
    }
}					/* main */


/*************************

******/

VOID 
StoreVar ()
{
    unsigned int step;
    int ret, len, i;
    float f1, f2;
    char *cp, *GetFloat ();

/*
System Variables at end of Major-Cycle step #10 :

Number of classifiers:		  100 (Max 100)
Num. Candidate-classifiers:	   9;	 6 Cfs. won (  4 posted msgs).
Support for bidders:		 1500.0
Average (Hi,Low) bids:	   104.84 (140.80,26.78)
Num. candidate matches:		  30
Number of messages:			   4   (Max-Int 24, Max 32)

High/Low strength Cfs:	 1996.6 (52) /   1707.2 (123)
Total strength (ave):	185018.8  (1850.2)
*/

    sscanf (inbuf, "System Variables at end of Major-Cycle step #%d)", &step);

    Step[index] = step;

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* skip 2 lines */
    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    sscanf (inbuf, "Num. Candidate-classifiers: %d; %d Cfs. won (%d",
	    &CandCf[index], &CfWon[index], &CfPost[index]);

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* skip 2 lines */
    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    sscanf (inbuf, "Num. candidate matches: %d", &CandMsg[index]);
    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    sscanf (inbuf, "Number of messages: %d", &PostMsg[index]);

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* skip 1 line */

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    sscanf (inbuf, "High/Low strength Cfs: %f (%d) / %f (%d)",
	    &f1, &i, &f2, &i);
    HiStr[index] = f1;
    LoStr[index] = f2;

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    sscanf (inbuf, "Total strength (ave): %f (%f)", &f1, &f2);
    AveStr[index] = f2;

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    if (strncmp (inbuf, "Average BidRatio", 16) == 0) {
       /* printf( "\nBR line '%s'", inbuf ); */
	for (cp = inbuf; *cp != ':' && *cp != '\0'; ++cp);
	++cp;
	for (; *cp == ' ' && *cp != '\0'; ++cp);
       /* printf( "...value is '%s'", cp ); */
	f1 = atof (cp);
       /* printf( "\n atof -> %f", f1 ); */
	cp = GetFloat (cp, &AveBR[index], -1.0, "", &ret);
       /* printf( "\n GetFloat -> AveBR[%d] %f", index, AveBR[index] ); */
    } else
	AveBR[index] = 0;

    TCandCf[index] += CandCf[index];
    TCfWon[index] += CfWon[index];
    TCfPost[index] += CfPost[index];
    TCandMsg[index] += CandMsg[index];
    TPostMsg[index] += PostMsg[index];
    THiStr[index] += HiStr[index];
    TLoStr[index] += LoStr[index];
    TAveStr[index] += AveStr[index];
    TAveBR[index] += AveBR[index];

    ++index;

    if (NumFilC == 0)
	++NmStored;

}					/* StoreVar */


/*************************

******/

VOID 
PrntFull (Plain)
    short Plain;
{
    int i;

    if (!Plain)
	printf ("\nStep  | HiStr | LoStr | AvStr | CndCf | CfWon | CfPst | CndMs | PstMs | AveBR");

    for (i = 0; i < NmStored && i < NMDISSZ; ++i) {
	if (i % 10 == 0 && !Plain)
	    printf ("\n------|-------|-------|-------|-------|-------|-------|-------|-------|------");
	if (!Plain)
	    printf ("\n%5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %5d | %4.2f",
		    Step[i], HiStr[i], LoStr[i], AveStr[i], CandCf[i], CfWon[i], CfPost[i], CandMsg[i], PostMsg[i], AveBR[i]);
	else
	    printf ("\n%5d %5d %5d %5d %5d %5d %5d %5d %5d",
		    Step[i], HiStr[i], LoStr[i], AveStr[i], CandCf[i], CfWon[i], CfPost[i], CandMsg[i], PostMsg[i], AveBR[i]);
    }

}					/* PrntFull */

/*************************

******/

VOID 
PrntTFull (Plain)
    short Plain;
{
    int i;

    if (!Plain) {
	printf ("\nAverages for %d files:\n", NumFilC);
	printf ("\nStep  | HiStr | LoStr | AvStr | CndCf | CfWon | CfPst | CndMs | PstMs | AveBR");
    }
    for (i = 0; i < NmStored && i < NMDISSZ; ++i) {
	if (i % 10 == 0 && !Plain)
	    printf ("\n------|-------|-------|-------|-------|-------|-------|-------|-------|------");
	if (!Plain)
	    printf ("\n%5d | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %5.0f | %4.2f",
		    Step[i], THiStr[i], TLoStr[i], TAveStr[i], TCandCf[i],
		    TCfWon[i], TCfPost[i], TCandMsg[i], TPostMsg[i], TAveBR[i]);
	else
	    printf ("\n%5d %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f",
		    Step[i], THiStr[i], TLoStr[i], TAveStr[i], TCandCf[i],
		    TCfWon[i], TCfPost[i], TCandMsg[i], TPostMsg[i], TAveBR[i]);
    }

}					/* PrntTFull */

/*************************

******/

VOID 
PrntVar (Type)
    int Type;
{

    for (index = 0; index < NmStored; ++index) {
	printf ("\n%5d", Step[index]);
	if (Type == 1)
	    printf (" %5d", CandCf[index]);
	else if (Type == 2)
	    printf (" %5d", CfWon[index]);
	else if (Type == 3)
	    printf (" %5d", CfPost[index]);
	else if (Type == 4)
	    printf (" %5d", CandMsg[index]);
	else if (Type == 5)
	    printf (" %5d", PostMsg[index]);
	else if (Type == 6)
	    printf (" %5d", HiStr[index]);
	else if (Type == 7)
	    printf (" %5d", LoStr[index]);
	else if (Type == 8)
	    printf (" %5d", AveStr[index]);
	else if (Type == 9)
	    printf (" %4.2f", AveBR[index]);
    }

}					/* PrntVar */

/*************************

******/

VOID 
Help ()
{

    printf ("\nGETVARS extracts data from a log file produced by CFS-C program:");
    printf ("\nThe log must have had DISPLAY VARS auto-displayed periodically.");
    printf ("\nProgram should be invoked by:");
    printf ("\n  getvars  logfile p h f cfc cfw cfp msgc msgp hs ls as br");
    printf ("\nwhere:");
    printf ("\n  logfile   is the name of a file containing the log you");
    printf ("\n			want to analyze.It may also be of the form:");
    printf ("\n  log.lg,n  where n is an integer 1..10, in which case data will");
    printf ("\n			extracted from the files log.lg0, log.lg1,...,log.lg(n-1)");
    printf ("\n			and it will be averaged over those files.");

    printf ("\nAll other parameters are optional. For each one included, a");
    printf ("\ntable of information is produced.  Each table includes a column");
    printf ("\nthat contains the CycleStep for each display, and one or more");
    printf ("\ncolumns for the variables extracted. The parameters:");
    printf ("\n");
    printf ("\n  p		 Include to produce 'plain' output, i.e.,");
    printf ("\n			without titles, bars, and lines (for input to some grapher).");
    printf ("\n");

    printf ("\n[press any key to see next screen]");
    getch ();

    printf ("\n  h		 Include to echo all lines read until it encounters");
    printf ("\n			a line with the string:");
    printf ("\n				; END-HEADER");
    printf ("\n			This provides easy way to get runtime parameter settings");
    printf ("\n			and pre-run comments include with display of strengths.");
    printf ("\n");
    printf ("\n  f		 Extract information as if all the rest of the parameters");
    printf ("\n			had been entered, and list in one multiple column table.");
    printf ("\n");
    printf ("\n  cfc	   Number of candidate classifiers");
    printf ("\n  cfw	   Number of classifiers that won.");
    printf ("\n  cfp	   Number of classifier that *posted* messages.");
    printf ("\n");
    printf ("\n  msgc	  Number of candidate messages.");
    printf ("\n  msgp	  Number of messages posted.");
    printf ("\n");
    printf ("\n  hs		Highest strength.");
    printf ("\n  ls		Lowest strength.");
    printf ("\n  as		Average strength.");
    printf ("\n");
    printf ("\n  br		Average bidratio.");

}					/* Help  */


/**
strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .
**/

int 
strcmpn (Str1, Str2, N)
    char Str1[], Str2[];
    int N;
{
    int ret, i;

    for (ret = 1, i = 0; Str1[i] == Str2[i] && i < N; ++i);

    if (i == N)
	ret = 0;

    return (ret);

}					/* strcmpn */
