
/*
GETVC: A program to extract payoff rates from logs of FSW experiements,
produced by the DISPLAY ENV,1 command.
(This can also extract visit counts, but use GETVC1 to do that.)

See Help() for a further description.

Notes:
1. Some limits:
   STATESZ  max number of states that can be counted.
   COUNTSZ  max number of data points (display of visit counts)

2. There are lots of parameters, but it turns out I never use them
   so I plan to clean them out and clean this program up *real soon now*.

Use LARGE model in the 80x86 world to compile this so
you can collect lots of data points, e.g. use
  mkmscul  getvc1
(Link this to cfsio.obj and utility.obj from the cfsc package.)
*/

#include "config.h"
#include "utility.h"
#include "cfsio.ext"
#include "getvc.h"

unsigned int URndSd;			/* we need this to include UTILITY.C at link time. */
short int EchoFlg;

#ifdef THIS				/* inlcude if linked to cfsutil  */
#include "core.ext"
unsigned int NmCfs;
struct CfNode *HiStrCf, *StrSrtCf;
float TotCfStr, AveCfStr, TPBiasStr, TRBiasStr;	/* Define to resolve externs in */
#endif

#define   STATESZ	  32
#define   COUNTSZ	 256
#define   ENDINT	   -1

int VCounts[STATESZ][COUNTSZ];
int TVCounts[STATESZ][COUNTSZ];
int stalist[STATESZ];
long unsigned int stplst[COUNTSZ];
float TotSRew[COUNTSZ];
int stpndx, standx;

long unsigned int EqStart = 0;
int EqCnt = 0;
float EqTRew = 0;
float EqTRVar = 0;

float TTotSRew[COUNTSZ];
long unsigned int TEqStart = 0;
int TEqCnt = 0;
float TEqTRew = 0;
float TEqTRVar = 0;

float EqTRews[15];			/* for calculating sd of EqTRew values across files */
float AEqTRews, VEqTRews;

int Divisor = 1;

short AllIdFlg;

int NmVCSto = 0;
int NmStaIds = 0;

int NumFilC = 0;			/* number of files from which data was collected */
int numfiles, filenum;			/* number of files from command, and file we are working on */

short margflg = FALSE;
short sumflg = FALSE;

char inbuf[200];
FILE *InFptr;

VOID StoreVC (), PrntVC (), PrntTVC (), PrntTR (), PrntTTR (), Help ();
char *GetInt ();


/*****************************

*********/

VOID 
main (argc, argv)
    int argc;
    char *argv[];
{
    int ret, len, err, i, id;
    short idparflg, plainflg;
    short trewflg, tonlyflg;
    char *cptr, *idptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2 ();
    float f;

    if (argc == 1) {
	Help ();
	exit (1);
    }
    AllIdFlg = TRUE;
    idparflg = FALSE;
    plainflg = FALSE;
    trewflg = tonlyflg = sumflg = FALSE;

    path[0] = '\0';

    for (standx = 0; standx < STATESZ; ++standx) {
	stalist[standx] = -1;
	for (stpndx = 0; stpndx < COUNTSZ; ++stpndx)
	    VCounts[standx][stpndx] = TVCounts[standx][stpndx] = 0;
    }

    for (stpndx = 0; stpndx < COUNTSZ; ++stpndx)
	TTotSRew[stpndx] = TotSRew[stpndx] = 0;
    TEqStart = TEqCnt = 0;
    TEqTRew = TEqTRVar = 0;

    NmVCSto = NmStaIds = 0;

    for (i = 2; i < argc; ++i) {
	if (strcmp (argv[i], "d") == 0) {
	    GetInt (argv[i], &Divisor, -1, ",", &err);
	    if (err || Divisor == -1) {
		printf ("\nIllegal Divisor in run parameter.\n");
		exit (1);
	    }
	} else if (strcmp (argv[i], "m") == 0)
	    margflg = TRUE;
	else if (strcmp (argv[i], "p") == 0)
	    plainflg = TRUE;
	else if (strcmp (argv[i], "tr") == 0)
	    trewflg = TRUE;
	else if (strcmp (argv[i], "to") == 0)
	    tonlyflg = TRUE;		/* Print total over n files only */
	else if (strcmp (argv[i], "so") == 0)
	    sumflg = TRUE;		/* Print summary only--average and sd in equil */
	else if (*argv[i] == 'i') {
	    idptr = argv[i];
	    ++idptr;
	    idparflg = TRUE;
	} else {
	    printf ("\nIllegal parameter '%s', ignored.", argv[i]);
	    printf ("\nFor help, enter getvc \n");
	}
    }

    if (trewflg & (plainflg || margflg))
	plainflg = margflg = TRUE;	/* these are same for total rewards */

   /* lets get the basic filename and perhaps a number of files */
   /* could be  filename  or  filename,3  or some other digit */
    cptr = StpTok2 (argv[1], buff, sizeof (buff), " ,");
    if (*cptr == '\0') {
	strcpy (basefnam, buff);
	numfiles = 1;
    } else {
	strcpy (basefnam, buff);
	if (sscanf (cptr, "%d", &numfiles) != 1) {
	    printf ("\n\nIllegal first parameter (%s)--enter GETVC for help.\n\n", argv[1]);
	    exit (1);
	} else if (numfiles < 1 || numfiles > 10) {
	    printf ("\n\nIllegal number of files on first parameter (%s)--enter GETVC for help.\n\n",
		    argv[1]);
	    exit (1);
	}
    }

    if (idparflg) {
	AllIdFlg = FALSE;
	for (standx = 0; standx < STATESZ; ++standx) {
	    idptr = GetInt (idptr, &id, -1, ",", &err);
	    if (err) {
		printf ("\nIllegal State Id in run parameter.\n");
		exit (1);
	    } else if (id == -1)
		break;
	    else {
		stalist[standx] = id;
		++NmStaIds;
	    }
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
		printf ("For help, enter GETVC .\n\n");
		exit (1);
	    }
	    continue;
	}
	if (!plainflg) {
	    printf ("\nExtracting CFS-C data from from the file ");

#if LATTICEC
	    getcwd (path, sizeof (path));
	    printf ("%s\\", path);
#endif

	    printf ("%s ...\n", fname);
	}
	stpndx = 0;

	while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF)
	    if (strcmpn (inbuf, "; TEST", 6) == 0 ||
		    strcmpn (inbuf, "C-? ; TEST", 10) == 0) {
		while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF)
		    if (strcmpn (inbuf, "; ENDTEST", 9) == 0 ||
			    strcmpn (inbuf, "C-? ; ENDTEST", 13) == 0)
			break;
	    } else if (strcmpn (inbuf, "State", 5) == 0 &&
		       strcmpn (inbuf, "CurState  EffVal", 16) != 0)
		StoreVC ();

	if (!tonlyflg) {
	    if (trewflg) {
		PrntTR (plainflg);
		EqTRews[NumFilC] = EqTRew;
	    } else
		PrntVC (plainflg);
	}
	if (fclose (InFptr) != 0)
	    printf ("\n\nERR: couldn't close log file '%s'!!\n", fname);

	++NumFilC;

    }					/* end of one file  */

    if (NumFilC > 1) {
	if (trewflg) {
	    PrntTTR (plainflg);
	    if (!plainflg && NumFilC > 1) {
		VEqTRews = AEqTRews = 0;
		for (filenum = 0; filenum < NumFilC; ++filenum)
		    AEqTRews += EqTRews[filenum];
		AEqTRews /= NumFilC;
		for (filenum = 0; filenum < NumFilC; ++filenum) {
		    f = AEqTRews - EqTRews[filenum];
		    VEqTRews += f * f;
		}
		VEqTRews /= NumFilC - 1;
		VEqTRews = sqrt (VEqTRews);
		printf ("\n\nAverage of totals: %6.0f (SD %.1f).\n", AEqTRews, VEqTRews);
	    }
	} else
	    PrntTVC (plainflg);
    }
}					/* main */


/*****************************

*********/

VOID 
StoreVC ()
{
    int ret, len, id, count, err, x, i;
    long unsigned int step;
    float f, totrew;
    char *cp;

    sscanf (inbuf, "State %d (r %f, tr %f) [Dft %d, Invld %d, r %d] CycleStp %ld.",
	    &x, &f, &totrew, &x, &x, &x, &step);
    stplst[stpndx] = step;

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    cp = &inbuf[12];

    if (stpndx == 0) {
	for (i = 0; i < STATESZ; ++i) {
	    cp = GetInt (cp, &count, -1, " ", &err);
	    if (count == -1 || err)
		break;
	    else {
		if (AllIdFlg) {
		    stalist[i] = i;
		    ++NmStaIds;
		    VCounts[i][stpndx] = count;
		    TVCounts[i][stpndx] += count;
		} else if ((id = IntInLst (i, stalist, NmStaIds)) >= 0) {
		    VCounts[id][stpndx] = count;
		    TVCounts[id][stpndx] += count;
		}
	    }
	}
    } else {
	for (i = 0; i < STATESZ; ++i) {
	    cp = GetInt (cp, &count, -1, " ", &err);
	    if (count == -1 || err)
		break;
	    else if ((id = IntInLst (i, stalist, NmStaIds)) >= 0) {
		VCounts[id][stpndx] = count;
		TVCounts[id][stpndx] += count;
	    }
	}
    }

    TotSRew[stpndx] = totrew;
    TTotSRew[stpndx] += totrew;

    ++stpndx;
    if (filenum == 0)
	++NmVCSto;

}					/* StoreVC */


/*****************************

*********/

VOID 
PrntVC (PlainFlg)
    short PlainFlg;
{
    int linecnt, i;

    if (!PlainFlg) {
	printf ("\n\nVisit Counts for FSW states:");
	printf ("\n\n	   States ->\n Step ");
	for (i = 0; i < NmStaIds; ++i)
	    printf ("%3d  ", stalist[i]);
	printf ("\n-----");
	for (i = 0; i < NmStaIds; ++i)
	    printf ("|----");
	printf ("\n");
    }
    if (!margflg) {
	printf ("%5ld", stplst[0]);
	for (standx = 0; standx < NmStaIds; ++standx)
	    if (PlainFlg)
		printf (" %4d", VCounts[standx][0]);
	    else
		printf ("|%4d\n", VCounts[standx][0]);
    }
    EqStart = stplst[NmVCSto - 1];

    for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	printf ("%5ld", stplst[stpndx]);

	for (standx = 0; standx < NmStaIds; ++standx) {
	    if (margflg)
		i = VCounts[standx][stpndx] - VCounts[standx][stpndx - 1];
	    else
		i = VCounts[standx][stpndx];

	    if (PlainFlg)
		printf (" %4d", i);
	    else
		printf ("|%4d", i);
	}

	if (++linecnt % 4 == 0 && !PlainFlg) {
	    printf ("\n-----");
	    for (i = 0; i < NmStaIds; ++i)
		printf ("|----");
	}
	printf ("\n");
    }

}					/* PrntVC  */


/*****************************

*********/

VOID 
PrntTVC (PlainFlg)
    short PlainFlg;
{
    int linecnt, i;

    if (!PlainFlg) {
	printf ("\n\nAverage Visit Counts for FSW states:");
	printf ("\n\n	   States ->\n Step ");
	for (i = 0; i < NmStaIds; ++i)
	    printf ("%3d  ", stalist[i]);
	printf ("\n-----");
	for (i = 0; i < NmStaIds; ++i)
	    printf ("|----");
	printf ("\n");
    }
    TVCounts[standx][0] /= NumFilC;

    if (!margflg) {
	printf ("%5ld", stplst[stpndx]);
	for (standx = 0; standx < NmStaIds; ++standx)
	    if (PlainFlg)
		printf (" %4d\n", TVCounts[standx][0]);
	    else
		printf ("|%4d\n", VCounts[standx][0]);
    }
    EqStart = stplst[NmVCSto - 1];

    for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	printf ("%5ld", stplst[stpndx]);

	for (standx = 0; standx < NmStaIds; ++standx) {
	    TVCounts[standx][stpndx] /= NumFilC;
	    if (margflg)
		i = TVCounts[standx][stpndx] - TVCounts[standx][stpndx - 1];
	    else
		i = TVCounts[standx][stpndx];

	    if (PlainFlg)
		printf (" %4d", i);
	    else
		printf ("|%4d", i);
	}

	if (++linecnt % 4 == 0 && !PlainFlg) {
	    printf ("\n-----");
	    for (i = 0; i < NmStaIds; ++i)
		printf ("|----");
	}
	printf ("\n");
    }

}					/* PrntTVC  */


/*****************************

*********/

VOID 
PrntTR (PlainFlg)
    short PlainFlg;
{
    int linecnt, i;
    float f, mrate, trate;

    if (!margflg && !sumflg) {
	printf ("\n\nTotal Reward for FSW1:\n");
	printf ("\n Step	TotRew	 MargRew\n");
	printf ("\n------|----------|-----------\n");
    }
    if (!sumflg)
	printf ("%5ld", stplst[stpndx]);
    if (!margflg && !sumflg)
	printf (" |  %6.0f  |	  **", TotSRew[0]);
    else if (!sumflg)
	printf (" %6.0f", TotSRew[0]);
    printf ("\n");

    EqStart = stplst[NmVCSto - 1] * 0.75;
    EqTRew = EqCnt = 0;

    if (!sumflg)
	for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	    printf ("%5ld", stplst[stpndx]);

	    f = TotSRew[stpndx] - TotSRew[stpndx - 1];

	    if (stplst[stpndx] >= EqStart) {
		EqTRew += f;
		++EqCnt;
	    }
	    if (margflg)
		printf (" %6.1f", f);
	    else
		printf (" |  %6.0f  |  %6.0f", TotSRew[stpndx], f);

	    if (++linecnt % 4 == 0 && !margflg && !sumflg)
		printf ("\n------|----------|-----------");

	    printf ("\n");
    } else				/* sumflg is on */
	for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {

	    f = TotSRew[stpndx] - TotSRew[stpndx - 1];

	    if (stplst[stpndx] >= EqStart) {
		EqTRew += f;
		++EqCnt;
	    }
	}

    if (!PlainFlg) {
	if (EqCnt != 0) {
	    EqTRew /= EqCnt;
	    printf ("\n\nAve Marg Reward in last 1/4 of run: %6.1f", EqTRew);

	    for (EqTRVar = 0.0, stpndx = 1; stpndx < NmVCSto; ++stpndx) {
		if (stplst[stpndx] >= EqStart) {
		    mrate = TotSRew[stpndx] - TotSRew[stpndx - 1];
		    trate = mrate - EqTRew;
		    EqTRVar += trate * trate;
		}
	    }
	    EqTRVar /= EqCnt - 1;
	    trate = sqrt (EqTRVar);
	    printf ("\nSD of Ave Marg reward:			   %6.1f", trate);
	}
    }
}					/* PrntTR  */


/*****************************

*********/

VOID 
PrntTTR (PlainFlg)
    short PlainFlg;
{
    int linecnt, i;
    float f, mrate, trate;

    TTotSRew[0] /= NumFilC;
    printf ("%5ld", stplst[stpndx]);
    printf (" %6.0f", TTotSRew[0]);
    printf ("\n");

    TEqStart = stplst[NmVCSto - 1] * 0.75;

    for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	printf ("%5ld", stplst[stpndx]);

	TTotSRew[stpndx] /= NumFilC;
	f = TTotSRew[stpndx] - TTotSRew[stpndx - 1];

	if (stplst[stpndx] >= TEqStart) {
	    TEqTRew += f;
	    ++TEqCnt;
	}
	printf (" %6.1f", f);
	printf ("\n");
    }

    if (!PlainFlg) {
	if (TEqCnt != 0) {
	    TEqTRew /= TEqCnt;
	    printf ("\n\nAve Marg Reward in last 1/4 of run: %6.1f", TEqTRew);

	    for (TEqTRVar = 0.0, stpndx = 1; stpndx < NmVCSto; ++stpndx) {
		if (stplst[stpndx] >= TEqStart) {
		    mrate = TTotSRew[stpndx] - TTotSRew[stpndx - 1];
		    trate = mrate - TEqTRew;
		    TEqTRVar += trate * trate;
		}
	    }
	    TEqTRVar /= TEqCnt - 1;
	    trate = sqrt (TEqTRVar);
	    printf ("\nSD of Ave Marg reward:			   %6.1f", trate);
	}
    }
}					/* PrntTTR  */


/*
strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .
*/

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


/*
IntInLst	Check for int in list (array) of ints.

	Test	Int to look for.

	List	Array of int's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise
*/

int 
IntInLst (Test, List, Max)
    int Test, List[], Max;
{
    int ret, i;

    for (i = 0; i <= Max; ++i)
	if (Test == List[i])
	    break;

    if (i <= Max)
	ret = i;
    else
	ret = -1;

    return (ret);

}					/* IntInLst */


/*****************************

*********/

VOID 
Help ()
{

    printf ("\nGETVC extracts payoffs (performance) data from FSW1 logs.");
    printf ("\nThe logs must include displays of the environment produced");
    printf ("\nby the DISPLAY ENV,1 command (or the equivalent autodisplay of environment).");
    printf ("\n\nTo get state visit counts and rates from the log, use GETVC1.");
    printf ("\n");
    printf ("\nUsage:");
    printf ("\n   getvc  filename.lg1  m  tr");
    printf ("\n		Collect data from file filename.lg1 .");
    printf ("\n		'm' menas display marginal values (change since last display)");
    printf ("\n		'tr' means display the total reward received by system.");
    printf ("\n");
    printf ("\n	getvc filename,3   m  tr");
    printf ("\n		as before, but collect data from files filename.lg0,");
    printf ("\n		filename.lg1, and filename.lg2 (the '3' could be 1..10).");
    printf ("\n		Data is displayed from each file and then an average over all");
    printf ("\n		the files is also calculated.");
    printf ("\n");
    printf ("\n[press any key to continue...]");
    getch ();
    printf ("\nNotes:");
    printf ("\n1. Include a ' p ' after the 'm' paramater to display things in a 'plain'");
    printf ("\n   format, i.e., with no labels or lines.  This is useful to feed the data");
    printf ("\n   to a plotting program.");
    printf ("\n2. Some limits (which could be compiled into larger values:");
    printf ("\n   Number of states that can be counted: %d", STATESZ);
    printf ("\n   Number of display points per run: %d", COUNTSZ);


}					/* Help */
