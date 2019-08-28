
/*

Program to extract performance data from log produced by CFS-C program.
In paricular, the log must include displays of the environemnt
produced by the DISPLAY ENV,2 command.

*/

#include "config.h"

#include "utility.h"
#include "cfsio.ext"
#include "getperf.h"

short EchoFlg;

#define   NMGUESZ	 200		/* max num of times performance displayed */
#define   ENDINT	   -1		/* end-of-data mark */

unsigned int guestplst[NMGUESZ];	/* steps at which performance displayed */
unsigned int GsLTable[NMGUESZ], GsTTable[NMGUESZ],	/* performance values (totals ) found */
    GsWTable[NMGUESZ], GsDTable[NMGUESZ];
int gsindx;				/* index into performance */
int NmGsSto = 0;			/* number of samples stored. */

unsigned int TGsLTable[NMGUESZ], TGsTTable[NMGUESZ],	/* performance values (totals ) found */
    TGsWTable[NMGUESZ], TGsDTable[NMGUESZ];

float HiGsL = 0.0;			/* highest marginal performance */
float LoGsL = 0.0;
unsigned int HiGsStp = 1;		/* First step HiGsL was reached */
unsigned int LoGsStp = 1;

int EPercent;				/* Percent of end of run to be called 'equilibrium' period. */
unsigned int EStaStp, EEndStp;		/* equil start and end step */

int EGsCnt;				/* Number of marginal displays in equil. */

float EPerf = 0.0;			/* Mean Performance (%right) from marignals in Equilirium */
float EPerfT = 0.0;
float EPerfTs[10];
float EPVar = 0.0;			/* variance of performance (% right L) in equil */
float EPVarT = 0.0;

float EHiPerf = 0.0;			/* highest marginal performance */
float ELoPerf = 0.0;
unsigned int EHiStep = 1;		/* First step HiGsL was reached */
unsigned int ELoStep = 1;
float EHiPerfT = 0.0;
float ELoPerfT = 0.0;

unsigned int PCurve[11];
unsigned int PCurveT[11];

float CrtFrac = 0.90;			/* Criterium for good performance--fraction of 'equilibrium' */
int CrtLen = 100;			/* Min number of steps at CrtFrac to meet criteria */
int CrtStp = -1;			/* First step criterium met  -joke*/
unsigned int CrtStpT = 0;
unsigned int CrtGsCnt = 0;
float CrtPerf = 0.0;
float CrtPerfT = 0.0;
float CrtPVar = 0.0;
float CrtPVarT = 0.0;

float CrtHiPerf = 0.0;			/* highest marginal performance */
float CrtLoPerf = 0.0;
unsigned int CrtHiStep = 0;
unsigned int CrtLoStep = 0;
float CrtHiPT = 0.0;
float CrtLoPT = 0.0;

int NumFilC = 0;			/* number of files from which data was collected */
int numfiles, filenum;			/* number of files from command, and file we are working on */

char inbuf[200];
FILE *InFptr;

VOID StoreGue (), PrntGue (), PrntTGs (), PrntPerf (), PrntTPerf (), Help ();
char *GetInt ();

short GuessFlg, PlainFlg, PerfFlg;
/**/

#if !MACLSC
VOID 
main (argc, argv)
#else
VOID 
_main (argc, argv)
#endif

    int argc;
    char *argv[];
{
    int ret, len, err, i, drivecd;
    short headflg, mprflg, tprflg, mptflg, tptflg, mpdflg, tpdflg;
    float efrac;
    char *cptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2 ();

    if (argc == 1) {
	Help ();
	exit (1);
    }
    drivecd = 0;
    path[0] = '\0';

    for (gsindx = 0; gsindx < NMGUESZ; ++gsindx)
	TGsLTable[gsindx] = TGsTTable[gsindx] = TGsWTable[gsindx] = TGsDTable[gsindx] = 0;

    for (i = 0; i <= 10; ++i)
	PCurveT[i] = 0;
    for (i = 0; i <= 9; ++i)
	EPerfTs[i] = 0;

    EPercent = 25;
    PlainFlg = GuessFlg = headflg = FALSE;
    PerfFlg = TRUE;
    mprflg = tprflg = mptflg = tptflg = mpdflg = tpdflg = FALSE;

   /* get the paramters and set the flags */

    for (i = 2; i < argc; ++i) {
	if (strcmp (argv[i], "p") == 0)
	    PlainFlg = TRUE;
	else if (strcmp (argv[i], "h") == 0)
	    headflg = TRUE;
	else if (strcmp (argv[i], "g") == 0)
	    GuessFlg = TRUE;
	else if (strcmp (argv[i], "mpr") == 0)	/* marg % right */
	    mprflg = TRUE;
	else if (strcmp (argv[i], "tpr") == 0)	/* total % right */
	    tprflg = TRUE;
	else if (strcmp (argv[i], "mpt") == 0)	/* marg % type */
	    mptflg = TRUE;
	else if (strcmp (argv[i], "tpt") == 0)	/* total % type */
	    tptflg = TRUE;
	else if (strcmp (argv[i], "mpd") == 0)	/* marg % default */
	    mpdflg = TRUE;
	else if (strcmp (argv[i], "tpd") == 0)	/* total % default */
	    tpdflg = TRUE;
	else if (*argv[i] == 'e') {
	    cptr = argv[i];
	    ++cptr;
	    cptr = GetInt (cptr, &EPercent, -1, " ", &err);
	    if (err || EPercent < 1 || EPercent > 100) {
		printf ("\nValue after 'e' must be integer from 1 to 100. Using 25.");
		EPercent = 25;
	    }
	} else if (strcmpn (argv[i], "cf", 2) == 0) {
	    cptr = argv[i];
	    ++cptr;
	    ++cptr;
	    cptr = GetInt (cptr, &ret, -1, " ", &err);
	    if (err || ret < 1 || ret > 100) {
		printf ("\nValue after 'cf' must be integer from 1 to 100. Using 90.");
		CrtFrac = 0.90;
	    } else
		CrtFrac = ret / 100.0;
	} else if (strcmpn (argv[i], "cl", 2) == 0) {
	    cptr = argv[i];
	    ++cptr;
	    ++cptr;
	    cptr = GetInt (cptr, &CrtLen, -1, " ", &err);
	    if (err || CrtLen < 1) {
		printf ("\nValue after 'cl' must be integer > 1. Using 100.");
		CrtFrac = 100;
	    }
	} else if (strcmp (argv[i], "help") == 0) {
	    Help ();
	    exit (1);
	} else {
	    printf ("\nIllegal parameter '%s', ignored.", argv[i]);
	    printf ("\nFor help, enter:   getstr help\n");
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
	    printf ("\n\nIllegal first parameter (%s)--enter getperf for help.\n\n", argv[1]);
	    exit (1);
	} else if (numfiles < 1 || numfiles > 9) {
	    printf ("\n\nIllegal number of files on first parameter (%s)--enter getperf for help.\n\n",
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
		printf ("For help, enter getperf");
		exit (1);
	    }
	    continue;
	}
	if (!PlainFlg) {
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
	for (gsindx = 0; gsindx < NMGUESZ; ++gsindx)
	    GsLTable[gsindx] = GsTTable[gsindx] = GsWTable[gsindx] = GsDTable[gsindx] = 0;
	gsindx = 0;

	while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF)
	    if (strcmpn (inbuf, "; TEST", 6) == 0 ||
		    strcmpn (inbuf, "C-? ; TEST", 10) == 0) {
		while ((ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len)) != EOF)
		    if (strcmpn (inbuf, "; ENDTEST", 9) == 0 ||
			    strcmpn (inbuf, "C-? ; ENDTEST", 13) == 0)
			break;
	    } else if (strcmpn (inbuf, "TotNmGsL", 8) == 0)
		StoreGue ();

	efrac = 1.0 - (EPercent / 100.0);
	EStaStp = Round (EEndStp * efrac);

	if (GuessFlg || PerfFlg) {
	    if (!PlainFlg)
		printf ("\n\nLETSEQ Performance (total and recent counts)\n");
	    PrntGue ();
	}
	if (mprflg)
	    PrntPerf (1);

	else if (tprflg)
	    PrntPerf (2);

	else if (mptflg)
	    PrntPerf (3);

	else if (tptflg)
	    PrntPerf (4);

	else if (mpdflg)
	    PrntPerf (5);

	else if (tpdflg)
	    PrntPerf (6);

	if (fclose (InFptr) != 0)
	    printf ("\n\nERR: couldn't close log file '%s'!!\n", fname);

	EPerfTs[NumFilC] = EPerf;

	++NumFilC;

    }					/* end of one file  */

   /* Now print the total strength and performance tables */

    if (NumFilC > 1) {
	for (gsindx = 1; gsindx < NmGsSto; ++gsindx) {
	    TGsLTable[gsindx] /= NumFilC;
	    TGsTTable[gsindx] /= NumFilC;
	    TGsWTable[gsindx] /= NumFilC;
	    TGsDTable[gsindx] /= NumFilC;
	}
	if (GuessFlg || PerfFlg)
	    PrntTGs ();
	if (mprflg)
	    PrntTPerf (1);
    }
}					/* main */


/*********************



*****/

VOID 
StoreGue ()
{
    unsigned int TotNmGsL, TotNmGsT, TotNmGsW, step, NmDftGue;

    sscanf (inbuf, "TotNmGsL %u, TotNmGsT %u, TotNmGsW %u (CycleStp %u) [TotNmDft %u]",
	    &TotNmGsL, &TotNmGsT, &TotNmGsW, &step, &NmDftGue);

    if (filenum == 0) {
	++NmGsSto;
	EEndStp = step;
    }
    guestplst[gsindx] = step;

    GsLTable[gsindx] = TotNmGsL;
    GsTTable[gsindx] = TotNmGsT;
    GsWTable[gsindx] = TotNmGsW;
    if (NmDftGue > 0)
	GsDTable[gsindx] = NmDftGue;

    TGsLTable[gsindx] += TotNmGsL;
    TGsTTable[gsindx] += TotNmGsT;
    TGsWTable[gsindx] += TotNmGsW;
    if (NmDftGue > 0)
	TGsDTable[gsindx] += NmDftGue;

    ++gsindx;

}					/* StoreGue */


/*********************



*****/

VOID 
PrntGue ()
{
    int linecnt, prfi, prfi10;
    float tfloat, trate, mrate;

    EGsCnt = 0;
    EPerf = 0.0;
    EHiPerf = CrtHiPerf = 0.0;
    ELoPerf = CrtLoPerf = 100.0;

    for (prfi = 1; prfi <= 10; ++prfi)
	PCurve[prfi] = -1;

    if (guestplst[0] != 0) {
	if ((1.0 * GsLTable[0] / guestplst[0]) > HiGsL) {
	    HiGsL = 1.0 * GsLTable[0] / guestplst[0];
	    HiGsStp = guestplst[0];
	}
	trate = 100.0 * GsLTable[0] / guestplst[0];
	mrate = 100.0 * GsLTable[0] / guestplst[0];
    } else {
	HiGsL = 0;
	HiGsStp = 0;
	trate = mrate = -1;
    }

    if (!PlainFlg && GuessFlg) {
	printf ("\n Step | TotGsL,dif | TotGsT,dif | TotGsW,dif | %Cum RL, Dif | DftGue,dif");
	printf ("\n------|------------|------------|------------|--------------|-----------");
	printf ("\n%5u | %5u %4d | %5u %4d | %5u %4d |  %5.1f %5.1f | %5u %4d", guestplst[0],
		GsLTable[0], GsLTable[0], GsTTable[0], GsTTable[0], GsWTable[0], GsWTable[0], trate, mrate,
		GsDTable[0], GsDTable[0]);
    } else if (GuessFlg)
	printf ("\n%5u %6u %4d %6u %4d %6u %4d %4.2f %4.2f %6u %4d", guestplst[0],
		GsLTable[0], GsLTable[0], GsTTable[0], GsTTable[0], GsWTable[0], GsWTable[0], trate, mrate,
		GsDTable[0], GsDTable[0]);

    for (linecnt = gsindx = 1; gsindx < NmGsSto; ++gsindx) {
	trate = mrate = -1;
	if (guestplst[gsindx] != 0)
	    trate = 100.0 * GsLTable[gsindx] / guestplst[gsindx];
	if ((guestplst[gsindx] - guestplst[gsindx - 1]) != 0)
	    mrate = 100.0 * (GsLTable[gsindx] - GsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
	if (!PlainFlg && GuessFlg)
	    printf ("\n%5u | %5u %4d | %5u %4d | %5u %4d |  %5.1f %5.1f | %5u %4d", guestplst[gsindx],
		    GsLTable[gsindx], GsLTable[gsindx] - GsLTable[gsindx - 1],
		    GsTTable[gsindx], GsTTable[gsindx] - GsTTable[gsindx - 1],
		    GsWTable[gsindx], GsWTable[gsindx] - GsWTable[gsindx - 1], trate, mrate,
		    GsDTable[gsindx], GsDTable[gsindx] - GsDTable[gsindx - 1]);
	else if (GuessFlg)
	    printf ("\n%5u %6u %4d %6u %4d %6u %4d %5.1f %5.1f %6u %4d", guestplst[gsindx],
		    GsLTable[gsindx], GsLTable[gsindx] - GsLTable[gsindx - 1],
		    GsTTable[gsindx], GsTTable[gsindx] - GsTTable[gsindx - 1],
		    GsWTable[gsindx], GsWTable[gsindx] - GsWTable[gsindx - 1], trate, mrate,
		    GsDTable[gsindx], GsDTable[gsindx] - GsDTable[gsindx - 1]);

	if (++linecnt % 10 == 8 && !PlainFlg && GuessFlg)
	    printf ("\n------|------------|------------|------------|--------------|-----------");

       /* check for highest marginal performance */

	if (mrate > HiGsL) {
	    HiGsL = mrate;
	    HiGsStp = guestplst[gsindx];
	}
       /* see if mrate is first time to some perf. curve milestone */

	for (prfi = 1; prfi <= 10; ++prfi) {
	    if (mrate < prfi * 10)	/* mrate below this (and higher) milestones */
		break;
	    else if (PCurve[prfi] != -1)/* not -1, so we hit this earlier */
		continue;
	    else			/* first time to this milestone */
		PCurve[prfi] = guestplst[gsindx];
	}

	if (guestplst[gsindx] >= EStaStp) {
	    ++EGsCnt;			/* increment count of marginals in equil. */
	    EPerf += mrate;		/* increment total of marginals */
	    if (mrate > EHiPerf) {
		EHiPerf = mrate;
		EHiStep = guestplst[gsindx];
	    }
	    if (mrate <= ELoPerf) {
		ELoPerf = mrate;
		ELoStep = guestplst[gsindx];
	    }
	}
    }

   /* Store Perf Curve into totals array for perf curves of several runs */

    for (prfi = 1; prfi <= 10; ++prfi) {
	if (PCurve[prfi] != -1)
	    PCurveT[prfi] += PCurve[prfi];
    }

    if (!PlainFlg) {
	printf ("\n");
	printf ("\nOverall performance rate:  ");
	--gsindx;
	if (EEndStp != 0 && gsindx >= 0 && guestplst[gsindx] != 0) {
	    tfloat = 100.0 * GsLTable[gsindx] / guestplst[gsindx];
	    printf ("%5.1f", tfloat);
	} else
	    printf ("-- (EndStep was 0!)");

	for (prfi = 1; prfi <= 10; prfi += 2) {
	    prfi10 = prfi * 10;
	    if (PCurve[prfi] != -1 && PCurve[prfi + 1] != -1)
		printf ("\nSteps to %2u,%3u%%  performance:   %3u, %3u",
			prfi10, prfi10 + 10, PCurve[prfi], PCurve[prfi + 1]);
	    else if (PCurve[prfi] != -1)
		printf ("\nSteps to %2u,%3u%%  performance:   %3u, ---",
			prfi10, prfi10 + 10, PCurve[prfi]);
	    else if (PCurve[prfi + 1] != -1)	/* should never happen! */
		printf ("\nSteps to %2u,%3u%%  performance:   ---, %3u",
			prfi10, prfi10 + 10, PCurve[prfi + 1]);
	    else
		printf ("\nSteps to %2u,%3u%%  performance:   ---, ---",
			prfi10, prfi10 + 10);
	}

	printf ("\nMean Perf. in Equilibrium:   ");
	if (EGsCnt != 0) {
	    EPerf /= EGsCnt;
	    EPerfT += EPerf;
	    printf ("%5.1f	   (starting at step %d)", EPerf, EStaStp);

	    for (EPVar = 0.0, gsindx = 1; gsindx < NmGsSto; ++gsindx) {
		if (guestplst[gsindx] >= EStaStp) {
		    mrate = 100.0 * (GsLTable[gsindx] - GsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
		    trate = mrate - EPerf;
		    EPVar += trate * trate;
		}
	    }
	    EPVar /= EGsCnt - 1;
	    EPVarT += EPVar;
	    EHiPerfT += EHiPerf;
	    ELoPerfT += ELoPerf;
	    trate = sqrt (EPVar);
	    printf ("\nSD of Mean Equil Perf:	   %5.2f", trate);
	    printf ("\nHigh Perf. in Equil (Step):  %5.1f (%u)", EHiPerf, EHiStep);
	    printf ("\nLow  Perf. in Equil (Step):  %5.1f (%u)", ELoPerf, ELoStep);
	} else
	    printf ("\n-- (EndStep - StartStep is 0 for equilibrium range!");

	printf ("\n");
    } else {				/* Plainflg is on! */
	--gsindx;
	if (EEndStp != 0 && gsindx >= 0 && guestplst[gsindx] != 0) {
	    tfloat = 100.0 * GsLTable[gsindx] / guestplst[gsindx];
	    printf ("\n%5.1f", tfloat);
	} else
	    printf ("--");

	for (prfi = 1; prfi <= 10; ++prfi) {
	    if (PCurve[prfi] != -1)
		printf (" %u", PCurve[prfi]);
	    else
		printf (" --");
	}

	if (EGsCnt != 0) {
	    EPerf /= EGsCnt;
	    EPerfT += EPerf;
	    printf ("  %5.1f (%u)", EPerf, EStaStp);

	    for (EPVar = 0.0, gsindx = 1; gsindx < NmGsSto; ++gsindx) {
		if (guestplst[gsindx] >= EStaStp) {
		    mrate = 100.0 * (GsLTable[gsindx] - GsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
		    trate = mrate - EPerf;
		    EPVar += trate * trate;
		}
	    }
	    EPVar /= EGsCnt - 1;
	    EPVarT += EPVar;
	    EHiPerfT += EHiPerf;
	    ELoPerfT += ELoPerf;
	    trate = sqrt (EPVar);
	    printf ("  %5.2f", trate);
	    printf ("  %5.1f (%u)", EHiPerf, EHiStep);
	    printf ("  %5.1f (%u)", ELoPerf, ELoStep);
	}
	printf ("\n");
    }

}					/* PrntGue */


/*********************



*****/

VOID 
PrntTGs ()
{
    int linecnt, prfi, prfi10;
    float tfloat, trate, mrate;

    if (!PlainFlg)
	printf ("\n\nAverages for %d files: ", NumFilC);

    EGsCnt = 0;
    EPerf = 0.0;
    EHiPerf = CrtHiPerf = 0.0;
    ELoPerf = CrtLoPerf = 100.0;

    for (prfi = 1; prfi <= 10; ++prfi)
	PCurve[prfi] = -1;

    if (guestplst[0] != 0) {
	if ((1.0 * TGsLTable[0] / guestplst[0]) > HiGsL) {
	    HiGsL = 1.0 * TGsLTable[0] / guestplst[0];
	    HiGsStp = guestplst[0];
	}
	trate = 100.0 * TGsLTable[0] / guestplst[0];
	mrate = 100.0 * TGsLTable[0] / guestplst[0];
    } else {
	HiGsL = 0;
	HiGsStp = 0;
	trate = mrate = -1;
    }

    if (!PlainFlg && GuessFlg) {
	printf ("\n Step | TotGsL,dif | TotGsT,dif | TotGsW,dif | %Cum RL, Dif | DftGue,dif");
	printf ("\n------|------------|------------|------------|--------------|-----------");
	printf ("\n%5u | %5u %4d | %5u %4d | %5u %4d |  %5.1f %5.1f | %5u %4d", guestplst[0],
		TGsLTable[0], TGsLTable[0], TGsTTable[0], TGsTTable[0], TGsWTable[0], TGsWTable[0], trate, mrate,
		TGsDTable[0], TGsDTable[0]);
    } else if (GuessFlg)
	printf ("\n%5u %6u %4d %6u %4d %6u %4d %4.2f %4.2f %6u %4d", guestplst[0],
		TGsLTable[0], TGsLTable[0], TGsTTable[0], TGsTTable[0], TGsWTable[0], TGsWTable[0], trate, mrate,
		TGsDTable[0], TGsDTable[0]);

    for (linecnt = gsindx = 1; gsindx < NmGsSto; ++gsindx) {
	trate = mrate = -1;
	if (guestplst[gsindx] != 0)
	    trate = 100.0 * TGsLTable[gsindx] / guestplst[gsindx];
	if ((guestplst[gsindx] - guestplst[gsindx - 1]) != 0)
	    mrate = 100.0 * (TGsLTable[gsindx] - TGsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
	if (!PlainFlg && GuessFlg)
	    printf ("\n%5u | %5u %4d | %5u %4d | %5u %4d |  %5.1f %5.1f | %5u %4d", guestplst[gsindx],
		    TGsLTable[gsindx], TGsLTable[gsindx] - TGsLTable[gsindx - 1],
		    TGsTTable[gsindx], TGsTTable[gsindx] - TGsTTable[gsindx - 1],
		    TGsWTable[gsindx], TGsWTable[gsindx] - TGsWTable[gsindx - 1], trate, mrate,
		    TGsDTable[gsindx], TGsDTable[gsindx] - TGsDTable[gsindx - 1]);
	else if (GuessFlg)
	    printf ("\n%5u %6u %4d %6u %4d %6u %4d %5.1f %5.1f %6u %4d", guestplst[gsindx],
		    TGsLTable[gsindx], TGsLTable[gsindx] - TGsLTable[gsindx - 1],
		    TGsTTable[gsindx], TGsTTable[gsindx] - TGsTTable[gsindx - 1],
		    TGsWTable[gsindx], TGsWTable[gsindx] - TGsWTable[gsindx - 1], trate, mrate,
		    TGsDTable[gsindx], TGsDTable[gsindx] - TGsDTable[gsindx - 1]);

	if (++linecnt % 10 == 8 && !PlainFlg && GuessFlg)
	    printf ("\n------|------------|------------|------------|--------------|-----------");

       /* check for highest marginal performance */

	if (mrate > HiGsL) {
	    HiGsL = mrate;
	    HiGsStp = guestplst[gsindx];
	}
       /* see if mrate is first time to some perf. curve milestone */

	for (prfi = 1; prfi <= 10; ++prfi) {
	    if (mrate < prfi * 10)	/* mrate below this (and higher) milestones */
		break;
	    else if (PCurve[prfi] != -1)/* not -1, so we hit this earlier */
		continue;
	    else			/* first time to this milestone */
		PCurve[prfi] = guestplst[gsindx];
	}

	if (guestplst[gsindx] >= EStaStp) {
	    ++EGsCnt;			/* increment count of marginals in equil. */
	    EPerf += mrate;		/* increment total of marginals */
	    if (mrate > EHiPerf) {
		EHiPerf = mrate;
		EHiStep = guestplst[gsindx];
	    }
	    if (mrate <= ELoPerf) {
		ELoPerf = mrate;
		ELoStep = guestplst[gsindx];
	    }
	}
    }

   /* Store Perf Curve into totals array for perf curves of several runs */

    for (prfi = 1; prfi <= 10; ++prfi) {
	if (PCurve[prfi] != -1)
	    PCurveT[prfi] += PCurve[prfi];
    }

    if (!PlainFlg) {
	printf ("\n");
	printf ("\nOverall performance rate:  ");
	--gsindx;
	if (EEndStp != 0 && gsindx >= 0 && guestplst[gsindx] != 0) {
	    tfloat = 100.0 * TGsLTable[gsindx] / guestplst[gsindx];
	    printf ("%5.1f", tfloat);
	} else
	    printf ("-- (EndStep was 0!)");

	for (prfi = 1; prfi <= 10; prfi += 2) {
	    prfi10 = prfi * 10;
	    if (PCurve[prfi] != -1 && PCurve[prfi + 1] != -1)
		printf ("\nSteps to %2u,%3u%%  performance:   %3u, %3u",
			prfi10, prfi10 + 10, PCurve[prfi], PCurve[prfi + 1]);
	    else if (PCurve[prfi] != -1)
		printf ("\nSteps to %2u,%3u%%  performance:   %3u, ---",
			prfi10, prfi10 + 10, PCurve[prfi]);
	    else if (PCurve[prfi + 1] != -1)	/* should never happen! */
		printf ("\nSteps to %2u,%3u%%  performance:   ---, %3u",
			prfi10, prfi10 + 10, PCurve[prfi + 1]);
	    else
		printf ("\nSteps to %2u,%3u%%  performance:   ---, ---",
			prfi10, prfi10 + 10);
	}

	printf ("\nMean Perf. in Equilibrium:   ");
	if (EGsCnt != 0) {
	    EPerf /= EGsCnt;
	    EPerfT += EPerf;
	    printf ("%5.1f	   (starting at step %d)", EPerf, EStaStp);

	    for (EPVar = 0.0, gsindx = 1; gsindx < NmGsSto; ++gsindx) {
		if (guestplst[gsindx] >= EStaStp) {
		    mrate = 100.0 * (TGsLTable[gsindx] - TGsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
		    trate = mrate - EPerf;
		    EPVar += trate * trate;
		}
	    }
	    EPVar /= EGsCnt - 1;
	    EPVarT += EPVar;
	    EHiPerfT += EHiPerf;
	    ELoPerfT += ELoPerf;
	    trate = sqrt (EPVar);
	    printf ("\nSD of Mean Equil Perf:	   %5.2f", trate);
	    printf ("\nHigh Perf. in Equil (Step):  %5.1f (%u)", EHiPerf, EHiStep);
	    printf ("\nLow  Perf. in Equil (Step):  %5.1f (%u)", ELoPerf, ELoStep);
	} else
	    printf ("\n-- (EndStep - StartStep is 0 for equilibrium range!");

	printf ("\n");
    } else {				/* Plainflg is on! */
	--gsindx;
	if (EEndStp != 0 && gsindx >= 0 && guestplst[gsindx] != 0) {
	    tfloat = 100.0 * TGsLTable[gsindx] / guestplst[gsindx];
	    printf ("\n%5.1f", tfloat);
	} else
	    printf ("--");

	for (prfi = 1; prfi <= 10; ++prfi) {
	    if (PCurve[prfi] != -1)
		printf (" %u", PCurve[prfi]);
	    else
		printf (" --");
	}

	if (EGsCnt != 0) {
	    EPerf /= EGsCnt;
	    EPerfT += EPerf;
	    printf ("  %5.1f (%u)", EPerf, EStaStp);

	    for (EPVar = 0.0, gsindx = 1; gsindx < NmGsSto; ++gsindx) {
		if (guestplst[gsindx] >= EStaStp) {
		    mrate = 100.0 * (TGsLTable[gsindx] - TGsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
		    trate = mrate - EPerf;
		    EPVar += trate * trate;
		}
	    }
	    EPVar /= EGsCnt - 1;
	    EPVarT += EPVar;
	    EHiPerfT += EHiPerf;
	    ELoPerfT += ELoPerf;
	    trate = sqrt (EPVar);
	    printf ("  %5.2f", trate);
	    printf ("  %5.1f (%u)", EHiPerf, EHiStep);
	    printf ("  %5.1f (%u)", ELoPerf, ELoStep);
	}
	printf ("\n");
    }

}					/* PrntTGs */


/****************

strcmpn	 Compare leftmost N characters of two strings.

	Str1	Strings to compare (possibly NULL terminated).
	Str2
	N	   Number of characters to compare.

	Return: If leftmost N characters are the same, return 0
			else  return != 0 .

******/

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


/***********

IntInLst	Check for int in list (array) of ints.

	Test	Int to look for.

	List	Array of int's.

	Max	 Last entry to check.

	Return  N   if Test is N-th entry in List[0...Max].
			-1  otherwise

******/

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


/**************

PrntPerf	Print performance for SCI-GRAF.

	Which   Value	 What-to-print
			  1		 Marginal % correct letter.

******/

VOID 
PrntPerf (Which)
    int Which;
{
    float mrate;

    if (Which == 1) {
	if (guestplst[0] != 0)
	    mrate = 100.0 * (GsLTable[0] / guestplst[0]);
	else
	    mrate = -1;
    }
    printf ("\n%5u  %5.1f", guestplst[0], mrate);

    for (gsindx = 1; gsindx < NmGsSto; ++gsindx) {
	if (Which == 1) {
	    if ((guestplst[gsindx] - guestplst[gsindx - 1]) != 0)
		mrate = 100.0 * (GsLTable[gsindx] - GsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
	    else
		mrate = -1;
	}
	printf ("\n%5u %5.1f", guestplst[gsindx], mrate);
    }

}					/* PrntPerf  */



/**********************

PrntTPerf	Print total performance for SCI-GRAF.

	Which   Value	 What-to-print
			  1		 Marginal % correct letter.


**/

VOID 
PrntTPerf (Which)
    int Which;
{
    float mrate;

    if (Which == 1) {
	if (guestplst[0] != 0)
	    mrate = 100.0 * (TGsLTable[0] / guestplst[0]);
	else
	    mrate = -1;
    }
    printf ("\n%5u  %5.1f", guestplst[0], mrate);

    for (gsindx = 1; gsindx < NmGsSto; ++gsindx) {
	if (Which == 1) {
	    if ((guestplst[gsindx] - guestplst[gsindx - 1]) != 0)
		mrate = 100.0 * (TGsLTable[gsindx] - TGsLTable[gsindx - 1]) / (guestplst[gsindx] - guestplst[gsindx - 1]);
	    else
		mrate = -1;
	}
	printf ("\n%5u %5.1f", guestplst[gsindx], mrate);
    }

}					/* PrntTPerf  */


/*********************



*****/

VOID 
Help ()
{

    printf ("\nProgram to extract performance data from log produced by LETSEQ1/CFS-C.");
    printf ("\nIn paricular, the log must include displays of the environemnt ");
    printf ("\nproduced by the DISPLAY ENV,2 command.");
    printf ("\n");
    printf ("\nUsage:");
    printf ("\n   getperf  filename.lg0  g");
    printf ("\n		Collect data from file filename.lg1 .");
    printf ("\n");
    printf ("\n	getvc1 filename,3 g");
    printf ("\n		as before, but collect data from files filename.lg0,");
    printf ("\n		filename.lg1, and filename.lg2 (the '3' could be 1..10).");
    printf ("\n		Data is displayed from each file and then an average over all");
    printf ("\n		the files is also calculated.");
    printf ("\n");

}					/* end Help */
