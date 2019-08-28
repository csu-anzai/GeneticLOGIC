
/*
Program to extract "visit counts" (performance) from logs of FSW1 experiements.

See Help() for details.

Notes:
1. Some limits:
   STATESZ  max number of states that can be counted.
   COUNTSZ  max number of data points (display of visit counts)
   NUMFILES max number of files that can be averaged.

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
#include "getvc1.h"

unsigned int URndSd;			/* we need this to include UTILITY.C at link time. */
short int EchoFlg;

#ifndef STATESZ
#define   STATESZ	 32
#endif

#ifndef COUNTSZ
#define   COUNTSZ	 95
#endif

#define   NUMFILES	 10
#define   ENDINT	   -1

#define   LONG		long

unsigned int CurState[COUNTSZ];
unsigned int VCounts[STATESZ][COUNTSZ], *AllocUIA ();	/* funct to allocate space */
float TVCounts[STATESZ][COUNTSZ], *AllocFA ();
int stalist[STATESZ];
LONG unsigned int stplst[COUNTSZ];
int stpndx, standx;

short bflg = FALSE;
short bsflg = FALSE;
unsigned int BCounts[STATESZ][COUNTSZ];
float TBCounts[STATESZ][COUNTSZ];
float EqBFr[STATESZ], EqBSFr;
float TEqBFr[STATESZ], TEqBSFr;		/* total equil fraction visit best next, for states and over all states */

float EqSFVB[NUMFILES];			/* fraction visit best states (ave over all) next, as printed for each logfile */
float EqSFVBMean, EqSFVBVar;		/* meand and var of EqSFVB */

float EqFrac = 0.75;			/* fraction of end of run in "Equilibrium" */
LONG unsigned int EqStart = 0;
int EqCnt = 0;
float EqTRew = 0;
float EqTRVar = 0;

LONG unsigned int TEqStart = 0;
int TEqCnt = 0;
float TEqTRew = 0;
float TEqTRVar = 0;

float EqTRews[STATESZ];			/* for calculating sd of EqTRew values across files */
float AEqTRews, VEqTRews;

int Divisor = 1;

int NmVCSto = 0;
int NmStaIds = 0;

int NumFilC = 0;			/* number of files from which data was collected */
int numfiles, filenum;			/* number of files from command, and file we are working on */

short margflg = FALSE;
short sumflg = FALSE;

char inbuf[1024];
FILE *InFptr;

VOID StoreVC (), PrntVC (), PrntTVC (), PrntBst (), PrntTBst (), Help ();
char *GetInt ();


/**********************



*********/

VOID 
main (argc, argv)
    int argc;
    char *argv[];
{
    int ret, len, err, i, id;
    short idparflg, plainflg;
    short tonlyflg;
    char *cptr, *idptr, path[64], basefnam[64], fname[64], buff[64], *StpTok2 ();
    float f;

    if (argc == 1) {
	Help ();
	exit (1);
    }
    idparflg = FALSE;
    plainflg = FALSE;
    tonlyflg = sumflg = FALSE;

    path[0] = '\0';

#ifdef THIS				/* Use when allocate these from the heap, to allow larger sizes */
    VCounts = AllocUIA ((COUNTSZ * STATESZ));
    TVCounts = AllocFA ((COUNTSZ * STATESZ));
#endif

    for (standx = 0; standx < STATESZ; ++standx) {
	stalist[standx] = -1;
	EqBFr[standx] = 0;
	TEqBFr[standx] = 0;
	for (stpndx = 0; stpndx < COUNTSZ; ++stpndx)
	    VCounts[standx][stpndx] = TVCounts[standx][stpndx] = 0;
    }

    for (i = 0; i < NUMFILES; ++i)
	EqSFVB[i] = 0;
    EqSFVBMean = EqSFVBVar = 0;

    TEqBSFr = 0;
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
	else if (strcmp (argv[i], "to") == 0)
	    tonlyflg = TRUE;		/* Print total over n files only */
	else if (strcmp (argv[i], "so") == 0)
	    sumflg = TRUE;		/* Print summary only--average and sd in equil */
	else if (strcmp (argv[i], "b") == 0)
	    bflg = TRUE;
	else if (strcmp (argv[i], "bso") == 0)
	    bsflg = bflg = TRUE;
	else if (*argv[i] == 'i') {
	    idptr = argv[i];
	    ++idptr;
	    idparflg = TRUE;
	} else if (strcmpn (argv[i], "e", 1) == 0) {
	    cptr = argv[i];
	    ++cptr;
	    GetFloat (cptr, &EqFrac, -1.0, ",", &err);
	    EqFrac /= 100.0;
	    if (err || EqFrac == -1.0 || EqFrac < 0.75 || EqFrac >= 1.0) {
		printf ("\nIllegal EqFrac in run parameter.\n");
		exit (1);
	    }
	} else {
	    printf ("\nIllegal parameter '%s', ignored.", argv[i]);
	    printf ("\nFor help, enter getvc1\n");
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
	    printf ("\n\nIllegal first parameter (%s)--enter getvc1 for help.\n\n", argv[1]);
	    exit (1);
	} else if (numfiles < 1 || numfiles > 10) {
	    printf ("\n\nIllegal number of files on first parameter (%s)--enter getvc1 for help.\n\n",
		    argv[1]);
	    exit (1);
	}
    }

    if (idparflg) {
	if (*idptr == 'a') {
	    ++idptr;
	    idptr = GetInt (idptr, &id, -1, ",", &err);
	    if (id < 0 || id > STATESZ) {
		printf ("\nToo many or few states in ia<s> parameter.");
		exit (1);
	    } else {
		for (standx = 0; standx < id; ++standx)
		    stalist[standx] = standx;
		NmStaIds = id;
	    }
	}
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
    if (NmStaIds == 0) {
	printf ("\nYou must specify a list or count of states to collect.\n");
	exit (1);
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
		printf ("For help, enter getvc1  \n");
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
	for (standx = 0; standx < STATESZ; ++standx) {
	    EqBFr[standx] = 0;
	    for (stpndx = 0; stpndx < COUNTSZ; ++stpndx)
		VCounts[standx][stpndx] = 0;
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
	    if (bflg)
		PrntBst (plainflg);
	    else
		PrntVC (plainflg);
	}
	if (fclose (InFptr) != 0)
	    printf ("\n\nERR: couldn't close log file '%s'!!\n", fname);

	++NumFilC;

    }					/* end of one file  */

    if (NumFilC > 1) {
	if (bflg)
	    PrntTBst (plainflg);
	else
	    PrntTVC (plainflg);
    }
}					/* main */


/**********************



*********/

VOID 
StoreVC ()
{
    LONG unsigned int step;
    int ret, len, id, count, err, x, i;
    float f, totrew;
    char *cp;

    sscanf (inbuf, "State %d (r %f, tr %f) [Dft %d, Invld %d, r %d] CycleStp %ld.",
	    &id, &f, &totrew, &x, &x, &x, &step);
    stplst[stpndx] = step;
    CurState[stpndx] = id;

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);
    cp = &inbuf[12];

    if (stpndx == 0) {			/* establish state list */
	for (i = 0; i < STATESZ; ++i) {
	    cp = GetInt (cp, &count, -1, " ", &err);
	    if (count == -1 || err)
		break;
	    else {
		if ((id = IntInLst (i, stalist, NmStaIds)) >= 0) {
		    VCounts[id][stpndx] = count;
		    TVCounts[id][stpndx] += count;
		   /* *** NOTE: Correct here for Totals and cur state */
		    if (bflg && CurState[stpndx] == i)
			TVCounts[id][stpndx] -= 1;
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
	       /* *** NOTE: Correct here for Totals and cur state */
		if (bflg && CurState[stpndx] == i)
		    TVCounts[id][stpndx] -= 1;
	    }
	}
    }

    ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* get Best line */
    i = 0;
    while (strcmpn (inbuf, "Best:", 5) == 0) {
	cp = &inbuf[5];
	for (; i < STATESZ; ++i) {
	    cp = GetInt (cp, &id, -1, ",", &err);	/* the state id */
	    if (id == -1 || err)
		break;
	    else {
		if ((x = IntInLst (id, stalist, NmStaIds)) < 0) {
		    for (; *cp != ' ' && *cp != ':' && *cp != '\0'; ++cp)	/* skip to next state quad */
			;
		} else {
		    cp = GetInt (cp, &count, -1, ",", &err);
		    if (count != -1 && !err) {
			BCounts[x][stpndx] = count;
			TBCounts[x][stpndx] += count;
		    } else
			printf ("\nStoreVC: err reading best count: stp %d", step);
		    for (; *cp != ',' && *cp != '\0'; ++cp)	/* skip vcount */
			;
		    for (; *cp != ' ' && *cp != '\0'; ++cp)	/* skip fraction */
			;
		}
		for (; *cp == ' ' && *cp != '\0'; ++cp)	/* skip blanks */
		    ;
		if (*cp == ':')		/* at end of states list */
		    break;
	    }
	}				/* counts on one line */
	ret = ReadS (inbuf, sizeof (inbuf), InFptr, &len);	/* get Best line */
    }					/* more best lines */

    ++stpndx;
    if (filenum == 0)
	++NmVCSto;

}					/* StoreVC */


/**********************



*********/

VOID 
PrntVC (PlainFlg)
    short PlainFlg;
{
    unsigned int linecnt, i;

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
		printf ("|%4d", VCounts[standx][0]);
	printf ("\n");
    }
    EqStart = stplst[NmVCSto - 1] * EqFrac;

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


/**********************



*********/

VOID 
PrntTVC (PlainFlg)
    short PlainFlg;
{
    unsigned int linecnt, i;

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
    EqStart = stplst[NmVCSto - 1] * EqFrac;

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


/**********************



*********/

VOID 
PrntBst (PlainFlg)
    short PlainFlg;
{
    LONG unsigned int totvc, totbest;
    int linecnt, i, j;
    float f;

    if (!PlainFlg && !sumflg) {
	printf ("\n\nBest Counts for FSW states:");
	if (!bsflg) {
	    printf ("\n\n	   States ->\n Step ");
	    for (i = 0; i < NmStaIds; ++i)
		printf (" %3d  ", stalist[i]);
	    printf ("\n------");
	    for (i = 0; i < NmStaIds; ++i)
		printf ("|-----");
	} else
	    printf ("\n Step   Sum \n------|-----");
	printf ("\n");
    }
    if (!margflg && !sumflg) {
	printf ("%5ld ", stplst[0]);
	totvc = totbest = 0;
	for (standx = 0; standx < NmStaIds; ++standx) {
	    if (CurState[0] == stalist[standx])
		j = VCounts[standx][0] - 1;
	    else
		j = VCounts[standx][0];
	    if (j != 0) {
		totbest += BCounts[standx][0];
		totvc += j;
		f = BCounts[standx][0] / j;
		if (!bsflg) {
		    if (PlainFlg)
			printf (" %.3f", f);
		    else
			printf ("|%.3f", f);
		}
	    } else if (!bsflg) {
		if (PlainFlg)
		    printf ("	  ");
		else
		    printf ("|	 ");
	    }
	}
	if (totvc != 0) {
	    f = 1.0 * totbest / totvc;
	    if (PlainFlg && bsflg)
		printf (" %0.3f", f);
	    else if (!PlainFlg)
		printf ("  %ld/%ld = %0.3f", totbest, totvc, f);
	} else if (!PlainFlg)
	    printf ("  %ld/%ld = --", totbest, totvc);
	printf ("\n");
    }
    EqCnt = 0;
    EqStart = stplst[NmVCSto - 1] * EqFrac;

    EqBSFr = 0;
    for (standx = 0; standx < NmStaIds; ++standx)
	EqBFr[standx] = 0;

    for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	if (!sumflg)
	    printf ("%5ld ", stplst[stpndx]);
	for (standx = 0, totvc = 0, totbest = 0; standx < NmStaIds; ++standx) {
	    if (CurState[stpndx] == stalist[standx])
		j = VCounts[standx][stpndx] - 1;
	    else
		j = VCounts[standx][stpndx];

	    if (margflg) {
		j = j - VCounts[standx][stpndx - 1];
		if (CurState[stpndx - 1] == stalist[standx])
		    ++j;		/* correct for prev state count too high */
	    }
	    if (j != 0) {
		if (margflg)
		    i = BCounts[standx][stpndx] - BCounts[standx][stpndx - 1];
		else
		    i = BCounts[standx][stpndx];

		f = i * 1.0 / j;
		totbest += i;
		totvc += j;

		if (!bsflg && !sumflg) {
		    if (PlainFlg)
			printf (" %.3f", f);
		    else
			printf ("|%.3f", f);
		}
	    } else if (!bsflg && !sumflg) {
		if (PlainFlg)
		    printf ("	  ");
		else
		    printf ("|	 ");
	    }
	    if (stplst[stpndx] >= EqStart)
		EqBFr[standx] += f;
	}

	if (totvc != 0) {
	    f = 1.0 * totbest / totvc;
	    if (PlainFlg && bsflg && !sumflg)
		printf ("  %0.3f", f);
	    else if (!PlainFlg && !sumflg)
		printf ("  %ld/%ld = %0.3f", totbest, totvc, f);
	    if (stplst[stpndx] >= EqStart)
		EqBSFr += f;
	} else if (!PlainFlg && !sumflg)
	    printf ("  Tot: %ld/%ld = --", totbest, totvc);

	if (++linecnt % 4 == 0 && !PlainFlg && !sumflg) {
	    printf ("\n------");
	    if (!bsflg)
		for (i = 0; i < NmStaIds; ++i)
		    printf ("|-----");
	    else
		printf ("|-----");
	}
	if (!sumflg)
	    printf ("\n");

	if (stplst[stpndx] >= EqStart)
	    ++EqCnt;
    }

    if (!PlainFlg) {
	printf ("\nEq:  ");
	if (EqCnt != 0) {
	    for (standx = 0; standx < NmStaIds; ++standx)
		if (standx % 21 == 20)
		    printf ("\n %5.3f", EqBFr[standx] / EqCnt);
		else
		    printf (" %5.3f", EqBFr[standx] / EqCnt);
	    printf ("			 %5.3f\n", EqBSFr / EqCnt);
	    EqSFVB[NumFilC] = EqBSFr / EqCnt;
	    EqSFVBMean += EqSFVB[NumFilC];
	}
    }
}					/* PrntBst  */

/**********************



*********/

VOID 
PrntTBst (PlainFlg)
    short PlainFlg;
{
    LONG unsigned int totvc, totbest;
    int linecnt, i, j;
    float f;

    if (sumflg)
	printf ("\n\nAverages for runs:\n");

    else if (!PlainFlg) {
	printf ("\n\nTotal Best Counts for FSW states:");
	if (!bsflg) {
	    printf ("\n\n	   States ->\n Step ");
	    for (i = 0; i < NmStaIds; ++i)
		printf (" %3d  ", stalist[i]);
	    printf ("\n------");
	    for (i = 0; i < NmStaIds; ++i)
		printf ("|-----");
	} else
	    printf ("\n Step   Sum \n------|-----");
	printf ("\n");
    }
    if (!margflg && !sumflg) {
	printf ("%5ld ", stplst[0]);
	totvc = totbest = 0;
	for (standx = 0; standx < NmStaIds; ++standx) {
	    TBCounts[standx][0] /= NumFilC;
	    TVCounts[standx][0] /= NumFilC;
	    j = TVCounts[standx][0];
	    if (j != 0) {
		totbest += TBCounts[standx][0];
		totvc += j;
		f = TBCounts[standx][0] / j;
		if (!bsflg) {
		    if (PlainFlg)
			printf (" %.3f", f);
		    else
			printf ("|%.3f", f);
		}
	    } else if (!bsflg) {
		if (PlainFlg)
		    printf ("	  ");
		else
		    printf ("|	 ");
	    }
	}
	if (totvc != 0) {
	    f = 1.0 * totbest / totvc;
	    if (PlainFlg && bsflg)
		printf (" %0.3f", f);
	    else if (!PlainFlg)
		printf ("  %ld/%ld = %0.3f", totbest, totvc, f);
	} else if (!PlainFlg)
	    printf ("  %ld/%ld = --", totbest, totvc);
	printf ("\n");
    }
    EqStart = stplst[NmVCSto - 1] * EqFrac;
    EqCnt = 0;
    TEqBSFr = 0;
    for (standx = 0; standx < NmStaIds; ++standx)
	TEqBFr[standx] = 0;

    for (linecnt = stpndx = 1; stpndx < NmVCSto; ++stpndx) {
	if (!sumflg)
	    printf ("%5ld ", stplst[stpndx]);
	for (standx = 0, totvc = 0, totbest = 0; standx < NmStaIds; ++standx) {

	    if (margflg)
		j = TVCounts[standx][stpndx] - TVCounts[standx][stpndx - 1];
	    else
		j = TVCounts[standx][stpndx];

	    if (j != 0) {
		if (margflg)
		    i = TBCounts[standx][stpndx] - TBCounts[standx][stpndx - 1];
		else
		    i = TBCounts[standx][stpndx];

		f = i * 1.0 / j;
		totbest += i;
		totvc += j;

		if (!bsflg && !sumflg) {
		    if (PlainFlg)
			printf (" %.3f", f);
		    else
			printf ("|%.3f", f);
		}
	    } else if (!bsflg && !sumflg) {
		if (PlainFlg)
		    printf ("	  ");
		else
		    printf ("|	 ");
	    }
	    if (stplst[stpndx] >= EqStart)
		TEqBFr[standx] += f;
	}

	if (totvc != 0) {
	    f = 1.0 * totbest / totvc;
	    if (PlainFlg && bsflg && !sumflg)
		printf ("  %0.3f", f);
	    else if (!PlainFlg && !sumflg)
		printf ("  %ld/%ld = %0.3f", totbest / NumFilC, totvc / NumFilC, f);
	    if (stplst[stpndx] >= EqStart)
		TEqBSFr += f;
	} else if (!PlainFlg && !sumflg)
	    printf ("  Tot: %ld/%ld = --", totbest / NumFilC, totvc / NumFilC);

	if (++linecnt % 4 == 0 && !PlainFlg && !sumflg) {
	    printf ("\n------");
	    if (!bsflg)
		for (i = 0; i < NmStaIds; ++i)
		    printf ("|-----");
	    else
		printf ("|-----");
	}
	if (!sumflg)
	    printf ("\n");
	if (stplst[stpndx] >= EqStart)
	    ++EqCnt;
    }

    if (!PlainFlg) {
	printf ("\nEq:   ");
	if (EqCnt != 0) {
	    for (standx = 0; standx < NmStaIds; ++standx)
		if (standx % 21 == 20)
		    printf ("\n %5.3f", TEqBFr[standx] / EqCnt);
		else
		    printf (" %5.3f", TEqBFr[standx] / EqCnt);
	    printf ("			%5.3f\n", TEqBSFr / EqCnt);

	    EqSFVBMean /= NumFilC * 1.0;
	    for (i = 0; i < NumFilC; ++i) {
		f = EqSFVB[i] - EqSFVBMean;
		EqSFVBVar += f * f;
	    }
	    EqSFVBVar = sqrt (EqSFVBVar / (NumFilC - 1.0));
	    printf ("Mean (SD):  %5.3f (%5.3f)\n", EqSFVBMean, EqSFVBVar);
	}
    }
}					/* PrntTBst  */

/**/
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

/**********************



*********/

VOID 
Help ()
{

    printf ("\nGETVC1 extracts 'visit count' (performance) data from FSW1 logs.");
    printf ("\nThe logs must include displays of the environment produced");
    printf ("\nby the DISPLAY ENV,1 command (or the equivalent autodisplay of environment).");
    printf ("\nUse GETVC to extract payoffs totals and rates from the log.");
    printf ("\n");
    printf ("\nUsage:");
    printf ("\n   getvc1  filename.lg0  m  bso  ia12");
    printf ("\n		Collect data from file filename.lg1 .");
    printf ("\n		'm' menas display marginal values (change since last display)");
    printf ("\n		'bso' means display the fraction of times system visited");
    printf ("\n		the 'next best state' from any of the states 0..11.");
    printf ("\n		Thus for this to work the fsw1/cfsc startup commands must");
    printf ("\n		must specify 'next best states' (see the fsw1 documentation).");
    printf ("\n		for the states 0..11 ('ia12' means collect from those 12 states.");
    printf ("\n");
    printf ("\n	getvc1 filename,3   m  bso ia12");
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
    printf ("\n   Number of files that can be averaged: %d", NUMFILES);

}					/* Help */

unsigned int *
AllocUIA (Size)
    unsigned int Size;
{
    unsigned int *ret;
    extern char *calloc ();

    if ((ret = (unsigned int *) calloc (Size, sizeof (unsigned int))) == NULL) {
	printf ("\nERR (AllocUIA): calloc returned NULL!");
	exit (1);
    }
    return (ret);

}

float *
AllocFA (Size)
    unsigned int Size;
{
    float *ret;
    extern char *calloc ();

    if ((ret = (float *) calloc (Size, sizeof (float))) == NULL) {
	printf ("\nERR (AllocFA): calloc returned NULL!");
	exit (1);
    }
    return (ret);

}
